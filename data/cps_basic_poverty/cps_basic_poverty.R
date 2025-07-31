library(epiextractr)

# annual chained CPI-U
cpi_u_annual <- haven::read_dta("./data/cps_basic_poverty/cpi_u.dta")

org_year_max = 2023

base_2019 = cpi_u_annual |>
  filter(year == 2019) |>
  pull(cpi_u)

# before CPI-U release
price_series_annual = cpi_u_annual |> 
  # add missing data
  add_row(year = org_year_max) |> 
  mutate(diff = (cpi_u/lag(cpi_u, 1)),
         cpi_u = 
           case_when(year == org_year_max & is.na(cpi_u) ~ lag(cpi_u, 1)*lag(diff, 1),
                     TRUE ~ cpi_u)) |> 
  # following is not necessary
  select(year, cpi_u)



# adjust Federal Poverty Levels using C-CPI-U
fpl_data <- cross_join(price_series_annual, read.csv("./data/cps_basic_poverty/fpl_threshold.csv")) |> 
  # FPL based on 2019
  #note: data can be reconstructed using 
  #       https://www2.census.gov/programs-surveys/cps/tables/time-series/historical-poverty-thresholds/thresh19.xlsx
  mutate(povguide = fpl * (cpi_u/base_2019)) |> 
  select(-fpl)



# Methodology for defining poverty in the CPS Basic
data_db <- load_org(2010:2023) |> 
  ## separate faminc bands into maximum values
  # 1. assign labels as indicator values
  mutate(faminc_det = as.character(haven::as_factor(faminc_det))) |> 
  # 2. use label to extract maximum income
  mutate(
    # 2a. extract max value from variable label
    faminc_max = case_when(
      # manually assign lowest max value
      str_detect(faminc_det, "Less than") ~ "4999",
      # manually assign highest max value (before "+")
      str_detect(faminc_det, "\\$150,000\\+") ~ "150000",
      # extract after hyphen and before parenthesis
      TRUE ~ str_extract(faminc_det, "(?<= - \\$)[^ (]+")),
    # 2b. remove comma and force to numeric
    faminc_max = as.numeric(str_remove(faminc_max, ","))) |>
  mutate(across(where(haven::is.labelled), zap_labels)) |> 
  # restrict due to schenrl definition change
  #filter(year >= 2013) |> 
  ## create inclusive family id
  #note: inclusive ~ breaking out non-trad living arrangements as separate families
  # 1. separate non-family household members into individual families
  mutate(famnum = case_when(
    # non-family members receive new unique family id
    famid == 0 ~ row_number()*10,
    # family members & related subfamily assigned as related family
    famid > 1 & famtype == 3 ~ 1,
    # all other members assigned as subfamily members
    famid > 0 ~ famid)) |> 
  # 2. create family size for inclusive family units 
  #     (non-family household members ~ individual families)
  mutate(famsize = n(), .by = c(hhid, year, month, famnum)) |> 
  # 4. indicator for non-traditional (mixed) living arrangements
  mutate(mixed_household = case_when(
    # secondary family/individual lives with primary family
    famtype %in% c(4, 5) & any(famtype == 1) ~ 1,
    # secondary family/individual lives with primary individual
    famtype %in% c(4, 5) & any(famtype == 2) ~ 2,
    TRUE ~ 0),
    .by = c(hhid, year, month)) |> 
  ## Federal Poverty Level (FPL) family type 
  #note: include information about children
  # 1. identify number of children per family
  mutate(child = if_else(famrel == 3 & age < 18, 1, 0)) |> 
  mutate(childsize = sum(child, na.rm = TRUE), .by = c(hhid, year, month, famnum)) |>
  # 2. assign family type as combination of total family size + # children
  #ex. four-person family with 2 children ~ 42
  mutate(fpl_famtype = case_when(famsize %in% 1:9 & childsize == 0 ~ famsize * 10,
                                 famsize %in% 2:8 & childsize == 1 ~ famsize * 10 + 1,
                                 famsize %in% 3:8 & childsize == 2 ~ famsize * 10 + 2,
                                 famsize %in% 4:8 & childsize == 3 ~ famsize * 10 + 3,
                                 famsize %in% 5:8 & childsize == 4 ~ famsize * 10 + 4,
                                 famsize %in% 6:8 & childsize == 5 ~ famsize * 10 + 5,
                                 famsize %in% 7:8 & childsize == 6 ~ famsize * 10 + 6,
                                 famsize == 8 & childsize == 7 ~ famsize * 10 + 7,
                                 famsize >= 9 & childsize %in% 0:8 ~ 90 + childsize,
                                 famsize > 9 & childsize > 8 ~ 98)) |> 
  # merge ASEC income ratios to adjust unrelated, non-primary family faminc
  #note: WIP, created external by Hilary Wething
  cross_join(haven::read_dta("./data/cps_basic_poverty/ASEC_inc_ratios.dta") |> select(-year), copy = TRUE) |> 
  # adjust family income for unrelated, non-primary family units
  #note: nonfam == individual, unrel == unrelated, sec == secondary
  mutate(max_faminc_max = max(faminc_max, na.rm = TRUE), .by = c(hhid, year, month)) |> 
  #note: primary ~ person or family primarily being interviewed
  mutate(i_maxinc = case_when(
    # living with primary family:
    # a) unrelated secondary family, ex: married couple + primary married couple
    famtype == 4 & mixed_household == 1 ~ max_faminc_max * rat_famunrel,
    
    # b) secondary individual, ex: roommate + primary married couple
    famtype == 5 & mixed_household == 1 ~ max_faminc_max * rat_famsec,
    
    # living with primary individual:
    # a) unrelated secondary family, ex: primary roommate + single parent
    #note: rat_nhhunrel ~ asec(nonfam) / asec(unrel)
    famtype == 4 & mixed_household == 2 ~ max_faminc_max * rat_nhhunrel,
    
    # b) secondary individual, ex: 2 or more roommates
    famtype == 5 & mixed_household == 2 ~ max_faminc_max * rat_nhhsec,
    # all primary or related persons do not have adjusted family income
    #TRUE ~ max(faminc_max),
    famtype %in% c(1:3) ~ max_faminc_max)) |> 
  # merge Federal Poverty Guidelines (FPL) by family type
  left_join(fpl_data, by = c("year", "fpl_famtype"), copy = TRUE, relationship = "many-to-many") |> 
  # 200% FPL indicator
  mutate(spov200 = case_when(
    # compare adjusted family income to 2x FPL
    i_maxinc < (povguide * 2.0) ~ 1,
    # adjust family of only children
    famsize == childsize ~ 1,
    # above FPL 200
    TRUE ~ 0)) |> 
  # focus/insecure population 
  mutate(insecure_pop = case_when(
    # 1. 16-64 enrolled person  
    age < 16 | age >= 65 ~ 0, schenrl == 1 ~ 0,
    # 2. family income below 200% FPL
    TRUE ~ spov200)) 

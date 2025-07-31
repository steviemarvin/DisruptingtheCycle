# additional functions
# functiont that reformats large dfs to smaller ones (for exporting)
df_reformat_fun <- function(data, values_col) {
  data |> 
    pivot_wider(id_cols = year,
                names_from = group_name,
                values_from = {{values_col}}) |> 
    rename_with(~gsub("wbhaa_", "", .), contains("wbhaa_"))
}


# load housing data
ddi <- read_ipums_ddi(here(housing_ddi))
housing_raw <- read_ipums_micro(ddi, verbose = FALSE)
names(housing_raw) <- tolower(names(housing_raw))


# clean housing data
housing_clean <- cleaning_fun(housing_raw)
  
housing_clean <- mutate(housing_clean, 
                        # cleaning housing vars removing NAs
                        hhincome = na_if(hhincome, 9999999),
                        hhincome = if_else(hhincome == 0, 1, hhincome),
                        rentgrs = na_if(rentgrs, 0),
                        owncost = na_if(owncost, 99999),
                        # creating owner and renter variables
                        owner = case_when(ownershp == 1 ~ 1,
                                          ownershp == 2 ~ 0,
                                          TRUE ~ NA),
                        renter = case_when(ownershp == 2 ~ 1,
                                           ownershp == 1 ~ 0,
                                           TRUE ~ NA),
                        # housing cost to hhincome ratios
                        rent_ratio = rentgrs / (hhincome / 12),
                        owncost_ratio = owncost / (hhincome / 12))

# getting year and serials for hh where household head has a child
housing_lowinc_fam <- filter(housing_clean,
                             u18 == 1 & poverty < 200 & poverty != 0,
                             related == 101) |> 
  select(year, serial)

# variables for housing summar
housing_summary <- housing_clean |> 
  inner_join(housing_lowinc_fam, by = c("year", "serial")) |> 
  summarize(across(hhwt | contains("ratio") | owner | renter | contains("wbhaa_") | contains("pov"), ~max(.x)),
            .by = c(year, serial)) |> 
  mutate(
    across(contains("ratio"), 
           ~case_when(.x >= 0.3 ~ 1,
                      is.na(.x) ~ NA,
                      TRUE ~ 0), 
           .names = "{.col}_burden"),
    across(contains("ratio") & !contains("burden"), 
           ~case_when(.x >= 0.3 & .x <= 0.5 ~ 1,
                      is.na(.x) ~ NA,
                      TRUE ~ 0),
           .names = "{.col}burden_mod"),
    across(contains("ratio") & !contains("burden"), 
           ~case_when(.x > 0.5 ~ 1,
                      is.na(.x) ~ NA,
                      TRUE ~ 0),
           .names = "{.col}burden_sev"))

# summary of homeownership, renter rate, and housing insecurity by race
housing_summaryXwbhaa <- housing_summary |> 
  group_by(year) |> 
  summarize_groups(wbhaa_white | wbhaa_black | wbhaa_hispanic | wbhaa_AIAN | wbhaa_AAPI,
                   ownership_rate = weighted.mean(owner, w = hhwt, na.rm = TRUE),
                   renter_share = weighted.mean(renter, w = hhwt, na.rm = TRUE),
                   owncost_ratio_burden = weighted.mean(owncost_ratio_burden, w = hhwt, na.rm = TRUE),
                   rent_ratio_burden = weighted.mean(rent_ratio_burden, w = hhwt, na.rm = TRUE)) |> 
  filter(group_value == 1) |> select(-group_value)


# formatting summary df to separate dfs for excel export
homeownershipXwbhaa <- df_reformat_fun(housing_summaryXwbhaa, ownership_rate)
rentersXwbhaa <- df_reformat_fun(housing_summaryXwbhaa, renter_share)
owncost_burdenXwbhaa <- df_reformat_fun(housing_summaryXwbhaa, owncost_ratio_burden)
rentcost_burdenXwbhaa <- df_reformat_fun(housing_summaryXwbhaa, rent_ratio_burden)

# measuring housholds with moderate and severe housing insecurity by race
burdens_summaryXwbhaa <- housing_summary |> 
  #filter(owncost_ratio_burden == 1 | rent_ratio_burden == 1) |> 
  group_by(year) |> 
  summarize_groups(wbhaa_white | wbhaa_black | wbhaa_hispanic | wbhaa_AIAN | wbhaa_AAPI,
                   owncost_burden_mod = weighted.mean(owncost_ratioburden_mod, w = hhwt, na.rm = TRUE),
                   owncost_burden_sev = weighted.mean(owncost_ratioburden_sev, w = hhwt, na.rm = TRUE),
                   rent_burden_mod = weighted.mean(rent_ratioburden_mod, w = hhwt, na.rm = TRUE),
                   rent_burden_sev = weighted.mean(rent_ratioburden_sev, w = hhwt, na.rm = TRUE)) |> 
  filter(group_value == 1) |> select(-group_value)

# formatting summary df to separate dfs for excel export
owncost_modXwbhaa <- df_reformat_fun(burdens_summaryXwbhaa, owncost_burden_mod)
owncost_sevXwbhaa <- df_reformat_fun(burdens_summaryXwbhaa, owncost_burden_sev)
rent_modXwbhaa <- df_reformat_fun(burdens_summaryXwbhaa, rent_burden_mod)
rent_sevXwbhaa <- df_reformat_fun(burdens_summaryXwbhaa, rent_burden_sev)


# summary of housing stats by poverty category
housing_summaryXpoverty <- housing_summary |> 
  group_by(year) |> 
  summarize_groups(pov200 | pov100 | pov50,
                   ownership_rate = weighted.mean(owner, w = hhwt, na.rm = TRUE),
                   renter_share = weighted.mean(renter, w = hhwt, na.rm = TRUE),
                   owncost_ratio_burden = weighted.mean(owncost_ratio_burden, w = hhwt, na.rm = TRUE),
                   rent_ratio_burden = weighted.mean(rent_ratio_burden, w = hhwt, na.rm = TRUE)) |> 
  filter(group_value == 1) |> select(-group_value)

homeownershipXpoverty <- df_reformat_fun(housing_summaryXpoverty, ownership_rate)
rentersXpoverty <- df_reformat_fun(housing_summaryXpoverty, renter_share)
owncost_burdenXpoverty <- df_reformat_fun(housing_summaryXpoverty, owncost_ratio_burden)
rentcost_burdenXpoverty <- df_reformat_fun(housing_summaryXpoverty, rent_ratio_burden)

# measuring households with moderate and severe housing insecurity by poverty categories
burdens_summaryXpoverty <- housing_clean |> 
  inner_join(housing_summary) |> 
  filter(owncost_ratio_burden == 1 | rent_ratio_burden == 1) |> 
  group_by(year) |> 
  summarize_groups(pov200 | pov100 | pov50,
                   owncost_burden_mod = weighted.mean(owncost_ratioburden_mod, w = hhwt, na.rm = TRUE),
                   owncost_burden_sev = weighted.mean(owncost_ratioburden_sev, w = hhwt, na.rm = TRUE),
                   rent_burden_mod = weighted.mean(rent_ratioburden_mod, w = hhwt, na.rm = TRUE),
                   rent_burden_sev = weighted.mean(rent_ratioburden_sev, w = hhwt, na.rm = TRUE)) |> 
  filter(group_value == 1) |> select(-group_value)

owncost_modXpoverty <- df_reformat_fun(burdens_summaryXpoverty, owncost_burden_mod)
owncost_sevXpoverty <- df_reformat_fun(burdens_summaryXpoverty, owncost_burden_sev)
rent_modXpoverty <- df_reformat_fun(burdens_summaryXpoverty, rent_burden_mod)
rent_sevXpoverty <- df_reformat_fun(burdens_summaryXpoverty, rent_burden_sev)
  
# distribution of homeowners by poverty level
homeownershipXpoverty <- housing_summary |> 
  filter(owner == 1) |> 
  mutate(above_100 = if_else(pov100 == 0, 1, 0)) |> 
  summarize(above_100 = weighted.mean(above_100, w = hhwt, na.rm = TRUE),
            pov100 = weighted.mean(pov100, w = hhwt, na.rm = TRUE),
            pov50 = weighted.mean(pov50, w = hhwt, na.rm = TRUE),
            .by = year)

source(here("code/ACS/housing_export.R"), echo = TRUE)

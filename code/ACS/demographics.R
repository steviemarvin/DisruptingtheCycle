# additional functions
table1_race_fun <- function(data, title, var){
  data |> 
    mutate(all_families = title) |> 
    pivot_wider(id_cols = all_families, names_from = wbhaa, values_from = {{var}})
}

# load data
ddi <- read_ipums_ddi(here(demog_ddi))
demo_raw <- read_ipums_micro(ddi, verbose = FALSE)
names(demo_raw) <- tolower(names(demo_raw))

# clean raw data
demo_clean <- cleaning_fun(demo_raw)
  
# filter to just low income households where household heads have children u18
demo_lowinc <- filter(demo_clean,
                      u18 == 1 & poverty < 200 & poverty != 0,
                      related == 101,
                      !is.na(wbhaa)) |> 
  select(year, serial) |> 
  # repopulate df with all household invidivuals
  inner_join(demo_clean)

# df with race/ethnicity of household head
demo_hhead_wbhaa <- demo_lowinc |> 
  # filter to just household head
  filter(related == 101) |> 
  select(serial, wbhaa, hhwt) |> 
  rename(wbhaa_hh = wbhaa) |> 
  filter(!is.na(wbhaa_hh))

demo_lowinc_count <- summarize(demo_hhead_wbhaa,
                              total_hh = sum(hhwt)) |>
  pull()
  
### DEMOGRAPHIC TABLES START HERE ###
# absolute count of low-income people
demo_lowinc_totpop <- filter(demo_clean,
                             poverty < 200 & poverty != 0) |> 
  summarize(weighted_count = round(sum(perwt, na.rm = TRUE), 0))


demo_clean |> 
  filter(gq != 3) |> 
  mutate(lowincome = if_else(poverty < 200, 1, 0)) |> 
  crosstab(year, lowincome, w = perwt)

# count of low income households by race and ethnicity
demo_wbhaa <- demo_hhead_wbhaa |> 
  summarize(weighted_sum = round(sum(hhwt, 0)),
            .by = wbhaa_hh) |> 
  mutate(share = weighted_sum / demo_lowinc_count) |> 
  select(-weighted_sum) |> 
  rename(wbhaa = wbhaa_hh) |> 
  table1_race_fun(title = "Race/ethnicity of household head", var = share)


### FOREIGN BORN ###
demo_lowinc <- mutate(demo_lowinc, 
                      foreign_born = case_when(bpl >= 1 & bpl <= 99 ~ 0,
                                               bpl > 99 & bpl <= 800 ~ 1,
                                               TRUE ~ NA))
                      
# count and share of foreign born household head
demo_foreignborn <- demo_lowinc |> 
  filter(related == 101) |> 
  summarize(share = weighted.mean(foreign_born, w = hhwt, na.rm = TRUE)) |> 
  mutate(all_families = "Share of foreign born household heads")

  
# count and share of foreign born household head by race/ethn
demo_foreignbornXwbhaa <- demo_lowinc |> 
  filter(related == 101, !is.na(wbhaa)) |> 
  summarize(foreign_born = weighted.mean(foreign_born, w = hhwt, na.rm = TRUE),
            .by = wbhaa) |> 
  table1_race_fun(title = "Share of foreign born household heads", foreign_born)


### DESCRIPTIVE VARS OF HOUSEHOLD HEAD ###

# marital status of household head 
demo_lowinc <- mutate(demo_lowinc,
                      married_parent = if_else((marst == 1 | marst == 2) & u18 == 1, 1, 0),
                      married_parent = labelled(married_parent,
                                                c("married" = 1,
                                                  "not_married" = 0)))

demo_hhead_marst <- demo_lowinc |> 
  filter(related == 101, u18 == 1) |> 
  summarize(share = weighted.mean(married_parent, w = perwt, na.rm = TRUE))|> 
  mutate(all_families = "Share of married household heads")


demo_hhead_marstXwbhaa <- demo_lowinc |> 
  filter(related == 101, u18 == 1, !is.na(wbhaa)) |> 
  summarize(share_married = weighted.mean(married_parent, w = perwt, na.rm = TRUE),
            .by = wbhaa) |> 
  table1_race_fun(title = "Share of married household heads", share_married)


# LGB status of household head <3 NEED TO CHECK
demo_lowinc <- mutate(demo_lowinc,
                      samesex = if_else(coupletype == 2 | coupletype == 4, 1, 0))

demo_LGB <- demo_lowinc |> 
  filter(related == 201 | related == 1114) |> 
  summarize(share = weighted.mean(samesex, w = hhwt)) |> 
  mutate(all_families = "Share of heads with same-sex spouse/partner")

           
demo_LGBXwbhaa <- demo_lowinc |> 
  filter(related == 201 | related == 1114) |> 
  left_join(demo_hhead_wbhaa) |> 
  filter(!is.na(wbhaa_hh)) |> 
  summarize(share_samesex = weighted.mean(samesex, w = hhwt),
            .by = wbhaa_hh) |> 
  rename(wbhaa = wbhaa_hh) |> 
  table1_race_fun(title = "Share of heads with same-sex spouse/partner", share_samesex)


# gender of household head
demo_lowinc <- mutate(demo_lowinc,
                      female = if_else(sex == 2, 1, 0))

demo_hhead_gender <- demo_lowinc |> 
  filter(related == 101) |> 
  summarize(share = weighted.mean(female, w = perwt)) |> 
  mutate(all_families = "Share of female household heads")
  
# gender of household head by race
demo_hhead_genderXwbhaa <- demo_lowinc |> 
  filter(related == 101 & !is.na(wbhaa)) |> 
  summarize(share_women = weighted.mean(female, w = perwt),
            .by = wbhaa) |> 
  table1_race_fun(title = "Share of female household heads", share_women)



# age of household head
demo_hhead_age <- demo_lowinc |> 
  filter(related == 101, !is.na(wbhaa)) |>
  summarize(mean_age = round(weighted.mean(age, w = perwt), 1),
            median_age = median(age),
            .by = c(wbhaa, sex)) |> 
  pivot_wider(id_cols = wbhaa, 
              names_from = sex, 
              values_from = c("mean_age", "median_age"))


### NUMBER OF CHILDREN IN HOUSEHOLD ###

demo_lowinc <- mutate(demo_lowinc,
                      childu18 = if_else(age < 18, 1, 0),
                      childu6 = if_else(age < 6, 1, 0))

# households by number of children in the household
demo_numchild <- demo_lowinc |> 
  summarize(across(childu18 | childu6, ~sum(.x)),
            hhwgt = max(hhwt),
            .by = serial) |> 
  mutate(across(childu18 | childu6, ~case_when(.x == 1 ~ 1,
                                               .x == 2 ~ 2, 
                                               .x == 3 ~ 3,
                                               .x > 3 ~ 4,
                                               TRUE ~ 0),
                .names = "num{.col}")) |> 
  dummy_cols(select_columns = c('numchildu18', 'numchildu6'), 
             ignore_na = TRUE) |> 
  summarize(across(contains("numchildu18_") | contains("numchildu6_"), 
                   ~round(sum(.x * hhwgt), 0))) |> 
  pivot_longer(cols = everything(),
               names_to = c('group', 'num_child'),
               names_sep = '_',
               values_to = 'count') |> 
  pivot_wider(id_cols = group, 
              names_from = num_child, 
              values_from = count) |>
  mutate(at2 = `2` + `3` + `4`,
         across(!matches("group"),
                ~.x / demo_lowinc_count)) |> 
  select(-contains("0"), -contains("3"))

# households by number of children in household by race/ethn
demo_wbhaa_count <- filter(demo_lowinc, 
                           related == 101,
                           !is.na(wbhaa)) |> 
  summarize(sum = sum(hhwt),
            .by = wbhaa) |> 
  rename(wbhaa_hh = wbhaa)

demo_numchildXwbhaa <- demo_lowinc |> 
  summarize(across(childu18 | childu6, ~sum(.x)),
            hhwgt = max(hhwt),
            .by = serial) |> 
  mutate(across(childu18 | childu6, ~case_when(.x == 1 ~ 1,
                                               .x == 2 ~ 2, 
                                               .x == 3 ~ 3,
                                               .x > 3 ~ 4,
                                               TRUE ~ 0),
                .names = "{.col}")) |> 
  dummy_cols(select_columns = c('childu18', 'childu6')) |> 
  inner_join(demo_hhead_wbhaa) |>
  summarize(across(contains("childu18_") | contains("childu6_"), 
                   ~round(sum(.x * hhwgt), 0)),
            .by = wbhaa_hh) |> 
  inner_join(demo_wbhaa_count) |> 
  mutate(at2_childu18 = childu18_2 + childu18_3 + childu18_4,
         at2_childu6 = childu6_2 + childu6_3 + childu6_4,
         across(!matches("wbhaa_hh"), 
                ~.x / sum)) |> 
  select(-contains("_3"), -sum)




### DISABILITY ###
demo_lowinc <- demo_lowinc |> 
  mutate(across(contains("diff"), ~case_when(.x == 0 ~ NA,
                                             .x == 1 ~ 0,
                                             .x == 2 ~ 1)), 
         diffany_child = case_when((diffrem == 1 | diffphys == 1 | 
                                      diffmob == 1 | diffcare == 1 | 
                                      diffsens == 1 | diffeye == 1 |
                                      diffhear == 1) & age < 18 ~ 1,
                                   ((diffrem == 0 | is.na(diffrem)) & 
                                      (diffphys == 0 | is.na(diffphys)) & 
                                      (diffmob == 0 | is.na(diffmob)) &
                                      (diffcare == 0 | is.na(diffcare)) & 
                                      diffsens == 0 & diffeye == 0 & 
                                      diffhear == 0) & age < 18 ~ 0,
                                   TRUE ~ NA),
         diffany_adult = case_when((diffrem == 1 | diffphys == 1 | 
                                      diffmob == 1 | diffcare == 1 | 
                                      diffsens == 1 | diffeye == 1 |
                                      diffhear == 1) ~ 1,
                                   (diffrem == 0 & diffphys == 0 & 
                                      diffmob == 0 & diffcare == 0 & 
                                      diffsens == 0 & diffeye == 0 & 
                                      diffhear == 0) ~ 0,
                                   TRUE ~ NA),
         diff_parent = case_when(diffany_adult == 1 & (related == 101 | related == 201 | related == 1114) ~ 1,
                                 diffany_adult == 0 & (related == 101 | related == 201 | related == 1114) ~ 0,
                                 TRUE ~ NA),
         diff_child = case_when(diffany_child == 1 ~ 1,
                                diffany_child == 0 ~ 0,
                                TRUE ~ NA),
         diff_childorparent = case_when(diff_parent == 1 ~ 1,
                                        diff_child == 1 ~ 1,
                                        TRUE ~ 0))

demo_disability <- demo_lowinc |> 
  summarize(across(diff_childorparent | hhwt, 
                   ~max(.x)),
            .by = serial) |> 
  summarize(share = weighted.mean(diff_childorparent, w = hhwt, na.rm = TRUE)) |> 
  mutate(all_families = "Share of households with a disabled parent or child")

demo_disabilityXwbhaa <- demo_lowinc |> 
  summarize(across(diff_childorparent | hhwt, 
                   ~max(.x)),
            .by = serial) |> 
  inner_join(demo_hhead_wbhaa) |> 
  summarize(share_diff = weighted.mean(diff_childorparent, w = hhwt, na.rm = TRUE),
            .by = wbhaa_hh) |> 
  rename(wbhaa = wbhaa_hh) |> 
  table1_race_fun(title = "Share of households with a disabled parent or child", share_diff)


### INTERGENERATIONAL HOUSEHOLDS ###
demo_lowinc <- mutate(demo_lowinc,
                      intergen = if_else(multgen == 3, 1, 0))


demo_intergen <- demo_lowinc |> 
  filter(related == 101) |> 
  summarize(share = weighted.mean(intergen, w = hhwt, na.rm = TRUE)) |> 
  mutate(all_families = "Share of intergenerational households")

  
# intergenerational households by race/ethn
demo_intergenXwbhaa <- demo_lowinc |> 
  filter(related == 101, !is.na(wbhaa)) |> 
  summarize(share_intergen = weighted.mean(intergen, w = hhwt, na.rm = TRUE),
            .by = wbhaa) |> 
  table1_race_fun(title = "Share of intergenerational households", share_intergen)


source(here("code/ACS/table1_allfamilies.R"), echo = TRUE)

source(here("code/ACS/demographics_export.R"), echo = TRUE)

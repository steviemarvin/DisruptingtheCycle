# demographics of all families

# setting up baseline df
all_hh <- filter(demo_clean,
                     u18 == 1,
                     related == 101, 
                     !is.na(wbhaa)) |> 
  select(serial) |> 
  inner_join(demo_clean)

# getting value of total # of households 
all_hh_count <- summarize(filter(all_hh, related == 101),
                          total_hh = sum(hhwt)) |> 
  pull()

# df with the race/ethnicity of all household heads with children u18
all_wbhaa_df <- filter(all_hh,
                            related == 101, 
                            !is.na(wbhaa)) |>
  select(serial, wbhaa) |> 
  rename(wbhaa_hh = wbhaa)

### DEMOGRAPHICS FOR ALL FAMILIES ###

# Race/ethnicity of household head
all_wbhaa <- filter(demo_clean,
                         u18 == 1,
                         related == 101,
                         !is.na(wbhaa)) |> 
  summarize(weighted_sum = round(sum(hhwt, 0 )),
            .by = wbhaa) |> 
  mutate(share = weighted_sum / all_hh_count) |> 
  table1_race_fun(title = "Race/ethnicity of household head", var = share)


# Share of foreign born household heads
all_hh <- mutate(all_hh, 
                      foreign_born = case_when(bpl >= 1 & bpl <= 99 ~ 0,
                                               bpl > 99 & bpl <= 800 ~ 1,
                                               TRUE ~ NA))

# count and share of foreign born household head
all_foreignborn <- all_hh |> 
  filter(related == 101) |> 
  summarize(share = weighted.mean(foreign_born, w = hhwt, na.rm = TRUE)) |> 
  mutate(all_families = "Share of foreign born household heads")

# count and share of foreign born household head by race/ethn
all_foreignbornXwbhaa <- all_hh |> 
  filter(related == 101, !is.na(wbhaa)) |> 
  summarize(foreign_born = weighted.mean(foreign_born, w = hhwt, na.rm = TRUE),
            .by = wbhaa) |> 
  table1_race_fun(title = "Share of foreign born household heads", foreign_born)


# Share of married household heads
all_hh <- mutate(all_hh,
                      married_parent = if_else((marst == 1 | marst == 2) & u18 == 1, 1, 0),
                      married_parent = labelled(married_parent,
                                                c("married" = 1,
                                                  "not_married" = 0)))

all_hhead_marst <- all_hh |> 
  filter(related == 101, u18 == 1) |> 
  summarize(share = weighted.mean(married_parent, w = perwt, na.rm = TRUE)) |> 
  mutate(all_families = "Share of married household heads")

all_hhead_marstXwbhaa <- all_hh |> 
  filter(related == 101, u18 == 1, !is.na(wbhaa)) |> 
  summarize(share_married = weighted.mean(married_parent, w = perwt, na.rm = TRUE),
            .by = wbhaa) |> 
  table1_race_fun(title = "Share of married household heads", share_married)


# Share of heads with same-sex spouse/partner
all_hh <- mutate(all_hh,
                      samesex = if_else(coupletype == 2 | coupletype == 4, 1, 0))

all_LGB <- all_hh |> 
  filter(related == 201 | related == 1114) |> 
  summarize(share = weighted.mean(samesex, w = hhwt)) |> 
  mutate(all_families = "Share of heads with same-sex spouse/partner")

all_LGBXwbhaa <- all_hh |> 
  filter(related == 201 | related == 1114) |> 
  left_join(all_wbhaa_df) |> 
  filter(!is.na(wbhaa_hh)) |> 
  summarize(share_samesex = weighted.mean(samesex, w = hhwt),
            .by = wbhaa_hh) |> 
  rename(wbhaa = wbhaa_hh) |> 
  table1_race_fun(title = "Share of heads with same-sex spouse/partner", share_samesex)

# Share of female household heads
all_hh <- mutate(all_hh,
                      female = if_else(sex == 2, 1, 0))

all_hhead_gender <- all_hh |> 
  filter(related == 101 & !is.na(wbhaa)) |> 
  summarize(share = weighted.mean(female, w = perwt)) |> 
  mutate(all_families = "Share of female household heads")

# gender of household head by race
all_hhead_genderXwbhaa <- all_hh |> 
  filter(related == 101 & !is.na(wbhaa)) |> 
  summarize(share_women = weighted.mean(female, w = perwt),
            .by = wbhaa) |> 
  table1_race_fun(title = "Share of female household heads", share_women)


# NUMBER OF CHILDREN
# creating variables that indicate resp is a childu18/childu6
all_hh <- mutate(all_hh,
                      childu18 = if_else(age < 18, 1, 0),
                      childu6 = if_else(age < 6, 1, 0))

# share of all households with at least 2 children u18 and u6
all_numchild <- all_hh |> 
  summarize(across(childu18 | childu6, ~sum(.x)),
            hhwgt = max(hhwt),
            .by = serial) |> 
  inner_join(all_wbhaa_df) |> 
  mutate(across(childu18 | childu6, 
                ~if_else(.x >= 2, 1, 0), 
                .names = "at2.{.col}")) |> 
  summarize(across(contains("at2"),
                   ~round(sum(.x * hhwgt), 0))) |> 
  mutate(across(everything(), 
                ~.x/all_hh_count)) |> 
  pivot_longer(cols = everything(),
               names_to = 'all_families',
               values_to = 'share') |> 
  mutate(all_families = if_else(all_families == "at2.childu18", 
                                "Share of households with 2+ children under 18",
                                "Share of households with 2+ children under 6"))

# share of all households with at least 2 children u18 and u6 by race/ethnicity
# getting total family counts by race and ethnicity
all_wbhaa_ct <- filter(all_hh, 
                       related == 101,
                       !is.na(wbhaa)) |> 
  summarize(sum = sum(hhwt),
            .by = wbhaa) |> 
  rename(wbhaa_hh = wbhaa)

# share of children by race/ethnicity
all_numchildXwbhaa <- all_hh |> 
  summarize(across(childu18 | childu6, ~sum(.x)),
            hhwgt = max(hhwt),
            .by = serial) |> 
  inner_join(all_wbhaa_df) |> 
  mutate(across(childu18 | childu6, 
                ~if_else(.x >= 2, 1, 0), 
                .names = "at2_{.col}")) |> 
  summarize(across(contains("at2"),
                   ~round(sum(.x * hhwgt), 0)),
            .by = wbhaa_hh) |> 
  inner_join(all_wbhaa_ct) |> 
  mutate(across(contains("at2"), 
                ~.x/sum)) |> 
  select(-sum) |> 
  pivot_longer(cols = contains("at2"),
               values_to = 'share') |> 
  pivot_wider(id_cols = name, 
              names_from = wbhaa_hh, 
              values_from = share) |> 
  mutate(all_families = if_else(name == "at2_childu18", 
                                "Share of households with 2+ children under 18",
                                "Share of households with 2+ children under 6")) |>
  select(-name)

### DISABILITY ###
all_hh <- all_hh |> 
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

all_disability <- all_hh |> 
  summarize(across(diff_childorparent | hhwt, 
                   ~max(.x)),
            .by = serial) |> 
  summarize(share = weighted.mean(diff_childorparent, w = hhwt, na.rm = TRUE)) |> 
  mutate(all_families = "Share of households with a disabled parent or child")

all_disabilityXwbhaa <- all_hh |> 
  summarize(across(diff_childorparent | hhwt, 
                   ~max(.x)),
            .by = serial) |> 
  inner_join(all_wbhaa_df) |> 
  summarize(share_diff = weighted.mean(diff_childorparent, w = hhwt, na.rm = TRUE),
            .by = wbhaa_hh) |> 
  rename(wbhaa = wbhaa_hh) |> 
  table1_race_fun(title = "Share of households with a disabled parent or child", share_diff)


### INTERGENERATIONAL HOUSEHOLDS ###
all_hh <- mutate(all_hh,
                      intergen = if_else(multgen == 3, 1, 0))


all_intergen <- all_hh |> 
  filter(related == 101) |> 
  summarize(share = weighted.mean(intergen, w = hhwt, na.rm = TRUE)) |> 
  mutate(all_families = "Share of intergenerational households")


# intergenerational households by race/ethn
all_intergenXwbhaa <- all_hh |> 
  filter(related == 101, !is.na(wbhaa)) |> 
  summarize(share_intergen = weighted.mean(intergen, w = hhwt, na.rm = TRUE),
            .by = wbhaa) |> 
  table1_race_fun(title = "Share of intergenerational households", share_intergen)

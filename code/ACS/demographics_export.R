### PREPARING DEMOGRAPHIC DFS FOR OUTPUT ###
demo_source = "Source: EPI analysis of 2023 American Community Survey 1-Year microdata via IPUMS"

# reformating num child dfs for table 1
demo_numchild_table1 <- select(demo_numchild, group, at2) %>% 
  mutate(group = if_else(group == "numchildu18", 
                         "Share of households with 2+ children under 18",
                         "Share of households with 2+ children under 6")) %>% 
  rename(all_families = group, 
         share = at2)


demo_numchildXwbhaa_table1 <- select(demo_numchildXwbhaa, wbhaa_hh, contains("at2")) %>% 
  pivot_longer(cols = contains("at2"),
               values_to = 'share') %>% 
  pivot_wider(id_cols = name, 
              names_from = wbhaa_hh, 
              values_from = share) %>% 
  mutate(all_families = if_else(name == "at2_childu18", 
                                "Share of households with 2+ children under 18",
                                "Share of households with 2+ children under 6")) %>%
  select(-name)


# binding dfs for "all" category
table1_lowinc <- data.frame(all_families = "Race/ethnicity of household head",
                            share = 0) %>% 
  rbind(demo_foreignborn) %>% 
  rbind(demo_hhead_marst) %>% 
  rbind(demo_LGB) %>% 
  rbind(demo_hhead_gender) %>% 
  rbind(demo_numchild_table1) %>% 
  select(all_families, share)

# binding dfs by race and ethnicity
table1_lowincXwbhaa <- demo_wbhaa %>% 
  rbind(demo_foreignbornXwbhaa) %>% 
  rbind(demo_hhead_marstXwbhaa) %>% 
  rbind(demo_LGBXwbhaa) %>% 
  rbind(demo_hhead_genderXwbhaa) %>% 
  rbind(demo_numchildXwbhaa_table1) %>% 
  select(all_families, white, black, hispanic, AIAN, AAPI)

# combinding the all and race dfs together
table1_lowinc <- full_join(table1_lowinc, table1_lowincXwbhaa) %>% 
  rename(All = share, lowinc_families = all_families)


# preparing all families data for table 1 export
table1_allfamilies <- data.frame(all_families = "Race/ethnicity of household head",
                                 share = 0) %>% 
  rbind(all_foreignborn) %>% 
  rbind(all_hhead_marst) %>% 
  rbind(all_LGB) %>% 
  rbind(all_hhead_gender) %>% 
  rbind(all_numchild) %>% 
  select(all_families, share)

table1_allfamiliesXwbhaa <- all_wbhaa %>% 
  rbind(all_foreignbornXwbhaa) %>% 
  rbind(all_hhead_marstXwbhaa) %>% 
  rbind(all_LGBXwbhaa) %>% 
  rbind(all_hhead_genderXwbhaa) %>% 
  rbind(all_numchildXwbhaa) %>% 
  select(all_families, white, black, hispanic, AIAN, AAPI)

table1_allfamilies <- full_join(table1_allfamilies, table1_allfamiliesXwbhaa) %>% 
  rename(All = share)


# preparing remainder of num child table formatting here
demo_numchild <- select(demo_numchild, -contains("at2"), -contains("_0")) %>% 
  pivot_longer(cols = c('1', '2', '4'),
               values_to = 'share') %>% 
  mutate(all_families = case_when(group == "numchildu18" & name == 1 ~ "Share of households with 1 child under 18",
                                  group == "numchildu18" & name == 2 ~  "Share of households with 2 children under 18",
                                  group == "numchildu18" & name == 4 ~ "Share of households with 4+ children under 18",
                                  group == "numchildu6" & name == 1 ~ "Share of households with 1 child under 6",
                                  group == "numchildu6" & name == 2 ~ "Share of households with 2 children under 6",
                                  group == "numchildu6" & name == 4 ~ "Share of households with 4+ children under 6")) %>% 
  select(share, all_families)


demo_numchildXwbhaa <- select(demo_numchildXwbhaa, -contains("at2"), -contains("_0")) %>% 
  pivot_longer(cols = contains("child"),
               values_to = 'share') %>% 
  pivot_wider(id_cols = name,
              names_from = wbhaa_hh,
              values_from = share) %>% 
  mutate(all_families = case_when(name == "childu18_1" ~ "Share of households with 1 child under 18",
                                  name == "childu18_2" ~ "Share of households with 2 children under 18",
                                  name == "childu18_4" ~ "Share of households with 4+ children under 18",
                                  name == "childu6_1" ~ "Share of households with 1 child under 6",
                                  name == "childu6_2" ~ "Share of households with 2 children under 6",
                                  name == "childu6_4" ~ "Share of households with 4+ children under 6")) %>% 
  select(-name)

# combining dfs 
combined_lowinc_demo <- demo_numchild  %>% 
  rbind(demo_disability) %>% 
  rbind(demo_intergen)
  

combined_lowinc_demoXwbhaa <- demo_numchildXwbhaa %>% 
  rbind(demo_disabilityXwbhaa) %>% 
  rbind(demo_intergenXwbhaa)

combined_lowinc_demo <- inner_join(combined_lowinc_demo, combined_lowinc_demoXwbhaa) %>% 
  select(all_families, share, white, black, hispanic, AIAN, AAPI) %>% 
  rename(lowinc_families = all_families,
         All = share)

full_lowinc_demo <- table1_lowinc %>% 
  rbind(combined_lowinc_demo)


### EXCEL OUTPUT ###

# exporting demographics data to excel
demo_charts <- createWorkbook()

# table 1: summary demographic stats to be used in report
sheets_fun(table1_lowinc, demo_charts, s = "Table 1", col = 1, )
writeData(wb = demo_charts,
          sheet = "Table 1",
          x = table1_allfamilies,
          startCol = 1,
          startRow = 10)
writeData(wb = demo_charts, 
          sheet = "Table 1",
          x = demo_source,
          startCol = 1,
          startRow = 19)

sheets_fun(full_lowinc_demo, demo_charts, s = "Full stats", col = 1, demo_source)

saveWorkbook(demo_charts, here("output","demo_charts.xlsx"), overwrite = TRUE)


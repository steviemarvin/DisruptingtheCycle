# filter to keep households where the household head has a child
households_df <- filter(asec_clean,
                        u18 == 1 & pov200 == 1,
                        relate == 101) %>% 
  select(year, serial) %>% 
  inner_join(asec_clean)

hh_wbhaa <- filter(asec_clean,
                      relate == 101) %>% 
  select(year, serial, wbhaa) %>% 
  rename(wbhaa_hh = wbhaa)

# summarizing labor stat of adults in households
all_adults <- households_df %>% 
  filter(age >= 18) %>% 
  mutate(
    # reformatting workly variable
    workly = case_when(workly == 0 ~ NA,
                       workly == 1 ~ 0,
                       workly == 2 ~ 1),
    # variable that indicates adults that are also hh head, spouse or other related indiv.
    related_adults = if_else(age >= 18 &
                               (hh_head_sp == 1 | related_adult == 1 | relate == 1114), 1, 0),
    related_earner = if_else(earner == 1 & related_adults == 1, 1, 0),
    # recategorizing variables below to focus just on household head
    across(offtotval | contains("pov") | workly, 
           ~if_else(relate == 101, .x, 0)),
    # earner stat of household head and spouse, variable needed to distinguish between household and family
    earner_p = case_when(earner == 1 & hh_head_sp == 1 ~ 1,
                         earner == 0 & hh_head_sp == 0 ~ 0,
                         TRUE ~ NA),
    nonearner_p = case_when(nonearner_adult == 1 & hh_head_sp == 1 ~ 1,
                            nonearner_adult == 0 & hh_head_sp == 1 ~ 0,
                            TRUE ~ NA)) %>% 
  # collapsing by household
  summarize(
    # max is to get an indicator value from these variables
    across(offtotval | asecwth | pov100 | pov50 | workly | ft,
           ~max(.x)),
    # sum is to get number of each variable for each household
    across(contains("earner") | related_adults | ft, 
           ~sum(.x, na.rm = TRUE)),
    .by = c(year, serial)) %>% 
  mutate(across(contains("earner") | ft,
                ~if_else(.x >= 1, 1, 0), 
                .names = "at1_{.col}"),
         num_rel_earner = case_when(related_earner == 0 ~ 0,
                                    related_earner == 1 ~ 1,
                                    related_earner == 2 ~ 2,
                                    related_earner >= 3 ~ 3)) %>% 
  dummy_cols(select_columns = c('num_rel_earner'), ignore_na = TRUE) %>% 
  left_join(hh_wbhaa)
  
samplesize_check <- all_adults %>% 
  summarize(
    sum = n(),
    .by = c(year, wbhaa_hh)) %>% 
  filter(!is.na(wbhaa_hh)) %>% 
  pivot_wider(id_cols = year, names_from = wbhaa_hh, values_from = sum)

### MAIN OUTPUT ###

# lf stat summary for households
hh_lfstat_sum <- all_adults %>% 
  summarize(
    at1_ft = weighted.mean(at1_ft, w = asecwth, na.rm = TRUE),
    num_earner_0 = weighted.mean(num_rel_earner_0, w = asecwth, na.rm = TRUE),
    .by = c(year, wbhaa_hh)) %>% 
  filter(!is.na(wbhaa_hh))

at1_earner <- hh_lfstat_sum %>% 
  pivot_wider(id_cols = year, names_from = wbhaa_hh, values_from = at1_ft)

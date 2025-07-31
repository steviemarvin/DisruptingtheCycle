# df of race of household head
hh_wbhaa <- filter(asec_clean,
                      relate == 101) |> 
  select(year, serial, wbhaa) |> 
  rename(wbhaa_hh = wbhaa)

# baseline df for household labor force stats
household_lfstat <- asec_clean |> 
  # filter to ensure that all households are those where head has a child
  filter(u18 == 1 & pov200 == 1, relate == 101) |> 
  select(year, serial) |> 
  # re-join with large df to have all respondents
  left_join(asec_clean) |> 
  filter(age >= 18) |> 
  mutate(
    # capturing long-term unemployment
    wksunem_int = case_when(wksunem1 >= 27 & wksunem1 != 99 ~ 1,
                            wksunem1 < 27 | wksunem1 == 99 ~ 0),
    # variable that indicates adults that are also hh head, spouse or other related indiv.
    related_adults = if_else(age >= 18 &
                               (hh_head_sp == 1 | related_adult == 1 | relate == 1114), 1, 0),
    related_ft_earner = if_else(earner == 1 & related_adults == 1 & ft == 1, 1, 0))
  
### MAIN OUTPUT ###
# household heads experiencing long term unemployment
hh_ltunem <- household_lfstat |> 
  filter(relate == 101, wkswork1 > 0 & wkswork1 <= 51, !is.na(wbhaa)) |> 
  filter(classwly != 0 & classwly != 29 & classwly != 99 & classwly != 13) |> 
  summarize(lt_unem = weighted.mean(wksunem_int, w = asecwt, na.rm = TRUE),
            .by = c(year, wbhaa)) |> 
  pivot_wider(id_cols = year, names_from = wbhaa, values_from = lt_unem) |> 
  select(year, white, black, hispanic, AIAN, AAPI)

# households with at least 1 full time earner
at1_earner <- household_lfstat |> 
  mutate(asecwth = if_else(relate == 101, asecwth, 0)) |> 
  summarize(related_ft_earner = sum(related_ft_earner, na.rm = TRUE),
            asecwth = max(asecwth),
            .by = c(year, serial)) |> 
  mutate(at1_ft_earner = if_else(related_ft_earner >= 1, 1, 0)) |> 
  left_join(hh_wbhaa) |> 
  filter(!is.na(wbhaa_hh)) |> 
  summarize(at1_ft_earner = weighted.mean(at1_ft_earner, w = asecwth, na.rm = TRUE),
            .by = c(year, wbhaa_hh)) |> 
  pivot_wider(id_cols = year, names_from = wbhaa_hh, values_from = at1_ft_earner) |> 
  select(year, white, black, hispanic, AIAN, AAPI)
  


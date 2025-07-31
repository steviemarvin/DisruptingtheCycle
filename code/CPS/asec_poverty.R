#Poverty benchmarking and charts
poverty <- asec_clean |> 
  filter(u18 == 1 & pov200 == 1 & relate == 101)

# share below FPL and below 50% of FPL
povertylevels <- poverty |> 
  summarize(across(pov100 | pov50, ~weighted.mean(.x, w = asecwth, na.rm = TRUE),
                   .names = "share_{.col}"),
            .by = year)

# share below FPL and below 50% of FPL by race and ethnicity
povertylevelsXwbhaa <- poverty |> 
  summarize(pov100 = weighted.mean(pov100, w = asecwth, na.rm = TRUE),
            pov50 = weighted.mean(pov50, w = asecwth, na.rm = TRUE),
            .by = c(year, wbhaa)) |> 
  filter(!is.na(wbhaa)) |> 
  pivot_wider(id_cols = year, 
              names_from = wbhaa, 
              values_from = c("pov100", "pov50"))

# function that reformats df povertylevelsXwbhaa for exporting
povlev_reformat_fun <- function(data, povertylevel){
  renaming = paste0(povertylevel, "_")
  
  data |> 
    select(year, contains(povertylevel)) |> 
    rename_with(~gsub(renaming, "", .), contains(renaming)) |> 
    select(year, white, black, hispanic, AIAN, AAPI)
}

povertylevelsXwbhaa100 <- povlev_reformat_fun(povertylevelsXwbhaa, "pov100")
povertylevelsXwbhaa50 <- povlev_reformat_fun(povertylevelsXwbhaa, "pov50")


povertyXwbhaa_fun <- function(data, povertylevel){
  data |>
    filter({{povertylevel}} == 1) |> 
    dummy_cols(select_columns = c('wbhaa'), ignore_na = TRUE) |> 
    summarize(across(contains("wbhaa_"), ~weighted.mean(.x, w = asecwth, na.rm = TRUE)),
              .by = year) |> 
    rename_with(~gsub("wbhaa_", "", .), contains("wbhaa_")) |> 
    select(year, white, black, hispanic, AIAN, AAPI)
}


pov200Xwbhaa <- poverty |> 
  dummy_cols(select_columns = c('wbhaa'), ignore_na = TRUE) |> 
  summarize(across(contains("wbhaa_"), ~weighted.mean(.x, w = asecwth, na.rm = TRUE)),
            .by = year) |> 
  rename_with(~gsub("wbhaa_", "", .), contains("wbhaa_"))|> 
  select(year, white, black, hispanic, AIAN, AAPI)

pov100Xwbhaa <- povertyXwbhaa_fun(poverty, pov100)

pov50Xwbhaa <- povertyXwbhaa_fun(poverty, pov50)

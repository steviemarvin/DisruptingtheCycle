# poverty stats use the same data files as housing stats

# poverty benchmark
poverty_benchmark <- filter(housing_clean,
                          u18 == 1 & poverty < 200 & poverty != 0,
                          related == 101)

povertylevels <- poverty_benchmark |> 
  summarize(pov100 = weighted.mean(pov100, w = hhwt, na.rm = TRUE),
            pov50 = weighted.mean(pov50, w = hhwt, na.rm = TRUE),
            .by = year)

povertylevelsXwbhaa <- poverty_benchmark |> 
  summarize(pov100 = weighted.mean(pov100, w = hhwt, na.rm = TRUE),
            pov50 = weighted.mean(pov50, w = hhwt, na.rm = TRUE),
            .by = c(year, wbhaa)) |> 
  filter(!is.na(wbhaa)) |> 
  pivot_wider(id_cols = year, 
              names_from = wbhaa, 
              values_from = c("pov100", "pov50"))

distribution_fun <- function(data, povlev){
  data |> 
    filter({{povlev}} == 1) |> 
    summarize(count = sum(hhwt, na.rm = TRUE),
              .by = c(year, wbhaa)) |> 
    filter(!is.na(wbhaa)) |> 
    pivot_wider(id_cols = year, 
                names_from = wbhaa, 
                values_from = count) |> 
    mutate(sum = white + black + hispanic + AIAN + AAPI,
           across(white | black | hispanic | AIAN | AAPI, 
                  ~.x / sum)) |> 
    select(-sum)
  
}
  
pov200_dist <- distribution_fun(poverty_benchmark, pov200)

pov100_dist <- distribution_fun(poverty_benchmark, pov100)

pov50_dist <- distribution_fun(poverty_benchmark, pov50)

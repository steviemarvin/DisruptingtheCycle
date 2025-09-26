# tabulating estimates for absolute counts 
# need to have run demographics.R first

# total count of low-income individuals
totalcount_indiv <- demo_clean |> 
  filter(poverty != 0, perwt > 0) |> 
  crosstab(year, pov200, w = perwt)

totalcount_hh <- demo_lowinc |> 
  filter(related == 101) |> 
  crosstab(year, pov200, w = hhwt)


# exporting housing dfs
# objects with the source to include under each table
housing_source = "Source: EPI analysis of 2007-2023 American Community Survey 1-Year microdata via IPUMS"


housing_charts <- createWorkbook()

sheets_fun(homeownershipXwbhaa, housing_charts, s = "Homeownership rate | race", 1, housing_source)

sheets_fun(rentersXwbhaa, housing_charts, s = "Share renters | race", 1, housing_source)

sheets_fun(owncost_burdenXwbhaa, housing_charts, s = "Housing insecurity | race", 1, housing_source)
sheets_fun(rentcost_burdenXwbhaa, housing_charts, s = "Housing insecurity | race", 8)

sheets_fun(owncost_modXwbhaa, housing_charts, s = "Housing insecurity (mod) | race", 1, housing_source)
sheets_fun(rent_modXwbhaa, housing_charts, s = "Housing insecurity (mod) | race", 8)

sheets_fun(owncost_sevXwbhaa, housing_charts, s = "Housing insecurity (sev) | race", 1, housing_source)
sheets_fun(rent_sevXwbhaa, housing_charts, s = "Housing insecurity (sev) | race", 8)

sheets_fun(homeownershipXpoverty, housing_charts, s = "Homeowners | poverty", 1, housing_source)

sheets_fun(owncost_modXpoverty, housing_charts, s = "Housing insecurity (mod) | pov", 1, housing_source)
sheets_fun(rent_modXpoverty, housing_charts, s = "Housing insecurity (mod) | pov", 8)

sheets_fun(owncost_sevXpoverty, housing_charts, s = "Housing insecurity (sev) | pov", 1, housing_source)
sheets_fun(rent_sevXpoverty, housing_charts, s = "Housing insecurity (sev) | pov", 8)

saveWorkbook(housing_charts, here("output","housing_charts.xlsx"), overwrite = TRUE)

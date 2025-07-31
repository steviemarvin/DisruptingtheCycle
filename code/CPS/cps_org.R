# data source
org_note = "Note: Race and ethnicity are mutually exclusive."
org_source = "Source: Economic Policy Institute. 2025. Current Population Survey Extracts, Version 2025.5.8, https://microdata.epi.org."

# run code provided by Jori Kandra to create poverty 200 threshold for cps org data
source(here("data/cps_basic_poverty/cps_basic_poverty.R"), echo = TRUE)

# keep only variables I need for analysis from data_db
org_clean <- select(data_db, year, month, finalwgt, basicwgt, orgwgt, age, wbho, cow1, emp, agechild, ownchild, spov200, union, unmem) |> 
  filter(spov200 == 1, wbho != 4, age >= 18, cow1 %in% (1:5)) |> 
  mutate(primeage = if_else(age >= 25 & age <= 54, 1, 0),
         parent = if_else(ownchild >= 1 & agechild != 0, 1, 0),
         wbho = labelled(wbho, c("White" = 1, "Black" = 2, "Hispanic" = 3)),
         wbho = as.character(as_factor(wbho)),
         date = as.Date(paste(year, month, 1, sep = "-"), "%Y-%m-%d"))

# prime-age EPOP for low-income parents
pa_epop_parents <- filter(org_clean, primeage == 1, parent == 1) |> 
  summarize(pa_epop = weighted.mean(emp, w = basicwgt, na.rm = TRUE),
            .by = c(date, wbho)) |> 
  pivot_wider(id_cols = date, names_from = wbho, values_from = pa_epop)

pa_unmem_parents <- filter(org_clean, primeage == 1, parent == 1, emp == 1) |> 
  summarize(unmem = weighted.mean(unmem, w = orgwgt, na.rm = TRUE),
            .by = c(date, wbho)) |> 
  pivot_wider(id_cols = date, names_from = wbho, values_from = unmem)

pa_unmem_parents_year <- filter(org_clean, primeage == 1, parent == 1) |> 
  summarize(unmem = weighted.mean(unmem, w = orgwgt / 12, na.rm = TRUE),
            .by = c(year, wbho)) |> 
  pivot_wider(id_cols = year, names_from = wbho, values_from = unmem)

org_charts <- createWorkbook()

sheets_fun <- function(data, wb, s) {
  
  # determining number of columns in df
  col_number <- ncol(data)
  
  # add worksheet
  addWorksheet(wb, sheetName = paste0(s))
  # map data + determine what column df will start
  writeData(wb, sheet = paste0(s), x = data, startCol = 1, startRow = 2)
  writeData(wb, sheet = paste0(s), x = org_note, startCol = 1, startRow = nrow(data)+3)
  writeData(wb, sheet = paste0(s), x = org_source, startCol = 1, startRow = nrow(data)+4)
  
  addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = '0.0%'), cols = 2:col_number, rows = 2:(nrow(data)+2), gridExpand = TRUE)
  
  return(data)
  
}

sheets_fun(pa_epop_parents, org_charts, s = "PA EPOP | race")
sheets_fun(pa_unmem_parents, org_charts, s = "Union membership | race")
sheets_fun(pa_unmem_parents_year, org_charts, s = "Union membership (yr) | race")


saveWorkbook(org_charts, here("output","org_charts.xlsx"), overwrite = TRUE)



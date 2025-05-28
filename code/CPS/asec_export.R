asec_source = "Source: EPI analysis of 2008-2024 US Census Bureau Current Population Survey Annual Social and Economic Supplement microdata"

sheets_fun <- function(data, wb, s, col) {
  
  # determining number of columns in df
  col_number <- ncol(data)
  scnd_df <- col+1
  
  # add worksheet
  if(col == 1){
    # add worksheet
    addWorksheet(wb, sheetName = paste0(s))
    # map data + determine what column df will start
    writeData(wb, sheet = paste0(s), x = data, startCol = col, startRow = 2)
    writeData(wb, sheet = paste0(s), x = asec_source, startCol = col, startRow = nrow(data)+4)
    
  } else {
    # map data + determine what column df will start
    writeData(wb, sheet = paste0(s), x = data, startCol = col, startRow = 2)
  }
  
  # format cells to percentage
  if(col == 1) {
    addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = '0.0%'), cols = 2:col_number, rows = 2:(nrow(data)+2), gridExpand = TRUE)
  } else {
    addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = '0.0%'), cols = scnd_df:(scnd_df+col_number), rows = 2:(nrow(data)+2), gridExpand = TRUE)
  }
  
  return(data)
  
}

# create workbook
asec_charts <- createWorkbook()

# At least 1 earner
sheets_fun(at1_earner, asec_charts, s = "At least 1 earner", col = 1)

# Share of household heads experiencing long-term unemployment
sheets_fun(hh_ltunem, asec_charts, s = "Heads exp lt unem", col = 1)

# Low income families by poverty level 
sheets_fun(povertylevels, asec_charts, s = "Poverty levels", col = 1)

# Race and ethnicity shares of low-income families, 2007-2023
sheets_fun(pov200Xwbhaa, asec_charts, s = "POV 200 | race", col = 1)

# Race and ethnicity shares of families below FPL, 2007-2023
sheets_fun(pov100Xwbhaa, asec_charts, s = "POV 100 | race", col = 1)

# Race and ethnicity shares of families below 50% of FPL, 2007-2023
sheets_fun(pov50Xwbhaa, asec_charts, s = "POV 50 | race", col = 1)

# Share of families below the FPL by race and ethnicity, 2007-2023
sheets_fun(povertylevelsXwbhaa100, asec_charts, s = "Share below FPL | race", col = 1)

# Share of families below 50% of FPL by race and ethnicity, 2007-2023
sheets_fun(povertylevelsXwbhaa50, asec_charts, s = "Share below 50% FPL | race", col = 1)

saveWorkbook(asec_charts, here("output","asec_charts.xlsx"), overwrite = TRUE)


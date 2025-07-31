cleaning_fun <- function(data){
  data |>
    mutate(
      # indicator var: respondent is either the hh head or spouse
      hh_head_sp = if_else(related == 101 | related == 201, 1, 0),
      #indicator var: whether or not respondent has a child under 18 or under 6
      u18 = if_else(yngch < 18 | eldch < 18, 1, 0),
      u6 = if_else(yngch < 6 | eldch < 6, 1, 0),
      # race & ethnicity var, modeling epi microdata extract variable
      wbhaa = case_when(
        # hispanic
        hispan >= 1 & hispan <= 4 ~ 3,
        # white alone, non-hispanic
        raced == 100 & hispan == 0 ~ 1, 
        # black alone or in any combination, non-hispanic
        racblk == 2 & hispan == 0 ~ 2, 
        # AIAN, exclude black
        racamind == 2 & racblk == 1 & hispan == 0 ~ 4, 
        # AAPI, exclude black and AIAN
        (racasian == 2 | racpacis == 2) & 
          racblk == 1 & racamind == 1 & hispan == 0 ~ 5,
        TRUE ~ NA),
      # label wbhaa variable
      wbhaa = labelled(wbhaa, c("white" = 1, 
                                "black" = 2, 
                                "hispanic" = 3, 
                                "AIAN" = 4, 
                                "AAPI" = 5)),
      pov200 = if_else(poverty < 200 & poverty > 0, 1, 0),
      pov100 = if_else(poverty < 100 & poverty > 0, 1, 0),
      pov50 = if_else(poverty < 50 & poverty > 0, 1, 0), 
      # changing to factor
      across(wbhaa, ~as.character(as_factor(.x)))) %>% 
    # creating indicator variables for each race/ethnicity for summarizing families as a whole down the line
    dummy_cols(select_columns = 'wbhaa', ignore_na = TRUE)
}

sheets_fun <- function(data, wb, s, col, source = NULL) {
  
  # determining number of columns in df
  col_number <- ncol(data)
  scnd_df <- col+1
  
  # add worksheet
  if(col == 1){
    # add worksheet
    addWorksheet(wb, sheetName = paste0(s))
    # map data + determine what column df will start
    writeData(wb, sheet = paste0(s), x = data, startCol = col, startRow = 2)
    if(!is.null(source)){
      writeData(wb, sheet = paste0(s), x = source, startCol = col, startRow = nrow(data)+4)
    }
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



asec_clean <- asec_raw %>% 
  # filtering out hflag == 0 to ensure 2014 ASEC is properly weighted
  filter(hflag == 1 | is.na(hflag)) %>% 
  mutate(
    # reformatting years for asec (since its about last year)
    survey_year = year,
    year = year - 1,
    # cleaning up NA values
    offtotval = if_else(offtotval == 9999999999, NA, offtotval),
    offcutoff = if_else(offcutoff == 999999, NA, offcutoff),
    # indicator var: poverty variables
    pov200 =  if_else(offtotval < (offcutoff * 2), 1, 0),
    pov100 = if_else(offtotval < offcutoff, 1, 0),
    pov50 = if_else(offtotval < (offcutoff / 2), 1, 0),
    # indicator var: adult status and household head / spouse status
    hh_head_sp = if_else(relate == 101 | 
                            relate == 201 | relate == 202 | relate == 203, 1, 0),
    # indicator var: respondent is an adult related to the household head 
    related_adult = if_else(relate >= 301 & relate <= 1001 & age >= 18, 1, 0),
    # indicator var: whether or not respondent has a child under 18 or under 6
    u18 = if_else(yngch < 18 | eldch < 18, 1, 0),
    u6 = if_else(yngch < 6 | eldch < 6, 1, 0),
    # race & ethnicity var, modeling epi microdata extract variable
    wbhaa = case_when(
      # hispanic
      hispan >= 1 & hispan <= 612 ~ 3,
      # white alone, non-hispanic
      race == 100 & hispan == 0 ~ 1,
      # black alone or in combinatoin, non-hispanic
      (race == 200 | race == 801 | 
         race == 805 | race == 806 | 
         race == 807 | race == 810 | 
         race == 811 | race == 814 | 
         race == 816 | race == 818) & 
        hispan == 0 ~ 2,
      # AIAN 
      (race == 300 | race == 802 | 
         race == 808 | race == 812 | 
         race == 813 | race == 815 | 
         race == 817 | race == 819) & 
        hispan == 0 ~ 4,  
      # AAPI
      ((race >= 650 & race <= 652) | 
         race == 809 | race == 803 | 
         race == 804) & 
        hispan == 0 ~ 5, 
      TRUE ~ NA),
    # label wbhaa variable
    wbhaa = labelled(wbhaa, c("white" = 1, 
                              "black" = 2, 
                              "hispanic" = 3, 
                              "AIAN" = 4, 
                              "AAPI" = 5)),
    wbhaa = as.character(as_factor(wbhaa)),
    # indicator vars: type of parent
    #marst = 6: never married/single
    single_parent = if_else(marst == 6 & u18 == 1, 1, 0), 
    #marst = 1: married, spouse present; 2: married, spouse absent
    married_parent = if_else((marst == 1 | marst == 2) & u18 == 1, 1, 0), 
    #3: separated; 4: Divorced; 5: Widowed; 7: Widowed or Divorced
    not_married_parent = if_else((marst >= 3 & marst <= 6) & u18 == 1, 1, 0), 
    # labor force variables
    # uhrsworkly = usual hours worked per week last year
    uhrsworkly = if_else(uhrsworkly == 999, 0, uhrsworkly),
    # designating full time and part time status
    ft = case_when(uhrsworkly >= 35 ~ 1,
                   uhrsworkly < 35 ~ 0,
                   TRUE ~ NA),
    # indicator var: earner status 
    annhrs = uhrsworkly * wkswork1,
    earner = if_else(annhrs != 0 & incwage != 0 & age >= 16 & 
                       (classwly != 0 & classwly != 29 & classwly != 99 & classwly != 13), 1, 0),
    nonearner_adult = if_else(annhrs == 0 & incwage == 0 & age >= 18, 1, 0),
    # reformating relate variable
    relate = case_when(relate == 202 | relate == 203 ~ 201,
                       relate == 1116 | relate == 1117 ~ 1114,
                       TRUE ~ relate)) %>% 
  dummy_cols(select_columns = 'wbhaa', ignore_na = TRUE)


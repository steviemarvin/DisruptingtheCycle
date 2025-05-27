# initial data load
library(tidyverse)
library(ipumsr)
library(here)

# set ipums key
ipumsr::set_ipums_api_key("ccd674ca01d26d540280185d70c617db212c3619", save = TRUE) 

demo_var_list = c(
  "YEAR",
  "RACE", "HISPAN", "RACAMIND", "RACASIAN", "RACBLK", "RACPACIS",
  "SEX", "AGE", "MARST", "BPL", "CITIZEN", "POVERTY",
  "NCHILD", "ELDCH", "YNGCH", 
  "RELATE", "COUPLETYPE", "MULTGEN",
  "DIFFREM", "DIFFPHYS", "DIFFMOB", "DIFFCARE", "DIFFSENS", "DIFFEYE", "DIFFHEAR"
) 

housing_var_list = list(
  var_spec("RELATE", case_selections = "1"),
  "YEAR",
  "RACE", "HISPAN", "RACAMIND", "RACASIAN", "RACBLK", "RACPACIS",
  "AGE",
  "NCHILD", "ELDCH", "YNGCH", 
  "POVERTY", 
  "HHINCOME", "RENTGRS", "OWNCOST", "OWNERSHP"
  )

# load demographics ipums usa extract
acs_demo_2023 <- define_extract_micro(
  collection = "usa",
  description = 'RWJF_demographics',
  samples = c('us2023a'),
  variables = demo_var_list) %>% 
  submit_extract() %>% 
  wait_for_extract()

# Download extract to input folder 
download_ext <- download_extract(extract = acs_demo_2023, 
                                 download_dir = "/projects/smarvin/RWJF_Summary/data/", overwrite = TRUE)

demo_raw <- read_ipums_micro(download_ext)
names(demo_raw) <- tolower(names(demo_raw))

### HOUSING DATA INIT DOWNLOAD ###

# load housing ipums usa extract
acs_housing <- define_extract_micro(
  collection = "usa",
  description = 'RWJF_housing',
  samples = list(
    'us2007a', 'us2008a', 'us2009a', 'us2010a', 'us2011a', 'us2012a',
    'us2013a', 'us2014a', 'us2015a', 'us2016a', 'us2017a', 'us2018a', 
    'us2019a', 'us2021a', 'us2022a', 'us2023a'),
  variables = housing_var_list) %>% 
  submit_extract() %>% 
  wait_for_extract()

# Download extract to input folder 
download_ext <- download_extract(extract = acs_housing, 
                                 download_dir = "/projects/smarvin/RWJF_Summary/data/", overwrite = TRUE)

housing_raw <- read_ipums_micro(download_ext)
names(housing_raw) <- tolower(names(housing_raw))

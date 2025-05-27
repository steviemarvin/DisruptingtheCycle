# initial data load
library(tidyverse)
library(ipumsr)
library(here)

# set ipums key
ipumsr::set_ipums_api_key("ccd674ca01d26d540280185d70c617db212c3619", save = TRUE) 

asec_var_list = c(
  "YEAR", "MONTH",
  "RACE", "HISPAN", "MARST", "RELATE", "AGE", 
  "NCHILD", "ELDCH", "YNGCH", 
  "OFFTOTVAL", "OFFCUTOFF", 
  "WORKLY", "CLASSWLY", "UHRSWORKLY", "WKSWORK1"
)

# load demographics ipums usa extract
asec_20082024 <- define_extract_micro(
  collection = "cps",
  description = "RWJF families",
  samples = c('cps2008_03s', 'cps2009_03s', 'cps2010_03s', 'cps2011_03s',
              'cps2012_03s', 'cps2013_03s', 'cps2014_03s', 'cps2015_03s',
              'cps2016_03s', 'cps2017_03s', 'cps2018_03s', 'cps2019_03s',
              'cps2020_03s', 'cps2021_03s', 'cps2022_03s', 'cps2023_03s',
              'cps2024_03s'),
  variables = asec_var_list) %>% 
  submit_extract() %>% 
  wait_for_extract()


# Download extract to input folder 
download_ext <- download_extract(extract = asec_20082024, 
                                 download_dir = "/projects/smarvin/RWJF_Summary/data/", overwrite = TRUE)

asec_raw <- read_ipums_micro(download_ext)
names(asec_raw) <- tolower(names(asec_raw))

### CPS BASIC DOWNLOAD ###
cps_var_list = list(
  var_spe("NCHILD", case_selections = c("1", "2", "3", "4", "5", "6", "7", "8", "9")),
  "YEAR", "MONTH",
  "RACE", "HISPAN", "AGE", 
  "NCHILD", "ELDCH", "YNGCH", 
  "OFFTOTVAL", "OFFCUTOFF",
  "LFSTAT", "EMP"
)

cps_2007_2023 <- define_extract_micro(
  collection = "cps",
  description = "RWJF families monthly",
  samples = c(''),
  variables = cps_var_list) %>% 
  submit_extract() %>% 
  wait_for_extract()





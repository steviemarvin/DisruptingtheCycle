library(ipumsr)
library(tidyverse) 
library(here) 
library(epidatatools) # summarize_groups
library(fastDummies) # create indicator variables
library(openxlsx) # workbook output
library(haven) # labelled()

# run init_data_load if data is not already downloaded in data folder
# data loads are in demographics.R and housing.R

# see file names in ~RWJF_Summary/data/
# need to change these manually if loading a new data set
demog_ddi = "data/usa_00047.xml"
housing_ddi = "data/usa_00051.xml"


source(here("code/ACS/acs_functions.R"), echo = TRUE)


# demographics of low income families
source(here("code/ACS/demographics.R"), echo = TRUE)


# housing stats for low income families
source(here("code/ACS/housing.R"), echo = TRUE)

# poverty stats for low income families
source(here("code/ACS/acs_poverty.R"), echo = TRUE)


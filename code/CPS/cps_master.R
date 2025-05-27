library(ipumsr)
library(here)
library(tidyverse)
library(fastDummies)
library(openxlsx)
library(haven)

## load data here FILL OUT DATA FILE MANUALLY
ddi <- read_ipums_ddi(here("data/cps_00023.xml"))
asec_raw <- read_ipums_micro(ddi, verbose = FALSE)
names(asec_raw) <- tolower(names(asec_raw))


# clean asec data
source(here("code/CPS/asec_clean.R"), echo = TRUE)

source(here("code/CPS/asec_hh_lfstat.R"), echo = TRUE)

source(here("code/CPS/asec_poverty.R"), echo = TRUE)

source(here("code/CPS/asec_export.R"), echo = TRUE)

## load cps data here FILL OUT DATA FILE MANUALLY

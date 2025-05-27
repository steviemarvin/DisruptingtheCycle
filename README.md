# Disrupting the Cycle data
This repository hosts all of the code used to produce data for the Disrupting the Cycle report. Below, there are details on how the code is organized.

### Folders you will need
You will need a data folder and an output folder to house data files and output files.

### ACS and CPS
The code is broadly organized by the source data. This project uses both the American Community Survey and Current Population Survey microdata obtained from IPUMS via the ipumsr package.
* ACS data is used to obtain 2023 demographic details and details on housing outcomes for low income families with children.
* CPS data, specifically the Annual Social and Economic Supplements data, is used to obtain details on labor force outcomes for low income families with children.

## Folder: ACS
1. acs_init_data_load.R
   _This R script file includes code that submits a data request to IPUMS to obtain data files.
   _You only need to run this the first time to get the necessary .xml and .dat.gz files to load to your workspace. The data files will be saved to your data folder which will be called in by the demographics.R and housing.R files. 
2. acs_master.R
   _This file is the master file that will execute the main code (after the initial data is downloaded to your project). One thing to note about this file is that objects **demog_ddi** and **housing_ddi** are used to call the appropriate .xml file. You should edit this according to your own .xml file in your data folder.  
3. acs_functions.R
   _This file includes two user defined functions: cleaning_fun() and sheets_fun().
    _cleaning_fun(): performs appropriate data cleaning and variable creation for all raw ACS data files.
     _needs libraries **tidyverse**, **haven** and **fastDummies** to run
     _used in demographics.R and housing.R
    _sheets_fun(): maps dataframes for excel export.
     _needs libraries **tidverse**, and **openxlsx** to run.
     _used in demographics_export.R and housing_export.R
4. demographics.R
7. housing.R
8. export files
   _demographics_export.R
   _housing_export.R

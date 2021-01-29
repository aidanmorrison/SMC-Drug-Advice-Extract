## Extract CMS Drugs Advice by quarter using HTML Scraping
##
## This script was written to be run on the RStudio Desktop App
##
## Outputs: this script will output CMS data for a specified FQ as a .csv
##
## Original Date: 15/01/2021
## Last Update: 29/01/2021
##
## Required Packages:
##  tidyverse (for basic data manipulation and pipe %>%)
##  rvest (for HTML Scraping. The XML dependency must be as up to date as possible)
##
## Contents
##  SECTION 1 - SETUP
##  SECTION 2 - GET DATA

## SECTION 1 - SETUP --------------------------------------------------------------
## Functions script (this script also loads in the tidyverse and rvest packages)
filepath <- paste0("//freddy/DEPT/PHIBCS/PHI/Prescribing/Topics/",
                   "Controlled Drugs/Not Recommended Drugs/",
                   "SMC Data Extract/Scripts/")

source(paste0(filepath, "01 - Functions.R"))


## SECTION 2 - GET DATA -----------------------------------------------------------
table <- get_data(FY = 2021, FQ = "Q3")


## View/ save Results -------------------------------------------------------------
table
save_data(FY = "2020-21", FQ = "Q3", table)

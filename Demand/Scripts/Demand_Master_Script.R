# Run scripts to produce a master demand table


require(tidyverse)
require(sf)
require(openxlsx)
require(mapview)
require(lwgeom)
require(httr)
require(data.table)
require(odbc)
require(DBI)
require(readxl)
require(janitor)
require(writexl)


# Generic functions that are used in multiple scripts
source("Scripts/Shared_Functions.R")



# Get watershed names and identifiers
ws <- makeSharePointPath("Watershed Folders/Watershed_Demand_Dataset_Paths.xlsx") %>%
  read_xlsx(sheet = "Main_Sheet", skip = 1)



# IMPORTANT!! CHOOSE A WATERSHED
ws <- ws[2, ] # Change the row index to your desired watershed



stopifnot(nrow(ws) == 1)



cat(paste0("Running scripts for ", ws$NAME, "\n"))



# GIS Pre-Processing Initial Steps
source("Scripts/GIS_POD_Flat_File_Prep.R")


# GIS Pre-Processing
source("Scripts/GIS_Preprocessing.R")


# Uses coordinate data input into the "R_Review" worksheet of the GIS Pre-Processing spreadsheet
# to identify which PODs flow into the watershed (via USGS StreamStats)
source("Scripts/POD_StreamStats_Analysis.R")


# Convert "water_use_report_extended.csv" to a SQLite database
#source("Scripts/Extended_CSV_to_SQLite.R")


# QA/QC functions for correcting unit conversion errors and duplicate reporting
source("Scripts/QAQC_Functions.R")


# A function to update reported amounts for new rights
source("Scripts/Face_Value_Substitution.R")


# Priority Date Pre-Processing
source("Scripts/Priority_Date_Preprocessing.R")


# Priority Date Module
source("Scripts/Priority_Date.R")


# Priority Date Post-Processing
source("Scripts/Priority_Date_Postprocessing.R")


# Duplicate Report Module *
  # Identifies 1 owner per water right per reporting year
  # Identifies if a single owner submitted duplicate reports across multiple water rights
  # in the same year
  # Doesn't need to be run again unless we want to analyze new Russian River water rights; 
  # the manual review has already been performed on the duplicates
source("Scripts/Multiple_Owner_Analysis.R")


# Expected Demand Module
source("Scripts/Expected_Demand.R")


# Supplemental Expected Demand Module
source("Scripts/Expected_Demand_Units_Issue_Flagger.R")


# Beneficial Use, Return Flow Module
source("Scripts/Beneficial_Use_Return_Flow.R")


# Diversion Out of Season Module (Parts A and B)*
#source("Scripts/Diversion_Out_Of_Season.R")


# Duplicate Report, Same Owner Module*
#source("Scripts/DuplicateReport_SameOwner.R")


# POD Sub-basin Assignment
source("Scripts/Assign_Subbasin_to_POD.R")


# QA/QC Working File Module*
  # This script was originally used to develop the QAQC Working File spreadsheet, but has been
  # superseded by the MasterDemandTable script, which produces the 2017_2020_RR_MasterDemandTable and
  # 2017-2022_R_MasterDemandTable CSVs directly
#source("Scripts/QAQC_Working_File.R") 


# MasterDemandTable.CSV for DWRAT
source("Scripts/MasterDemandTable.R")


# * = Script is not needed for the master demand table
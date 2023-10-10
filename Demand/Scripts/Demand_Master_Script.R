# Run scripts to produce a master demand table


# GIS Pre-Processing
#source("Scripts/GIS_POD_Flat_File_Prep.R")


# A QA/QC function for unit conversion errors
source("Scripts/QAQC_Functions.R")

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


# Beneficial Use, Return Flow Module
source("Scripts/Beneficial_Use_Return_Flow.R")


# Diversion Out of Season Module (Parts A and B)*
source("Scripts/Diversion_Out_Of_Season.R")


# Missing RMS Reports Module*
source("Scripts/Missing_RMS_Reports.R")


# POD Sub-basin Assignment
source("Scripts/Assign_Subbasin_to_POD.R")


# QA/QC Working File Module*
  # This script was originally used to develop the QAQC Working File spreadsheet, but has been
  # superseded by the MasterDemandTable script, which produces the 2023_RR_MasterDemandTable CSV directly
#source("Scripts/QAQC_Working_File.R") 

# MasterDemandTable.CSV for DWRAT
source("Scripts/MasterDemandTable.R")


# * = Script is not needed for the master demand table
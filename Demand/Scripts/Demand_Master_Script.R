# Run scripts to produce a master demand table


# GIS Pre-Processing
#source("Scripts/GIS_POD_Flat_File_Prep.R")


# A QA/QC function for unit conversion errors
source("Scripts/QAQC_Unit_Fixer_Function.R")


# Priority Date Pre-Processing
source("Scripts/Priority_Date_Preprocessing.R")


# Priority Date Module
source("Scripts/Priority_Date.R")


# Priority Date Post-Processing
source("Scripts/Priority_Date_Postprocessing.R")


# Duplicate Months, Years Module*
source("Scripts/DuplicateMonths_Years.R")


# Expected Demand Module
source("Scripts/Expected_Demand.R")


# Beneficial Use, Return Flow Module
source("Scripts/Beneficial_Use_Return_Flow.R")


# Diversion Out of Season Module (Parts A and B)*
source("Scripts/Diversion_Out_Of_Season.R")


# Duplicate Report, Same Owner Module*
source("Scripts/DuplicateReport_SameOwner.R")


# Missing RMS Reports Module*
source("Scripts/Missing_RMS_Reports.R")


# POD Subbasin Assignment
source("Scripts/Assign_Subbasin_to_POD.R")


# QA/QC Working File Module
source("Scripts/QAQC_Working_File.R")


# * = Script is not needed for the master demand table
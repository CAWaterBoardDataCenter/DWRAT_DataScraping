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

options(viewer = NULL) # For mapview


# There are several different coding blocks, which the SDA Demand QAQC Flags document 
  # in the SOPs and Documentation\1. Demand Data\SDU Methodology folder describes
  # in detail, but we have stamped them in the Demand Scripts 

# IMPORTANT!!
# Update "Watershed_Selection.R" to select a watershed
source("Scripts/Watershed_Selection.R") # DATA ACQUISITION SCRIPT

# IMPORTANT!! x2
# Specify the years to be included in the demand dataset
source("Scripts/Dataset_Year_Range.R")  #DATA ACQUISITION SCRIPT


# GIS Pre-Processing Initial Steps
source("Scripts/GIS_POD_Flat_File_Prep.R") # Consists of Remediation coding block and
# Data Acquisition coding block


# GIS Pre-Processing
source("Scripts/GIS_Preprocessing.R") # FLAGGING SCRIPT


# Uses coordinate data input into the "R_Review" worksheet of the GIS Pre-Processing spreadsheet
# to identify which PODs flow into the watershed (via USGS StreamStats)
source("Scripts/POD_StreamStats_Analysis.R")


# Convert "water_use_report_extended.csv" to a SQLite database
#source("Scripts/Extended_CSV_to_SQLite.R")


# Priority Date Pre-Processing
source("Scripts/Priority_Date_Preprocessing.R") # FLAGGING SCRIPT


# Priority Date Module
source("Scripts/Priority_Date.R") # FLAGGING SCRIPT and # DWRAT COMPLIANCE SCRIPT


# Priority Date Post-Processing
source("Scripts/Priority_Date_Postprocessing.R") # FLAGGING SCRIPT and REMEDIATION SCRIPT

# Duplicate Report Module *
  # Identifies 1 owner per water right per reporting year
  # Identifies if a single owner submitted duplicate reports across multiple water rights
  # in the same year
  # Doesn't need to be run again unless we want to analyze new Russian River water rights; 
  # the manual review has already been performed on the duplicates
source("Scripts/Multiple_Owner_Analysis.R") # FLAGGING SCRIPT

# Expected Demand Module
#skipped by Payman on 5/2/2024
source("Scripts/Expected_Demand.R") # FLAGGING SCRIPT

# Supplemental Expected Demand Module
source("Scripts/Expected_Demand_Units_Issue_Flagger.R") # FLAGGING SCRIPT


# Try to fix reports with NA values for all months and diversion types
#skipped by Payman on 5/2/2024
source("Scripts/Check_Empty_Reports.R") # FLAGGING AND REMEDIATION SCRIPT

# Beneficial Use, Return Flow Module
source("Scripts/Beneficial_Use_Return_Flow.R") # FLAGGING SCRIPT and An ARTIFACT
# because DWRAT does not consider beneficial uses or return flows


# Diversion Out of Season Module (Parts A and B)*
#source("Scripts/Diversion_Out_Of_Season.R")


# Duplicate Report, Same Owner Module*
#source("Scripts/DuplicateReport_SameOwner.R")


# POD Sub-basin Assignment
#source("Scripts/Assign_Subbasin_to_POD.R")
source("Scripts/Assign_Subbasin_via_Connectivity_Matrix.R")
# ^ Alternative script that uses connectivity matrix for sub-basin assignment



# QA/QC Working File Module*
  # This script was originally used to develop the QAQC Working File spreadsheet, but has been
  # superseded by the MasterDemandTable script, which produces the 2017_2020_RR_MasterDemandTable and
  # 2017-2022_R_MasterDemandTable CSVs directly
#source("Scripts/QAQC_Working_File.R") 


# MasterDemandTable.CSV for DWRAT
source("Scripts/MasterDemandTable.R")


# * = Script is not needed for the master demand table
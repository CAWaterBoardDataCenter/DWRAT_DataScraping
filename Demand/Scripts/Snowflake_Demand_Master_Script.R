# Scripts to produce a master demand table for modeling with DWRAT

#### Dependencies ####

require(data.table)
require(DBI)
require(dbplyr)
require(httr)
require(janitor)
require(lwgeom)
require(mapview)
require(odbc)
require(openxlsx)
require(readxl)
require(sf)
require(tidyverse)
require(writexl)

#### Data Acquisition ####

# Download eWRIMS Flat Files (Snowflake connection required)
source("Scripts/[Data_Acquisition]_Flat_File_Download.R")

#### Data Filtering ####

# IMPORTANT!!
# Update "Watershed_Selection.R" to select a watershed
source("Scripts/Watershed_Selection.R")

# IMPORTANT!! x2
# Specify the years to be included in the demand dataset
source("Scripts/Dataset_Year_Range.R")

# Initial POD Selection
# (This is done in Snowflake now)
#source("Scripts/[Data_Filtering]_Initial_POD_List.R")

#### Flagging ####

# Create a new table that will have flags listed for
# every water right in the watershed
# (Every subsequent script in this section adds new flags to that table)
source("Scripts/[Flagging]_Initialize_Table.R")

# GIS Pre-Processing Flags
# Flag the PODs of water rights that may appear within
# the selected watershed's boundaries
# Four different flags are used to identify candidate PODs
source("Scripts/[Flagging]_GIS_Identify_Watershed_PODs.R")

# Multiple Owners and Duplicate Reporting Flags
# Looking for multiple reports for the same right in a year
# (This may be a concern when a right changes owners)
# Also flag water rights with the same owner and the same reported value 
# in the same reporting year
source("Scripts/[Flagging]_Duplicate_Reporting.R")






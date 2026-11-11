# PURPOSE -----------------------------------------------------------------------

# This script tries to recalculate the Mattole Demand Dataset (2017-2024) water rights
# and PODs based on these 2 files:
  ## Final_Mattole_Paradigm_List.xlsx, Final_Ben_List worksheet
  ## "MATT_2017_2024_MDT_2026-11-03.csv"

# Last Updated By: Payman Alemi on 11/12/2026

## LOAD ALL DWRAT_DATASCRAPING LIBRARIES AND FUNCTIONS
source("Additional_Scripts/Load_Packages.R")
source("W1_Watershed_Demand/Scripts/Shared_Functions_Demand.R")

# IMPORT MATOLE DATASETS---------------------------------------------------------
mattole_pod_path <- makeSharePointPath("Program Watersheds/1. Watershed Folders/Mattole River/Data/GIS_Preprocessing/Final_Mattole_Paradigm_List.xlsx")
nattole_pods <-
  
mattole_rights_path <-
mattole_rights

# SNOWFLAKE STUFF----------------------------------------------------------------
## Log into Snowflake----
sf_con <- DBI::dbConnect(drv = odbc::odbc(), 
                         dsn = "snowflake", 
                         server = "gb51005.west-us-2.azure.snowflakecomputing.com", 
                         Trusted_Connection = "True", 
                         authenticator = "externalbrowser",
                         database = "SWRCB_INTERNAL_PROD",
                         schema = "WR_CALWATRS_FLATFILES",
                         role = "DWR_DEV_DEMAND_DATA_FLAGS_RWC_ACROLE",
                         warehouse = "DWR_WH")


# Extract the annual reports table
annual_reports <- dbGetQuery(sf_con, 
                        "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.annual_reports
                        where AR_ANNUALREPORTYEAR >= 2016")

## Extract the participants table
participants <- dbGetQuery(sf_con, 
                    "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.participants")


## Extract  geospatial relationships table
geospatial_relationships <- dbGetQuery(sf_con,
                 "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.geospatial_relationships")

# Grab the Data Dictionary Table
data_dictionary <-dbGetQuery(sf_con,
                             "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.data_dictionary")



## Compare Annual Reports and Participants tables
overlappingNames <- base::intersect(names(annual_reports), names(participants)) %>% sort()

## Share 16 fields 

## Compare Annual Reports and Geospatial Relationships Table
base::intersect()
## Compare Participants and Geospatial Relationships Table








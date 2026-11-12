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
mattole_pods <- read_xlsx(file = mattole_pod_path, sheet = "Final_List_Ben_2026") # 667 PODs
  
mattole_rights_path <- makeSharePointPath(paste0(
  "Program Watersheds/1. Watershed Folders/Mattole River/",
  "Data/Demand_QAQC/MATT_2017_2024_MDT_2026-11-03.csv"
))


mattole_rights <- read_csv(mattole_rights_path) 

## Eliminate split rights-- _1 and _2 from the APPLICATION_NUMBER field
mattole_rights$APPLICATION_NUMBER <-gsub(pattern = "_[0-9]$",
                                         replacement = "",
                                         x = mattole_rights$APPLICATION_NUMBER)
n_distinct(mattole_rights$APPLICATION_NUMBER) # Returns 539 rights

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

### eWRIMS Tables----
# Whittle to Mattole dataset only to save space

#### Water Use Report Extended----

## Fields relevant to demand process
wure_fields<- c(
  "AMOUNT","APPLICATION_ACCEPTANCE_DATE","APPLICATION_NUMBER","APPLICATION_PRIMARY_OWNER",
  "APPLICATION_RECD_DATE","DIVERSION_TYPE","FACE_VALUE_AMOUNT", "FACE_VALUE_UNITS",
  "INI_REPORTED_DIV_AMOUNT","INI_REPORTED_DIV_UNIT","MONTH","PARTY_ID","PRIORITY_DATE","SUB_TYPE","WATER_RIGHT_TYPE",
  "YEAR","YEAR_DIVERSION_COMMENCED")

wure_mattole <- data.table::fread("W1_Watershed_Demand/Intermediate/water_use_report_extended.csv")%>%
 semi_join(
  y = mattole_rights,
  by = "APPLICATION_NUMBER") %>%
  select(wure_fields)
 # 539 water rights, as expected

#### ewrims_pod_flat_file-----

pod_fields <- c(
  "APPLICATION_NUMBER", "COUNTY", "EAST_COORD", "HUC_12_NAME", "HUC_12_NUMBER",
  "HUC_8_NAME", "HUC_8_NUMBER", "LATITUDE", "LONGITUDE", "MERIDIAN",
  "NORTH_COORD", "PARCEL_NUMBER", "POD_ID", "QUARTER", "QUARTER_QUARTER",
  "RANGE_DIRECTION", "RANGE_NUMBER", "SECTION_NUMBER", "SOURCE_NAME", "SP_ZONE",
  "TOWNSHIP_DIRECTION", "TOWNSHIP_NUMBER", "TRIB_DESC", "WATER_RIGHT_STATUS", "WATER_RIGHT_TYPE",
  "WATERSHED", "WR_WATER_RIGHT_ID"
)

ewrims_pod_ff_mattole <- data.table::fread("W1_Watershed_Demand/Intermediate/ewrims_flat_file_pod.csv") %>%
  semi_join(
    y = mattole_pods,
    by = "POD_ID") %>%
  select(pod_fields)


### Calwatrs Tables----
# Whittle to Mattole dataset only to save space


# Extract the annual reports table
annual_reports <- dbGetQuery(sf_con, 
                        "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.annual_reports
                        where AR_ANNUALREPORTYEAR >= 2016")

annual_reports_mattole <- inner_join(x = annual_reports,
                                     y = mattole_rights,
                                     by = c("WR_APP_CLAIM_NAME" = "APPLICATION_NUMBER"))
# Returns 539 rights

#### Are Any Mattole rights missing?
missing_mattole_rights <- anti_join(x = mattole_rights, 
                                    y = annual_reports_mattole,
                                    by = c("APPLICATION_NUMBER" = "WR_APP_CLAIM_NAME")) %>%
  select(APPLICATION_NUMBER) %>% 
  distinct() # 0 rights are missing

#### Export missing Mattole Rights to SharePoint folder
# mattole_export_path <- makeSharePointPath(paste0(
#   "SOPs + Documentation/3. Demand Data/",
#   "SDU Methodology/Calwatrs_Flat_File_Demand_Scripting/"
# ))
# write.csv(x = missing_mattole_rights, 
#           file = paste0(mattole_export_path, "missing_mattole_rights_", Sys.Date(), ".csv"),
#           row.names = FALSE)

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








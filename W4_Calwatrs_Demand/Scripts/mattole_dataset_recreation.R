# PURPOSE -----------------------------------------------------------------------

# This script tries to recalculate the Mattole Demand Dataset (2017-2024) water rights
# and PODs based on these 2 files:
  ## Final_Mattole_Paradigm_List.xlsx, Final_Ben_List worksheet
  ## "MATT_2017_2024_MDT_2026-11-03.csv"

# Last Updated By: Payman Alemi on 11/12/2026

# LOAD ALL DWRAT_DATASCRAPING LIBRARIES AND FUNCTIONS----
source("Additional_Scripts/Load_Packages.R")
source("W1_Watershed_Demand/Scripts/Shared_Functions_Demand.R")

# IMPORT MATTOLE DATASETS---------------------------------------------------------
mattole_pod_path <- makeSharePointPath("Program Watersheds/1. Watershed Folders/Mattole River/Data/GIS_Preprocessing/Final_Mattole_Paradigm_List.xlsx")
mattole_pods <- read_xlsx(file = mattole_pod_path, sheet = "Final_List_Ben_2026")
  n_distinct(mattole_pods$POD_ID) #667 unique PODs

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

# EWRIMS TABLES----------------------------------------------------------------
# Whittle to Mattole dataset only to save space

## water use report extended----

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

## ewrims_pod_flat_file----

pod_fields <- c(
  "APPLICATION_NUMBER", "COUNTY", "EAST_COORD", "HUC_12_NAME", "HUC_12_NUMBER",
  "HUC_8_NAME", "HUC_8_NUMBER", "LATITUDE", "LONGITUDE", "MERIDIAN",
  "NORTH_COORD", "PARCEL_NUMBER", "POD_ID", "QUARTER", "QUARTER_QUARTER",
  "RANGE_DIRECTION", "RANGE_NUMBER", "SECTION_NUMBER", "SOURCE_NAME", "SP_ZONE",
  "TOWNSHIP_DIRECTION", "TOWNSHIP_NUMBER", "TRIB_DESC", "WATER_RIGHT_STATUS", "WATER_RIGHT_TYPE",
  "WATERSHED", "WR_WATER_RIGHT_ID"
)

ewrims_pod_ff_mattole <- data.table::fread(
  "W1_Watershed_Demand/Intermediate/ewrims_flat_file_pod.csv",
  colClasses = list(integer = "POD_ID")
) %>%
  semi_join(
    y = mattole_pods,
    by = "POD_ID"
  ) %>%
  select(all_of(pod_fields))

n_distinct(ewrims_pod_ff_mattole$AP)

## ewrims_flat_file_use_season----
season_fields <- c("APPLICATION_NUMBER", "USE_CODE")

ewrims_season_ff_mattole <- data.table::fread(
  "W1_Watershed_Demand/Intermediate/ewrims_flat_file_use_season.csv") %>%
  semi_join(
    y = mattole_rights,
    by = "APPLICATION_NUMBER"
  ) %>%
  select(all_of(season_fields)) # Returns 959 records but 539 water rights


# SNOWFLAKE---------------------------------------------------------------------
sf_con <- DBI::dbConnect(drv = odbc::odbc(), 
                         dsn = "snowflake", 
                         server = "gb51005.west-us-2.azure.snowflakecomputing.com", 
                         Trusted_Connection = "True", 
                         authenticator = "externalbrowser",
                         database = "SWRCB_INTERNAL_PROD",
                         schema = "WR_CALWATRS_FLATFILES",
                         role = "DWR_DEV_DEMAND_DATA_FLAGS_RWC_ACROLE",
                         warehouse = "DWR_WH")

# CALWATRS TABLES----
# Whittle to Mattole dataset only to save space

## Annual Reports Flat File----
annual_reports_mattole <- dbGetQuery(sf_con, 
                        "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.annual_reports
                        where AR_ANNUALREPORTYEAR >= 2016") %>%
  inner_join(y = mattole_rights, by = c("WR_APP_CLAIM_NAME" = "APPLICATION_NUMBER"))

### Count unique water rights
n_distinct(annual_reports_mattole$WR_APP_CLAIM_NAME)


## Participants Flat File----
participants <- dbGetQuery(sf_con, 
                    "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.participants") %>%
  inner_join(y = mattole_rights, by = c("WR_APP_CLAIM_NAME" = "APPLICATION_NUMBER"))

### Count unique water rights
n_distinct(participants$WR_APP_CLAIM_NAME) # Returns 539 rights


## Geospatial relationships Flat File----
geospatial_relationships_mattole <- dbGetQuery(sf_con,
                 "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.geospatial_relationships") %>%
  inner_join(y = mattole_pods, by = c("POLY_LEGACYNAME" = "POD_ID"))

### Convert POLY_LEGACYNAME to integer
geospatial_relationships_mattole$POLY_LEGACYNAME <- as.integer(geospatial_relationships_mattole$POLY_LEGACYNAME)


### Count unique PODS, rights, and geospatial IDs
n_distinct(geospatial_relationships_mattole$GR_ID) #1183 records

n_distinct(geospatial_relationships_mattole$POLY_LEGACYNAME)
# 667 records, which matches the number of PODs in the Mattole dataset--inner joins
# only work with exact matches, so this confirms that POLY_LEGACYNAME and POD_ID are identical

n_distinct(geospatial_relationships_mattole$GR_APPLICATION_NUMBER)

### Field Comparisons----
# You have to filter to:
  # 1. Active geospatial relationships only because uniqueness is based on geospatial relationships, not POD IDs
  # 2. Points of diversion because the gr flat file also houses points of storage, points of complaint,
      # and points of investigation

colnames(geospatial_relationships_mattole) %>% sort()

## Types of Geospatial Features
geospatial_relationships_mattole$GR_FEATURE_TYPE %>%unique()


#### Latitude and Longitude
gr_subset <- geospatial_relationships_mattole %>% select(POLY_LEGACYNAME, LATITUDE, POLY_LATITUDE,
                                                         LONGITUDE, POLY_LONGITUDE, GR_STATUS,
                                                         GR_RELATIONSHIP_TYPE) %>%
  arrange(POLY_LEGACYNAME) %>%
  filter(GR_STATUS == "Active")
head(gr_subset,20)

## Data Dictionary Table----
data_dictionary <-dbGetQuery(sf_con,
                             "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.data_dictionary")

### Geospatial relationship fields
gr_fields <- data_dictionary %>% filter(TABLE_NAME == "GEOSPATIAL_RELATIONSHIPS")


## Compare Annual Reports and Participants tables
overlappingNames <- base::intersect(names(annual_reports), names(participants)) %>% sort()

## Share 16 fields 

## Compare Annual Reports and Geospatial Relationships Table
base::intersect()
## Compare Participants and Geospatial Relationships Table


# Compare POD_ID to POLY_NAME
geospatial_relationships$POLY_LEGACYNAME <- as.integer(geospatial_relationships$POLY_LEGACYNAME)
pod_comparison <- inner_join(
  x = ewrims_pod_ff_mattole,
  y = geospatial_relationships,
  by = c("POD_ID" = "POLY_LEGACYNAME"))


  









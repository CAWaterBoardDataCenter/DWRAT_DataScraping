# PURPOSE -----------------------------------------------------------------------


# Last Updated By: Payman Alemi on 3/11/2026


# This script is for testing the Calwatrs Flat Files for compliance with the SDA
# section's requirements. I want to catch any fields that we need that are missing.


# Get Data from ReportManager ----
# require(odbc)
# require(DBI)
# require(tidyverse)
# 
# report_manager <- dbConnect(odbc(),
#                            Driver = "SQL Server",
#                            Server = "reportmanager,1542",
#                            Trusted_Connection = "Yes",
#                            Database = "ReportDB")
# 
# ## Import the water_use_report_extended
# flat_file_pods <- dbGetQuery(ReportManager,
#                              "Select * from ReportDB.FLAT_FILE.ewrims_water_use_report_extended")

# Get Data from Snowflake ----

remove(list = ls())



#### Procedure ####

# Log into Snowflake
sf_con <- DBI::dbConnect(drv = odbc::odbc(), 
                         dsn = "snowflake", 
                         server = "gb51005.west-us-2.azure.snowflakecomputing.com", 
                         Trusted_Connection = "True", 
                         authenticator = "externalbrowser",
                         database = "SWRCB_INTERNAL_PROD",
                         schema = "WR_CALWATRS_FLATFILES",
                         role = "DWR_DEV_DEMAND_DATA_FLAGS_RWC_ACROLE",
                         warehouse = "WR_WH")


# Extract the annual reports table
annual_reports <- dbGetQuery(sf_con, 
                        "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.annual_reports
                        where AR_ANNUALREPORTYEAR >= 2016")

# Extract the participants table
participants <- dbGetQuery(sf_con, 
                    "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.participants")


# Extract  geospatial relationships table
gr <- dbGetQuery(sf_con,
                 "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.geospatial_relationships")

# Grab the Data Dictionary Table
data_dictionary <-dbGetQuery(sf_con,
               "Select * from swrcb_internal_prod.wr_calwatrs_flatfiles.data_dictionary")

# Log out of Snowflake
# dbDisconnect(sf_con)

# Export the data_dictionary table to a spreadsheet with a date-stamp
require(openxlsx)
write.xlsx(x = data_dictionary,
           file = "OutputData/data_dictionary.xlsx")

# Subset the fields in the snowflake tables----

ar_field_subset <- c(
  "AR_NAME", "AR_PRIMARYOWNER", "AR_ANNUALREPORTYEAR", "WR_APP_CLAIM_NAME", "WR_APP_CLAIM_STATUS",
  "WR_APP_CLAIM_TYPE", "WR_APP_CLAIM_BENEFICIALPURPOSESOFUSE", "WR_APP_FACEVALUE", "WR_APP_FACEVALUEUNIT",
  "WR_APP_CLAIM_PRIORITYDATE", "WR_APP_CLAIM_SUBTYPE", "APP_CLAIM_ACCEPTANCE_DATE", "AR_ANNUALDIVERSIONVOLUMETOTAL",
  "AR_JANSTORAGEVOLUME", "AR_ANNUALDIVERSIONVOLUMEUNIT", "AR_FEBSTORAGEVOLUME",
  "AR_APRDIVERSIONVOLUME", "AR_MARSTORAGEVOLUME", "AR_AUGDIVERSIONVOLUME", "AR_APRSTORAGEVOLUME",
  "AR_MAYSTORAGEVOLUME", "AR_DECDIVERSIONVOLUME", "AR_JUNSTORAGEVOLUME", "AR_JULSTORAGEVOLUME",
  "AR_FEBDIVERSIONVOLUME", "AR_AUGSTORAGEVOLUME", "AR_JANDIVERSIONVOLUME", "AR_SEPSTORAGEVOLUME",
  "AR_JULDIVERSIONVOLUME", "AR_OCTSTORAGEVOLUME", "AR_JUNDIVERSIONVOLUME", "AR_NOVSTORAGEVOLUME",
  "AR_MARDIVERSIONVOLUME", "AR_DECSTORAGEVOLUME", "AR_MONTHLYDATAUNIT", "AR_MONTHLYSTORAGEUNIT",
  "AR_ANNUALSTORAGEVOLUMEUNIT", "AR_NOWATERWASDIVERTED", "AR_VERSIONNUMBER", "AR_MAYDIVERSIONVOLUME",
  "AR_ANNUALSTORAGEVOLUMETOTAL", "AR_MONTHLYDIVERSIONUNIT", "AR_NOVDIVERSIONVOLUME", "AR_OCTDIVERSIONVOLUME",
  "AR_SEPDIVERSIONVOLUME", "AR_WITHDRAWNWATERFROMSTORAGE", "AR_TYPEOFDIVERSION", "WR_PERMITID",
  "CLAIM_INITIALDIVERSION"
)

ar_field_no_monthly_div_data_subset <- c(
    "AR_NAME",
    "AR_PRIMARYOWNER",
    "AR_ANNUALREPORTYEAR",
    "WR_APP_CLAIM_NAME",
    "WR_APP_CLAIM_STATUS",
    "WR_APP_CLAIM_TYPE",
    "WR_APP_FACEVALUE",
    "WR_APP_FACEVALUEUNIT",
    "WR_APP_CLAIM_PRIORITYDATE",
    "WR_APP_CLAIM_SUBTYPE",
    "APP_CLAIM_ACCEPTANCE_DATE",
    "AR_VERSIONNUMBER"
  )

### AR Subset 1: Include Monthly Diversion Data ----
annual_reports_subset <- annual_reports %>%
  select(all_of(ar_field_subset))

head(annual_reports_subset)
str(annual_reports_subset)

### AR Subset 2: Exclude Monthly Diversion Data ----
annual_reports_subset_slim <- annual_reports %>%
  select(all_of(ar_field_no_monthly_div_data_subset))

head(annual_reports_subset_slim)

# 
# # Import the latest Russian River MDT----
rr_mdt <- read.csv(file = paste0(
  "C:/Users/palemi/Water Boards/Supply and Demand Assessment - ",
  "Documents/DWRAT/SDU_Runs/Demand_Datasets/RR_2017_2024_MDT_2025-04-04.csv"
))


## Count the unique rights in RR MDT
n_distinct(rr_mdt$APPLICATION_NUMBER) #2129 water rights

## Count the unique rights in the annual_reports_subset
n_distinct(annual_reports_subset$WR_APP_CLAIM_NAME) # 44,521 water rights

### Inner Join rr_mdt to annual_reports_subset----
annual_reports_subset_rr <- inner_join(
  x = annual_reports_subset,
  y = rr_mdt,
  by = c("WR_APP_CLAIM_NAME" ="APPLICATION_NUMBER")
)

### Confirm that all the RR MDT water right IDs were carried over
n_distinct(annual_reports_subset_rr$WR_APP_CLAIM_NAME) #All 2129 water rights carry over

### Inner join rr_mdt to annual_reports_subset_slim----
annual_reports_subset_slim_rr <- inner_join(
  x = annual_reports_subset_slim,
  y = rr_mdt,
  by = c("WR_APP_CLAIM_NAME" ="APPLICATION_NUMBER")
)

# Test a small subset of Russian River rights--pick 3 rights from the major sub-types
 # Small


# Subset the WURE table for the Russian River----

## Define the WURE columns to keep
wure_cols <- c("AMOUNT", "APPLICATION_ACCEPTANCE_DATE", "APPLICATION_NUMBER",
               "APPLICATION_PRIMARY_OWNER", "APPLICATION_RECD_DATE", "DIVERSION_TYPE",
               "FACE_VALUE_AMOUNT", "FACE_VALUE_UNITS", "INI_REPORTED_DIV_AMOUNT",
               "INI_REPORTED_DIV_UNIT", "MONTH", "PARTY_ID",
               "PRIMARY_OWNER_ENTITY_TYPE", "PRIORITY_DATE", "SUB_TYPE",
               "WATER_RIGHT_TYPE", "YEAR", "YEAR_DIVERSION_COMMENCED")

require(data.table)

wure_rr <- fread(
  file = "../DWRAT_DataScraping/Demand/RawData/water_use_report_extended.csv",
  select = wure_cols
)[APPLICATION_NUMBER %in% rr_mdt$APPLICATION_NUMBER]

## Filter out all use values, just keep direct diversion and diversion to storage

## Pivot wide so that each month appears as a separate table

# Creating Phase 1 Mapping Table----
# Map the annual_reports_subset to the Russian River MDT by the application number


# 





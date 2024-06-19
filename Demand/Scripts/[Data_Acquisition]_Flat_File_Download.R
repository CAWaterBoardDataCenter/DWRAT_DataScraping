# Download flat files locally from the Report Manager database

require(tidyverse)
require(odbc)
require(DBI)



cat("Starting '[Data_Acquisition]_Flat_File_Download.R'...")



# Downloading Flat Files Required by QAQC Process----

# Two flat files are required:
# Water Use Report Extended
# eWRIMS Flat File POD



# Some data filtering was already applied in Snowflake:


# water_use_report_extended

# A subset of columns was selected:
# AMOUNT, APPLICATION_ACCEPTANCE_DATE, APPL_ID, APPLICATION_PRIMARY_OWNER,
# APPLICATION_RECD_DATE, MONTH, YEAR, DIRECT_DIV_SEASON_END, DIRECT_DIV_SEASON_START,
# DIVERSION_TYPE, EFFECTIVE_DATE, EFFECTIVE_FROM_DATE, FACE_VALUE_AMOUNT,
# FACE_VALUE_UNITS, INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT, MAX_STORAGE,
# PARTY_ID, PRIMARY_OWNER_ENTITY_TYPE, PRIORITY_DATE, SOURCE_NAME, STORAGE_SEASON_END,
# STORAGE_SEASON_START, SUB_TYPE, TRIB_DESC, USE_CODE, WATER_RIGHT_STATUS,
# WATER_RIGHT_TYPE, WATERSHED

# The data was filtered to YEAR >= 2016


# eWRIMS Flat File POD

# A subset of columns was selected:
# "APPLICATION_NUMBER", "CERTIFICATE_ID", "COUNTY", "EAST_COORD", "HUC_12_NAME",
# "HUC_12_NUMBER", "HUC_8_NAME", "HUC_8_NUMBER", "LATITUDE", "LICENSE_ID", 
# "LOCATION_METHOD", "LONGITUDE", "MERIDIAN", "NORTH_COORD", "OBJECTID", 
# "PARCEL_NUMBER", "PERMIT_ID", "POD_COUNT", "POD_ID", "POD_LAST_UPDATE_DATE", 
# "POD_NUMBER", "POD_NUMBER_GIS", "POD_STATUS", "POD_TYPE", "QUAD_MAP_NAME", 
# "QUAD_MAP_NUMBER", "QUARTER", "QUARTER_QUARTER", "RANGE_DIRECTION", 
# "RANGE_NUMBER", "SECTION_CLASSIFIER", "SECTION_NUMBER", "SOURCE_NAME", 
# "SP_ZONE", "SPECIAL_USE_AREA", "TOWNSHIP_DIRECTION", "TOWNSHIP_NUMBER", 
# "TRIB_DESC", "WATER_RIGHT_STATUS", "WATER_RIGHT_TYPE", "WATERSHED", 
# "WR_WATER_RIGHT_ID"

# The operations in "[Data_Filtering]_Initial_POD_List.R" were performed



# Connect to Snowflake
sf_con <- DBI::dbConnect(drv = odbc::odbc(), 
                         dsn = "snowflake", 
                         server = "gb51005.west-us-2.azure.snowflakecomputing.com", 
                         Trusted_Connection = "True", 
                         authenticator = "externalbrowser",
                         database = "DWR_DEV",
                         schema = "DEMAND_DATA_FLAGS",
                         role = "DWR_DEV_DEMAND_DATA_FLAGS_RWC_ACROLE",
                         warehouse = "COMPUTE_WH") 



# First send the query to Snowflake
# Select all unique rows for the water use extended report table
query <- dbSendQuery(sf_con, "SELECT DISTINCT * FROM DWR_DEV.DEMAND_DATA_FLAGS.WATER_USE_REPORT_EXTENDED_TABLE")



# Load the results into R next
res <- dbFetch(query)



# Write 'res' to a file
write_csv(res, "RawData/water_use_report_extended.csv")



# Clear the query
dbClearResult(query)



# Next, perform similar operations for the POD flat file
query <- dbSendQuery(sf_con, "SELECT DISTINCT * FROM DWR_DEV.DEMAND_DATA_FLAGS.POD_FLAT_FILE_ACTIVE")



# Load the results into R next
res <- dbFetch(query)



# Write 'res' to a file
write_csv(res, "RawData/ewrims_flat_file_pod.csv")



# Clear the query
dbClearResult(query)



# Disconnect from Snowflake
dbDisconnect(sf_con)



# Output a completion message
cat("Done!\n")



# Clean up
remove(sf_con, query, res)
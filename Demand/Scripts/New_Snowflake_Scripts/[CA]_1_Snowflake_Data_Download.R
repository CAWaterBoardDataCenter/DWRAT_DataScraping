# Download flat files to SharePoint from the Snowflake database



#### Setup ####


remove(list = ls())


require(crayon)
require(tidyverse)
require(odbc)
require(DBI)


source("Scripts/New_Snowflake_Scripts/[HELPER]_1_Shared_Functions.R")


print("Starting '[CA]_Snowflake_Data_Download.R'...")
cat("\n")


#### Procedure ####


# Downloading Flat Files Required by QAQC Process----

# Two flat files are required:
# Water Use Report Extended
# eWRIMS Flat File POD



# Some data filtering was already applied in Snowflake:


# water_use_report_extended

# None


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



cat(paste0(strwrap(paste0("NOTE", ": If you have a proper connection setup, a ", 
                          "web browser", " will open up for authenticating Snowflake.\n\n",
"If your default browser is ", "NOT", " Microsoft Edge, you will likely need to authenticate manually.\n\n"), 
width = getOption("width")), collapse = "\n") %>%
  str_replace("^NOTE", red("NOTE")) %>%
  str_replace(" NOT ", red(" NOT ")) %>%
  str_replace(" web browser ", green(" web browser ")))
cat("\n\n")


# Try to connect to Snowflake
sf_con <- try(DBI::dbConnect(drv = odbc::odbc(), 
                             dsn = "snowflake", 
                             server = "gb51005.west-us-2.azure.snowflakecomputing.com", 
                             Trusted_Connection = "True", 
                             authenticator = "externalbrowser",
                             database = "DWR_DEV",
                             schema = "DEMAND_DATA_FLAGS",
                             role = "DWR_DEV_DEMAND_DATA_FLAGS_RWC_ACROLE",
                             warehouse = "COMPUTE_WH"), silent = TRUE)



# Check if there was an error
if (is.character(sf_con)) {
  
  cat("\n")
  cat(sf_con)
  cat("\n\n")
  
  stop(paste0(strwrap(paste0("Could not connect to Snowflake", ".\n\n",
              "Please verify that you are using either the ", "VPN", " or the ", "in-office corporate network", ".\n",
              "\n",
              "If you connected to the VPN or EPA network just now, you may need to ", "close and reopen RStudio", ".\n\n",
              "Also make sure that you have the required ", "role and permissions", " to access this database ",
              "(i.e., can you access the 'DEMAND_DATA_FLAGS' schema with the 'DWR_DEV_DEMAND_DATA_FLAGS_RWC_ACROLE' ", 
              "role through your browser?)\n"), 
              width = getOption("width")), collapse = "\n") %>%
         str_replace("Could not connect to Snowflake", red("Could not connect to Snowflake")) %>%
         str_replace("VPN", green("VPN")) %>%
         str_replace("in.office( corporate)?( network)?", green("in-office\\1\\2")) %>%
         str_replace("(in.office )?corporate( network)?", green("\\1corporate\\2")) %>%
         str_replace("(in.office )?(corporate )?network", green("\\1\\2network")) %>%
         str_replace("close( and)?( reopen)?( RStudio)?", red("close\\1\\2\\3")) %>%
         str_replace("(close )?and( reopen)?( RStudio)?", red("\\1and\\2\\3")) %>%
         str_replace("(close )?(and )?reopen( RStudio)?", red("\\1\\2reopen\\3")) %>%
         str_replace("(close )?(and )?(reopen )?RStudio", red("\\1\\2\\3RStudio")) %>%
         str_replace("role( and)?( permissions)?", green("role\\1\\2")) %>%
         str_replace("(role )?(and )?permissions", green("\\1\\2permissions")))
  
}



# First send the query to Snowflake
# Select unique rows for the water use extended report table
# A subset of columns is selected
# The data is filtered to YEAR >= 2016
cat("\nFetching a subset of the 'water_use_report_extended' table...\n")



query <- dbSendQuery(sf_con, "SELECT DISTINCT 
                                APPLICATION_NUMBER, YEAR, MONTH,
                                AMOUNT,DIVERSION_TYPE, MAX_STORAGE,
                                FACE_VALUE_AMOUNT, FACE_VALUE_UNITS, 
                                INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT,
                                EFFECTIVE_DATE, EFFECTIVE_FROM_DATE,
                                WATER_RIGHT_TYPE, DIRECT_DIV_SEASON_START,
                                STORAGE_SEASON_START, DIRECT_DIV_SEASON_END, 
                                STORAGE_SEASON_END,
                                PARTY_ID, APPLICATION_PRIMARY_OWNER,
                                PRIORITY_DATE, APPLICATION_RECD_DATE, APPLICATION_ACCEPTANCE_DATE, 
                                SUB_TYPE, YEAR_DIVERSION_COMMENCED
                     FROM DWR_DEV.DEMAND_DATA_FLAGS.EWRIMS_FLAT_FILE_WATER_USE_REPORT_EXTENDED
                     WHERE CAST(YEAR AS INTEGER) >= 2016")



# Load the results into R next
res <- dbFetch(query)



# Write 'res' to a file
write_csv(res, 
          makeSharePointPath(paste0("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended/",
                                    makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended/") %>% 
                                      list.files() %>% sort() %>% tail(1) %>% str_extract("^[0-9]+") %>% {ifelse(is.na(.) || length(.) == 0, 1, as.numeric(.) + 1)} %>%
                                      formatC(digits = 3, flag = "0"),
                                    "_water_use_report_extended_", Sys.Date(), ".csv")))



# Clear the query
dbClearResult(query)



# Next, perform similar operations for the POD flat file
cat("\nFetching the 'ewrims_flat_file_pod' table...\n")



query <- dbSendQuery(sf_con, "SELECT DISTINCT * FROM DWR_DEV.DEMAND_DATA_FLAGS.EWRIMS_FLAT_FILE_POD")



# Load the results into R next
res <- dbFetch(query)



# Write 'res' to a file
write_csv(res, 
          makeSharePointPath(paste0("Program Watersheds/7. Snowflake Demand Data Downloads/eWRIMS Flat File POD/",
                                    makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/eWRIMS Flat File POD/") %>% 
                                      list.files() %>% sort() %>% tail(1) %>% str_extract("^[0-9]+") %>% {ifelse(is.na(.) || length(.) == 0, 1, as.numeric(.) + 1)} %>%
                                      formatC(digits = 3, flag = "0"),
                                    "_ewrims_flat_file_pod_", Sys.Date(), ".csv")))



# Clear the query
dbClearResult(query)



# Disconnect from Snowflake
dbDisconnect(sf_con)



# Output a completion message
cat("\n\n")
print("The script is complete!")



# Clean up
remove(list = ls())


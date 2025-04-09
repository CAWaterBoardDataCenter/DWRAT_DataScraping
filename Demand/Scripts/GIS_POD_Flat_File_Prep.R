# Run the scripts one chunk at a time to insure that everything is working correctly.
#Install if you do not have in your current packages or are not up to date.----
#install.packages("tidyverse")

#Load Packages- This step must be done each time the project is opened. ----
require(tidyverse)
require(odbc)
require(DBI)

# Repair function for corrupted flat files----

# Jeff Yeazell and Damon Hess discovered in February 2024 that the flat files had been corrupted;
# The October, November, and December 2021 reported values are being assigned to their respective 
# months in 2022, and the 'missing ' 2021 values are then imputed as zeros. If diverters reported 
# for water year 2023, the October, November, and December 2022 reported values are being assigned 
# to their respective months in 2023. It looks like some sort of joining error where the year 
# is not included with the month. It affects every water right and possibly every analysis 
# performed on Water Year 2022 data or later where the error went unnoticed.

# REMEDIATION CODING BLOCK

fixData <- function(x) {
  
  # WY 2022 data:
  # Extract Oct, Nov, and Dec 2022 rows and rewind to 2021.
  t1 <- x %>% 
    filter(YEAR == 2022,
           MONTH %in% c(10:12)) %>% 
    mutate(YEAR = 2021)
  
  # Cut offending Oct, Nov, Dec rows for 2021 and 2022.
  x <- x %>% 
    filter(!(YEAR %in% c(2021, 2022) & MONTH %in% c(10:12)))
  
  # Bind corrected Oct, Nov, Dec 2021 rows.
  x <- bind_rows(x, t1)
  
  # WY 2023 data:
  # Extract Oct, Nov, and Dec 2023 rows and rewind to 2022.
  t2 <- x %>% 
    filter(YEAR == 2023,
           MONTH %in% c(10:12)) %>% 
    mutate(YEAR = 2022)
  
  # Cut offending Oct, Nov, Dec rows for 2022 and 2023.  
  x <- x %>% 
    filter(!(YEAR %in% c(2022, 2023) & MONTH %in% c(10:12)))
  
  # Bind corrected Oct, Nov, Dec 2022 rows.
  x <- bind_rows(x, t2) %>% 
    arrange(APPL_ID, YEAR, MONTH)
  
  
  
  # WY 2024 data:
  # Extract Oct, Nov, and Dec 2024 rows and rewind to 2023.
  t3 <- x %>% 
    filter(YEAR == 2024,
           MONTH %in% c(10:12)) %>% 
    mutate(YEAR = 2023)
  
  # Cut offending Oct, Nov, Dec rows for 2023 and 2024.  
  x <- x %>% 
    filter(!(YEAR %in% c(2023, 2024) & MONTH %in% c(10:12)))
  
  # Bind corrected Oct, Nov, Dec 2023 rows.
  x <- bind_rows(x, t3) %>% 
    arrange(APPL_ID, YEAR, MONTH)
  
  
  
  if (max(as.numeric(x$YEAR), na.rm = TRUE) > 2024) {
    
    
    for (i in 2025:max(as.numeric(x$YEAR))) {
      
      # WY 2025 onwards data:
      # Extract Oct, Nov, and Dec from each year and rewind to the prior year
      tx <- x %>% 
        filter(YEAR == i,
               MONTH %in% c(10:12)) %>% 
        mutate(YEAR = i - 1)
      
      
      
      # Cut offending Oct, Nov, Dec rows for both years.  
      x <- x %>% 
        filter(!(YEAR %in% c(i - 1, i) & MONTH %in% c(10:12)))
      
      
      
      # Bind corrected Oct, Nov, Dec rows.
      x <- bind_rows(x, tx) %>% 
        arrange(APPL_ID, YEAR, MONTH)
      
    }
    
  }
  
  
  # Return result.
  return(x)
  
}

# Downloading Flat Files Required by QAQC Process----

# Download in advance all flat files that will be used in the procedures of this script and the other demand-related scripts
# They will be downloaded directly from the ReportManager, 1542 SQL Server, Database ReportDB, which hosts all eWRIMS flat files. 
# This database is accessible to all Division staff. (VPN Required)
ReportManager <- dbConnect(odbc(),
                           Driver = "SQL Server",
                           Server = "reportmanager,1542",
                           Trusted_Connection = "Yes",
                           Database = "ReportDB")


# Save the POD flat file, ~73 MB as of 2/13/2024
Flat_File_PODs <- dbGetQuery(ReportManager,
                             "Select * from ReportDB.FLAT_FILE.ewrims_flat_file_pod") %>%
  write_csv("RawData/ewrims_flat_file_pod.csv")


# Get the master flat file as well, ~69 MB as of 2/13/2024
flat_file <- dbGetQuery(conn = ReportManager,
           statement = "Select * from ReportDB.FLAT_FILE.ewrims_flat_file") %>% 
  write_csv("RawData/ewrims_flat_file.csv")


# Download the Water Rights Annual Water Use Report file next, ~389 MB as of 2/13/2024
water_use_report <- dbGetQuery(conn = ReportManager,
           statement = "SELECT * from ReportDB.FLAT_FILE.ewrims_water_use_report
           WHERE YEAR >= 2016 ")

# Convert the YEAR  column to numeric
water_use_report$YEAR = as.numeric(water_use_report$YEAR)
  
  
if (water_use_report %>% 
    filter(YEAR == max(water_use_report$YEAR) & as.numeric(MONTH) > 9) %>%
    nrow() > 0) {
  
  # Apply the fixData function to water_use_report
  water_use_report_repaired <- fixData(water_use_report)
  
  write_csv(water_use_report_repaired,"RawData/water_use_report.csv")
  
} else {
  
  write_csv(water_use_report,"RawData/water_use_report.csv")
  
}

# DATA ACQUISITION CODING BLOCK

# Save the Water Rights Annual Water Use Extended Report file too, ~1.6 GB as of 2/13/2024
# (This works, but it takes a long time, and the progress bar might not update)
water_use_report_extended = dbGetQuery(conn = ReportManager,
           statement = "Select
                                                  APPLICATION_NUMBER,YEAR,MONTH,
                                                  AMOUNT,DIVERSION_TYPE, MAX_STORAGE,
                                                  FACE_VALUE_AMOUNT, FACE_VALUE_UNITS, 
                                                  INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT,
                                                  EFFECTIVE_DATE, EFFECTIVE_FROM_DATE,
                                                  WATER_RIGHT_TYPE, DIRECT_DIV_SEASON_START,
                                                  STORAGE_SEASON_START, DIRECT_DIV_SEASON_END, 
                                                  STORAGE_SEASON_END, 
                                                  PARTY_ID, APPLICATION_PRIMARY_OWNER,
                                                  PRIORITY_DATE, APPLICATION_RECD_DATE, APPLICATION_ACCEPTANCE_DATE, 
                                                  SUB_TYPE, YEAR_DIVERSION_COMMENCED,
                                                  USE_CODE
                                                  FROM ReportDB.FLAT_FILE.ewrims_water_use_report_extended
                                                  WHERE YEAR >= 2016
                                                  ")

if (water_use_report_extended %>% 
    filter(YEAR == max(as.numeric(water_use_report_extended$YEAR)) & as.numeric(MONTH) > 9) %>%
    nrow() > 0) {
  
  
  # Rename APPLICATION_NUMBER to APPL_ID so that it's compatible with the fixData function
  water_use_report_extended <- water_use_report_extended %>% 
    rename(APPL_ID = APPLICATION_NUMBER)
  
  # Change YEAR to numeric data type
  water_use_report_extended$YEAR = as.numeric(water_use_report_extended$YEAR)
  
  # Apply the fixData function to water_use_report_extended
  water_use_report_extended_repaired = fixData(water_use_report_extended)
  
  # Revert APPL_ID to APPLICATION_NUMBER
  water_use_report_extended_repaired <- water_use_report_extended_repaired %>% 
    rename(APPLICATION_NUMBER = APPL_ID)
  
  #Export water_use_report_extended_repaired to CSV
  write_csv(x = water_use_report_extended_repaired, file = "RawData/water_use_report_extended.csv")
  
} else {
  
  write_csv(x = water_use_report_extended, file = "RawData/water_use_report_extended.csv")
  
}

  
# Save the Water Rights Uses and Seasons flat file as well, ~96 MB
ewrims_flat_file_use_season <- dbGetQuery(conn = ReportManager, 
           statement = "Select * from 
                                          ReportDB.FLAT_FILE.ewrims_flat_file_use_season") %>% 
  write_csv("RawData/ewrims_flat_file_use_season.csv")


# Get the Water Rights Parties flat file after that
# (It is also a big file that would work better with read_csv() instead of download.file()) ~174 MB
ewrims_flat_file_party <- dbGetQuery(conn = ReportManager,
           statement = "Select * from ReportDB.FLAT_FILE.ewrims_flat_file_party") %>%
  write_csv("RawData/ewrims_flat_file_party.csv")


# Read the POD flat file
# (This is already assigned earlier at the SQL query step)
#Flat_File_PODs <- read.csv("RawData/ewrims_flat_file_pod.csv")


# Disconnect from ReportDB Database
dbDisconnect(conn = ReportManager)


#Apply the proper filters----

##Filter for Active PODs----
Flat_File_PODs_Status <- Flat_File_PODs[Flat_File_PODs$POD_STATUS == "Active", ]

##Get unique water right types----
# WR_Types <- Flat_File_PODs$WATER_RIGHT_TYPE %>% 
#   unique() %>% #extracts unique values
#   trimws() %>% #removes leading and lagging spaces
#   sort() %>%  #sorts alphabetically
#   data.frame() %>% #converts the dataset into a dataframe
#   colnames = c("WR_Types") #Assigns the column name "WR_Types"

# WR_Types #print values

#Water Right Types we ignore:
# Adjudicated
# Appropriative (State Filing)
# Cert of Right - Power
# Groundwater Recordation
# Non Jurisdictional
# Not Determined
# Section 12 File
# Temporary Permit
# Waste Water Change

##Filter by Water Right Type----
Flat_File_PODs_WR_Type <- Flat_File_PODs_Status[Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Appropriative" | 
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Federal Claims" | 
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Federal Stockponds" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Registration Cannabis" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Registration Domestic" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Registration Irrigation" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Registration Livestock" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Statement of Div and Use" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Stockpond" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "",]
##Get List of Water Right Statuses---- 
# WR_Statuses <- Flat_File_PODs$WATER_RIGHT_STATUS %>%
#   unique() %>% #Extracts unique values
#   trimws() %>% #Removes leading and lagging spaces
#   sort() %>%  #sorts alphabetically
#   data.frame() %>% #converts the dataset into a dataframe
#   colnames = c("WR_Statuses") #Assigns the column name "WR_Statuses"

#WR_Statuses #print values

##Filter by Water Right Status----
Flat_File_eWRIMS<- Flat_File_PODs_WR_Type[Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Active" | 
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Certified" | 
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Claimed - Local Oversight"|
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Claimed"|
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Completed"|
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Licensed"|
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Permitted"|
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Registered"|
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "" , ] 

##Remove unnecessary columns from Flat File----
#GIS pre-processing steps require you to keep only these 43 columns:
cols_to_keep <- c("APPLICATION_NUMBER", "CERTIFICATE_ID", "COUNTY", "EAST_COORD", "HUC_12_NAME", "HUC_12_NUMBER",
                  "HUC_8_NAME", "HUC_8_NUMBER", "LATITUDE", "LICENSE_ID", "LOCATION_METHOD", "LONGITUDE", "MERIDIAN", "NORTH_COORD",
                  "OBJECTID", "PARCEL_NUMBER", "PERMIT_ID", "POD_COUNT", "POD_ID", "POD_LAST_UPDATE_DATE", "POD_NUMBER",
                  "POD_NUMBER_GIS", "POD_STATUS", "POD_TYPE", "QUAD_MAP_NAME", "QUAD_MAP_NUMBER", "QUARTER", "QUARTER_QUARTER",
                  "RANGE_DIRECTION", "RANGE_NUMBER", "SECTION_CLASSIFIER", "SECTION_NUMBER", "SOURCE_NAME", "SP_ZONE",
                  "SPECIAL_USE_AREA", "TOWNSHIP_DIRECTION", "TOWNSHIP_NUMBER", "TRIB_DESC", "WATER_RIGHT_STATUS",
                  "WATER_RIGHT_TYPE", "WATERSHED", "WR_WATER_RIGHT_ID")

Flat_File_eWRIMS <- Flat_File_eWRIMS[, cols_to_keep, drop = FALSE]

#Replace Meridian Names with Meridian Short Names----
Flat_File_eWRIMS <- Flat_File_eWRIMS %>%
  mutate(MERIDIAN = case_when(
    MERIDIAN == " San Bernardino" ~ "SBM",
    MERIDIAN == "Mount Diablo" ~ "MDM",
    MERIDIAN == "Humboldt" ~ "HM",
    TRUE ~ MERIDIAN
  ))

#Add the FFMTRS field----
  #This field serves as the Flat File Mountain Township Range Section field
  #This field concatenates the Meridian, Township Number, Township Direction, Range Number, Range Direction, and Section Number fields
  #This field is used as a basis of comparison with the MTRS field in the PLSS_Sections_Fill shapefile

Flat_File_eWRIMS$FFMTRS = paste0(Flat_File_eWRIMS$MERIDIAN, 
                                 "-T",
                                 if_else(is.na(Flat_File_eWRIMS$TOWNSHIP_NUMBER),
                                         NA_character_,
                                         if_else(Flat_File_eWRIMS$TOWNSHIP_NUMBER < 10, 
                                                 paste0("0", Flat_File_eWRIMS$TOWNSHIP_NUMBER),
                                                 as.character(Flat_File_eWRIMS$TOWNSHIP_NUMBER))),
                                 Flat_File_eWRIMS$TOWNSHIP_DIRECTION, 
                                 "-R",
                                 if_else(is.na(Flat_File_eWRIMS$RANGE_NUMBER),
                                         NA_character_,
                                         if_else(Flat_File_eWRIMS$RANGE_NUMBER < 10, 
                                                 paste0("0", Flat_File_eWRIMS$RANGE_NUMBER),
                                                 as.character(Flat_File_eWRIMS$RANGE_NUMBER))),
                                 Flat_File_eWRIMS$RANGE_DIRECTION, 
                                 "-",
                                 Flat_File_eWRIMS$SECTION_NUMBER)


#### NOTE FOR LATER ####
# THERE ARE ENTRIES IN THE FLAT FILE WITH "NA" MERIDIAN, BUT OTHERWISE VALID PLSS INFORMATION
# THESE COULD BE FIXED WITH A ONE-TIME MANUAL REVIEW



#Convert Coordinate Fields From Character Format to Numeric Format----
Flat_File_eWRIMS <- Flat_File_eWRIMS %>%
  mutate_at(.vars = vars(LATITUDE, LONGITUDE), .funs = as.numeric)
#######################################USE THIS FILE FOR THE GIS STEP##########################################################################################################################################################################
####Check your output file
write_csv(Flat_File_eWRIMS,
          paste0("IntermediateData/Flat_File_eWRIMS_", Sys.Date() - 1, ".csv"))



# Clear the environment----
  # Get the name of all variables in the environment
remove(list = ls())

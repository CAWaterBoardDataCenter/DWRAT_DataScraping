# Run the scripts one chunk at a time to insure that everything is working correctly. When you become more familiar with the code you can run in larger sections. 
#Install if you do not have in your current packages or are not up to date.----
# install.packages("tidyverse")
# install.packages("readxl")
#Load Packages- This step must be done each time the project is opened. ----
require(tidyverse)
require(readxl)
require(data.table)


cat("Starting 'Priority_Date_Postprocessing.R'...\n")


source("Scripts/Watershed_Selection.R")
source("Scripts/Dataset_Year_Range.R")


######################################################################## Break ####################################################################################

# Each of the spreadsheets that use the water use report need different filters so only the date is filtered here 

# FLAGGING BLOCK----

# Read in the (very large) water use report extended flat file
# Import only certain columns
# Also, restrict the years included in the dataset
water_use_report <- fread(file = "RawData/water_use_report_extended.csv", 
                         select = c("APPLICATION_NUMBER","YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE")) %>% 
  unique()



# Perform an inner join (it is a one-to-many relationship) of Application_Number to water_use_report; this whittles 
# down the dataset to just the application_numbers in your watershed of interest
water_use_report_Combined <- inner_join(Application_Number, water_use_report, by = "APPLICATION_NUMBER",
                                       relationship = "one-to-many")



# Remove all data from before 2017 (Decision on 8/2/2023 because of "Combined" use type)
# (It was formerly 2014 because that was when the data structure changed in the system)
water_use_report_Date <- water_use_report_Combined %>%
  filter(YEAR >= yearRange[1]) %>%
  filter(YEAR <= yearRange[2]) %>%
  filter(!(YEAR %in% 2021:2022))

print("Data from 2021 and 2022 will be excluded!")
#Added to generate a 2017-2020 dataset on 4/22/2024, 
  #2021 and 2022 were heavily curtailed years; Payman Alemi added 2023 data but still
  #excluded 2021-2022 data on 12/16/2024



# If 'yearRange[2]' is 2022 or later, the most recent reports use water years
# In that case, filter out October - December of the last year in 'water_use_report_Date'
# Those three months are part of the next water year, which is not in this dataset
if (yearRange[2] >= 2022) {
  
  water_use_report_Date <- water_use_report_Date %>%
    filter(!(YEAR == yearRange[2] & MONTH %in% 10:12))
  
}



# SQLite Approach
# conn <- dbConnect(dbDriver("SQLite"), "RawData/water_use_report_extended_subset.sqlite")
# water_use_report <- dbGetQuery(conn, 
#                                paste0('SELECT DISTINCT ',
#                                       '"APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE" ',
#                                       'FROM "Table" ',
#                                       'WHERE "YEAR" BETWEEN ', 2017, ' AND ', 2022)) 
#                                       # ADJUST THESE NUMBERS TO CHANGE THE YEARS INCLUDED IN THE DEMAND DATASET
# dbDisconnect(conn)


# All data from before 2017 is removed (Decision on 8/2/2023 because of "Combined" use type)
# (It was formerly 2014 because that was when the data structure changed in the system)

# The dataset can be restricted to 2020 (Added to generate a 2017-2020 dataset on 10/17/2023)
# 2021 and 2022 were heavily curtailed years

# Perform an inner join (it is a one-to-many relationship)
# (This is only used alongside the SQLite approach)
# water_use_report_Date <- inner_join(Application_Number, water_use_report, by = "APPLICATION_NUMBER",
#                                     relationship = "one-to-many")


# Import functions for updates to the dataset

# REMEDIATION BLOCK----
# QA/QC functions for correcting unit conversion errors and duplicate reporting
source("Scripts/QAQC_Functions.R")


# A function to update reported amounts for new rights
source("Scripts/Face_Value_Substitution.R")



# Using the function defined in "Scripts/QAQC_Unit_Fixer_Function.R",
# correct entries in 'water_use_report_Date' for unit conversion errors
water_use_report_Date <- water_use_report_Date %>%
  unitFixer(ws)



# After that, apply corrections for duplicate reporting
water_use_report_Date <- water_use_report_Date %>%
  dupReportingFixer(ws)



# Similarly, for relatively new appropriative water rights,
# if they report no values for a year, replace their data so that
# the total AMOUNT for that year equals their Face Value
water_use_report_Date <- water_use_report_Date %>%
  faceValSub(yearRange = (year(Sys.Date()) - 2):year(Sys.Date()))



# Output the data to a CSV file
write.csv(water_use_report_Date,
          paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_water_use_report_DATE", ".csv"), row.names = FALSE)

# Remove variables from the environment that will no longer be used (free up memory)
remove(water_use_report, water_use_report_Date, unitFixer, water_use_report_Combined,
       chooseUseType, iterateQAQC, useMeasurementData, dupReportingFixer, removeDups,
       faceValSub, faceValExtract, faceValAssign, monthExtract,
       applyConversionFactor)#, conn)

######################################################################## Break ####################################################################################

# FLAGGING BLOCK ----
# Read the use season flat file next
ewrims_flat_file_use_season <- read.csv("RawData/ewrims_flat_file_use_season.csv")


# Perform another inner join
ewrims_flat_file_use_season_Combined <- inner_join(Application_Number, ewrims_flat_file_use_season, by = "APPLICATION_NUMBER", 
                                                   relationship = "one-to-many")


# Remove rows where "APPLICATION_NUMBER" starts with "S" (statements of diversion and use)
# ewrims_flat_file_use_season_Combined <- ewrims_flat_file_use_season_Combined %>%
#   filter(!grepl("^S", APPLICATION_NUMBER)) 


# Filter by use status next
ewrims_flat_file_use_season_Combined_USE_STATUS <- ewrims_flat_file_use_season_Combined %>%
  filter(is.na(USE_STATUS) |
           USE_STATUS %in% c("Added by change order", "Added by correction order",
                             "Added under section 798 of Regs", "Migrated from old WRIMS data",
                             "Requested when filed", ""))


# Perform additional filters for collection season status
ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS <- ewrims_flat_file_use_season_Combined_USE_STATUS %>%
  filter(is.na(COLLECTION_SEASON_STATUS_1) | is.na(COLLECTION_SEASON_STATUS_2) | is.na(COLLECTION_SEASON_STATUS_3) |
           COLLECTION_SEASON_STATUS_1 %in% c("Migrated from old WRIMS data", "Reduced by order",
                                             "Reduced when licensed", "Requested when filed", "") |
           COLLECTION_SEASON_STATUS_2 %in% c("Migrated from old WRIMS data", "Reduced by order",
                                             "Reduced when licensed", "Requested when filed", "") |
           COLLECTION_SEASON_STATUS_3
         %in% c("Migrated from old WRIMS data", "Reduced by order",
                "Reduced when licensed", "Requested when filed", ""))

################################################################# DIRECT_DIV_SEASON_STATUS ########################################################################

# Filter 'ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS' further
# This time, check the three different status columns
ewrims_flat_file_use_season_Combined_DIRECT_DIV_SEASON_STATUS <- ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS %>%
  filter(is.na(DIRECT_DIV_SEASON_STATUS_1) | is.na(DIRECT_DIV_SEASON_STATUS_2) | is.na(DIRECT_DIV_SEASON_STATUS_3) |
           DIRECT_DIV_SEASON_STATUS_1 %in% c("Migrated from old WRIMS data", "Reduced by order",
                                             "Reduced when licensed", "Requested when filed", "") |
           DIRECT_DIV_SEASON_STATUS_2 %in% c("Migrated from old WRIMS data", "Reduced by order",
                                             "Reduced when licensed", "Requested when filed", "") |
           DIRECT_DIV_SEASON_STATUS_3 %in% c("Migrated from old WRIMS data", "Reduced by order",
                                             "Reduced when licensed", "Requested when filed", ""))


# Write the output to a file
write.csv(ewrims_flat_file_use_season_Combined_DIRECT_DIV_SEASON_STATUS,
          paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_ewrims_flat_file_use_season_WITH_FILTERS.csv"), row.names = FALSE)

# Remove unnecessary variables again to save memory
remove(ewrims_flat_file_use_season, ewrims_flat_file_use_season_Combined,
       ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS,
       ewrims_flat_file_use_season_Combined_DIRECT_DIV_SEASON_STATUS,
       ewrims_flat_file_use_season_Combined_USE_STATUS)


################################################################### Beneficial Use and Return Flow ############################################################

# Prepare the input file for the beneficial use module next

# Read in the CSV
Beneficial_Use_and_Return_Flow <- read.csv(paste0("IntermediateData/", ws$ID, 
                                                  "_", yearRange[1], "_", yearRange[2], 
                                                  "_ewrims_flat_file_use_season_WITH_FILTERS.csv"))


# Keep a subset of the columns
Beneficial_Use_and_Return_Flow_FINAL <- Beneficial_Use_and_Return_Flow %>%
  select(APPLICATION_NUMBER, USE_CODE, WATER_RIGHT_TYPE, FACE_VALUE_AMOUNT,
         INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT, APPLICATION_PRIMARY_OWNER,
         PRIMARY_OWNER_ENTITY_TYPE) %>%
  unique()


####Output the variable to a file
write.csv(Beneficial_Use_and_Return_Flow_FINAL,
          paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Beneficial_Use_and_Return_Flow_FINAL.csv"), row.names = FALSE)



###################################################################Statistics############################################################

# Get statistical data next

# Read in a CSV 
Statistics <- read.csv(paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_water_use_report_DATE.csv"))


# Keep a subset of the columns
Statistics_FINAL  <- Statistics %>%
  select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE)


# Output the data
write.csv(Statistics_FINAL,
          paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Statistics_FINAL.csv"), row.names = FALSE)


# Read in another CSV next 
Statistics_FaceValue_IniDiv <- read.csv(paste0("IntermediateData/", ws$ID, "_ewrims_flat_file_WITH_FILTERS.csv"))


# Remove most variables from the data frame
Statistics_FaceValue_IniDiv_Final  <- Statistics_FaceValue_IniDiv %>%
  select(APPLICATION_NUMBER, INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT,
         FACE_VALUE_AMOUNT, FACE_VALUE_UNITS)


# Output results to a file structure
write.csv(Statistics_FaceValue_IniDiv_Final,
          paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Statistics_FaceValue_IniDiv_Final.csv"), row.names = FALSE)


################################################################### Diversion out of Season Part A ############################################################

# Write a CSV file for the first Diversion out of Season module

# Read in the use season flat file
Diversion_out_of_Season_Part_A <- read.csv(paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_ewrims_flat_file_use_season_WITH_FILTERS.csv"))


# Extract a portion of the table
Diversion_out_of_Season_Part_A_FINAL <- Diversion_out_of_Season_Part_A %>%
  select(APPLICATION_NUMBER, USE_STATUS, DIRECT_SEASON_START_MONTH_1, DIRECT_SEASON_START_MONTH_2,
         DIRECT_DIV_SEASON_END_MONTH_1, DIRECT_DIV_SEASON_END_MONTH_2, STORAGE_SEASON_START_MONTH_1,
         STORAGE_SEASON_START_MONTH_2, STORAGE_SEASON_END_MONTH_1, STORAGE_SEASON_END_MONTH_2) %>%
  unique()


# Output the data to a file
write.csv(Diversion_out_of_Season_Part_A_FINAL,
          paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Diversion_out_of_Season_Part_A_FINAL.csv"), row.names = FALSE)


###################################################################Diversion out of Season Part B############################################################

# Write a CSV file for the second Diversion out of Season module

# Read in a flat file
Diversion_out_of_Season_Part_B <- read.csv(paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_water_use_report_DATE.csv"))


# Filter down the table to remove application numbers that start with "S" (statements of diversion and use)
# Also, keep only entries that have "DIRECT" or "STORAGE" as a diversion type
Diversion_out_of_Season_Part_B_N <- Diversion_out_of_Season_Part_B %>%
  filter(!grepl("^S", APPLICATION_NUMBER)) %>%
  filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE"))


# Extract a subset of the columns after that
Diversion_out_of_Season_Part_B_FINAL <- Diversion_out_of_Season_Part_B_N %>%
  select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE) %>%
  unique()


# Output a CSV file
write.csv(Diversion_out_of_Season_Part_B_FINAL,
          paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Diversion_out_of_Season_Part_B_FINAL.csv"), row.names = FALSE)


# Remove unnecessary variables at this step to free up memory
remove(Beneficial_Use_and_Return_Flow, Beneficial_Use_and_Return_Flow_FINAL,
       Diversion_out_of_Season_Part_A, Diversion_out_of_Season_Part_A_FINAL,
       Diversion_out_of_Season_Part_B_N, Diversion_out_of_Season_Part_B,
       Diversion_out_of_Season_Part_B_FINAL, 
       Statistics, Statistics_FaceValue_IniDiv, Statistics_FaceValue_IniDiv_Final,
       Statistics_FINAL)


###################################################################Missing RMS Reports############################################################

# Prepare the input file for the missing RMS reports module


# Read in a flat file CSV
Missing_RMS_Reports <- read.csv(paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_water_use_report_DATE.csv")) %>%
  unique()


# Read in the results from the Priority Date module
Priority_Date <- read_xlsx(paste0("OutputData/", ws$ID, "_Priority_Date_Scripted.xlsx"), col_types = "text") %>%
  select(APPLICATION_NUMBER, ASSIGNED_PRIORITY_DATE, PRE_1914, RIPARIAN, APPROPRIATIVE, APPROPRIATIVE_DATE_SOURCE, STATEMENT_PRIORITY_SOURCE)


# Perform an inner join, merging the application list to reduce data
# ("one-to-many" is the relationship because rights have separate rows for each month and year in 'Missing_RMS_Reports')
Missing_RMS_Reports_Priority_Date_Combined <- Priority_Date %>%
  inner_join(Missing_RMS_Reports, by = "APPLICATION_NUMBER",
             relationship = "one-to-many")


# Keep only a subset of the columns
# Also, filter the data based on diversion type
Missing_RMS_Reports_FINAL <- Missing_RMS_Reports_Priority_Date_Combined %>%
  select(APPLICATION_NUMBER,YEAR,MONTH,AMOUNT,DIVERSION_TYPE,ASSIGNED_PRIORITY_DATE) %>%
  filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE"))


# Output the data
write.csv(Missing_RMS_Reports_FINAL,
          paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Missing_RMS_Reports_FINAL.csv"), row.names = FALSE)


######################################## QAQC Working Files###################################

####################Application Numbers############################

# Use "RR_pod_points_Merge_filtered_PA_[DATE].xlsx", extract two columns, update the spreadsheet as needed, depends on the result of your GIS pre-processing review for your watershed
# Application_Number <- read_xlsx("InputData/RR_pod_points_Merge_filtered_PA_2023-09-19.xlsx") %>%
#   group_by(APPLICATION_NUMBER, POD_ID) %>%
#   summarize(FREQUENCY = n(), .groups = "drop") %>%
#   select(APPLICATION_NUMBER, FREQUENCY) %>%
#   unique()


# Read in the eWRIMS Flat File
ewrims_flat_file <- read.csv("RawData/ewrims_flat_file.csv") %>%
  select(APPLICATION_NUMBER, WATER_RIGHT_TYPE, WATER_RIGHT_STATUS, 
         PRIMARY_OWNER_ENTITY_TYPE, APPLICATION_PRIMARY_OWNER, SOURCE_NAME,
         TRIB_DESC, WATERSHED) %>%
  unique()


# Perform a left join (keeping the rows from 'Application_Number', even if there is no match)
# The relationship is "one-to-one"
ewrims_flat_file_one <- Application_Number %>%
  left_join(ewrims_flat_file, by = "APPLICATION_NUMBER", relationship = "one-to-one")


# Remove rows with the same value for "APPLICATION_NUMBER" (only the first instance is preserved)
ewrims_flat_file_Three <- ewrims_flat_file_one[!duplicated(ewrims_flat_file_one$APPLICATION_NUMBER), ]


# Select a subset of columns
ewrims_flat_file_Working_File <- ewrims_flat_file_Three %>%
  select(APPLICATION_NUMBER, WATER_RIGHT_TYPE, WATER_RIGHT_STATUS,
         PRIMARY_OWNER_ENTITY_TYPE, APPLICATION_PRIMARY_OWNER, SOURCE_NAME, 
         TRIB_DESC, WATERSHED)


# Output data to a file structure
write.csv(ewrims_flat_file_Working_File,
          paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_ewrims_flat_file_Working_File.csv"), row.names = FALSE)


####################################################Contact Information#################################################

# Load in the party flat file for contact information
# Keep only a subset of the columns
#ewrims_flat_file_party <- read.csv(paste0("RawData/", ws$ID, "_ewrims_flat_file_party.csv")) %>%
#  select(APPLICATION_ID, CONTACT_INFORMATION_PHONE, CONTACT_INFORMATION_EMAIL)


# Remove entries with duplicate application IDs (only the first instance of each ID will remain) 
#ewrims_flat_file_party_APPLICATION_ID <- ewrims_flat_file_party[!duplicated(ewrims_flat_file_party$APPLICATION_ID), ]


# Get a subset with blank phone numbers 
#Phone <- ewrims_flat_file_party_APPLICATION_ID %>%
#  filter(CONTACT_INFORMATION_PHONE == "")


# Get a subset with blank emails
#Email <- ewrims_flat_file_party_APPLICATION_ID %>%
#  filter(CONTACT_INFORMATION_EMAIL == "")


# Get a subset with 999-999-9999 for phone numbers
#Nine <- ewrims_flat_file_party_APPLICATION_ID %>%
#  filter(CONTACT_INFORMATION_PHONE == "999-999-9999")


# Combine these three data frames
#ewrims_flat_file_party_Final <- rbind(Phone, Nine, Email)


# Finally, remove all variables from the workspace
remove(ewrims_flat_file, ewrims_flat_file_one, #ewrims_flat_file_party,
       #ewrims_flat_file_party_APPLICATION_ID, ewrims_flat_file_party_Final,
       ewrims_flat_file_Three, ewrims_flat_file_Working_File, #Nine, Phone, Email,
       Application_Number, Priority_Date, Missing_RMS_Reports, Missing_RMS_Reports_FINAL,
       Missing_RMS_Reports_Priority_Date_Combined)


cat("Done!\n")
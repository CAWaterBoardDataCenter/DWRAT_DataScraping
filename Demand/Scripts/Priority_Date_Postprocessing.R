# Run the scripts one chunk at a time to insure that everything is working correctly. When you become more familiar with the code you can run in larger sections. 
#Install if you do not have in your current packages or are not up to date.----
# install.packages("tidyverse")
# install.packages("readxl")
#Load Packages- This step must be done each time the project is opened. ----
library(tidyverse)
library(readxl)



######################################################################## Break ####################################################################################

# Each of the spreadsheets that use the water use report need different filters so only the date is filtered here 

# Read in the (very large) water use report extended flat file
#Import only the selected_columns of the water_use_report_extended.csv
water_use_report <- fread(file = "RawData/water_use_report_extended.csv", 
                          select = c("APPLICATION_NUMBER","YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE")) %>% unique()


# Perform an inner join (it is a one-to-many relationship)
water_use_report_Combined <- inner_join(Application_Number, water_use_report, by = "APPLICATION_NUMBER",
                                        relationship = "one-to-many")


# Remove all data from before 2017 (Decision on 8/2/2023 because of "Combined" use type)
# (It was formerly 2014 because that was when the data structure changed in the system)
water_use_report_Date <- water_use_report_Combined %>%
  filter(YEAR >= 2017)


# Using the function defined in "Scripts/QAQC_Unit_Fixer_Function.R",
# correct entries in 'water_use_report_Date' for unit conversion errors
water_use_report_Date <- water_use_report_Date %>%
  unitFixer()


# After that, apply corrections for duplicate reporting
water_use_report_Date <- water_use_report_Date %>%
  dupReportingFixer()


# Output the data to a CSV file
write.csv(water_use_report_Date,"IntermediateData/water_use_report_DATE.csv", row.names = FALSE)

# Remove variables from the environment that will no longer be used (free up memory)
remove(water_use_report, water_use_report_Combined, water_use_report_Date, unitFixer, chooseUseType, iterateQAQC, useMeasurementData)

######################################################################## Break ####################################################################################

# Read the use season flat file next
ewrims_flat_file_use_season <- read.csv("RawData/ewrims_flat_file_use_season.csv")

# Perform another inner join
ewrims_flat_file_use_season_Combined <- inner_join(Application_Number, ewrims_flat_file_use_season, by = "APPLICATION_NUMBER", 
                                                   relationship = "one-to-many")

# Remove rows where "APPLICATION_NUMBER" starts with "S" (statements of diversion and use)
ewrims_flat_file_use_season_Combined <- ewrims_flat_file_use_season_Combined %>%
  filter(!grepl("^S", APPLICATION_NUMBER)) 

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
          "IntermediateData/ewrims_flat_file_use_season_WITH_FILTERS.csv", row.names = FALSE)

# Remove unnecessary variables again to save memory
remove(ewrims_flat_file_use_season, ewrims_flat_file_use_season_Combined,
       ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS,
       ewrims_flat_file_use_season_Combined_DIRECT_DIV_SEASON_STATUS,
       ewrims_flat_file_use_season_Combined_USE_STATUS)

###########################################################################Duplicate_Reports_POD(F1)###################################################################
#################################################################################### Break ############################################################################


# Read back in the filtered ewrims flat file
Duplicate_Reports_POD <- read.csv("IntermediateData/ewrims_flat_file_WITH_FILTERS.csv")


# Select a subset of these columns
Duplicate_Reports_POD_FINAL_List <- Duplicate_Reports_POD %>%
  select("APPLICATION_NUMBER","POD_ID","LATITUDE","LONGITUDE","SOURCE_NAME","APPLICATION_PRIMARY_OWNER" ,"PRIMARY_OWNER_NAME","CERTIFICATE_ID","PERMIT_ID",	
         "LICENSE_ID",	"WATER_RIGHT_TYPE",	"WATER_RIGHT_STATUS",	"PRIMARY_OWNER_ENTITY_TYPE","MAX_DD_APPL","MAX_DD_UNITS","MAX_DD_ANN","MAX_STORAGE","MAX_TAKEN_FROM_SOURCE","USE_DIRECT_DIV_ANNUAL_AMOUNT",	
         "USE_STORAGE_AMOUNT",	"POD_NUMBER",	"POD_STATUS",	"DIRECT_DIVERSION_RATE",	"POD_TYPE")


# Write the shortened variable to a new CSV file
write.csv(Duplicate_Reports_POD_FINAL_List,"IntermediateData/Duplicate_Reports_POD_FINAL_List.csv", row.names = FALSE)



# At this point in the script, the original file had a procedure that produced 
# a CSV file called 'Overlapping_Water_Rights.csv'

# It is comparable to the Excel modules for QA/QC
# It highlights likely duplicated points of diversion in the dataset

# This version of the script does not include that procedure, but the original
# version can be referenced if that code is ever needed

# The script is "QAQC_Combine_Flat_Files" in "Supply and Demand Assessment - Documents\Onboarding Materials\demandanalysis_040722\Pre Processing Scripts\QAQC_Scripts"
# The output file is 'Overlapping_Water_Rights.csv' in "Supply and Demand Assessment - Documents\Onboarding Materials\demandanalysis_040722\Pre Processing Scripts\QAQC_Scripts\Output"


################################################################### Beneficial Use and Return Flow ############################################################

# Prepare the input file for the beneficial use module next

# Read in the CSV
Beneficial_Use_and_Return_Flow <- read.csv("IntermediateData/ewrims_flat_file_use_season_WITH_FILTERS.csv")


# Keep a subset of the columns
Beneficial_Use_and_Return_Flow_FINAL <- Beneficial_Use_and_Return_Flow %>%
  select(APPLICATION_NUMBER, USE_CODE, WATER_RIGHT_TYPE, FACE_VALUE_AMOUNT,
         INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT, APPLICATION_PRIMARY_OWNER,
         PRIMARY_OWNER_ENTITY_TYPE) %>%
  unique()


####Output the variable to a file
write.csv(Beneficial_Use_and_Return_Flow_FINAL,"IntermediateData/Beneficial_Use_and_Return_Flow_FINAL.csv", row.names = FALSE)


###################################################################Duplicate Values - Months and Years############################################################

# Create the input file for the duplicate months and years module

# Read in the CSV 
Duplicate_Values_Months_and_Years <- read.csv("IntermediateData/water_use_report_DATE.csv")


# Keep only necessary columns
Duplicate_Values_Months_and_Years <- Duplicate_Values_Months_and_Years %>%
  select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE)


# Remove USE from DIVERSION_TYPE
# unique() will be used to remove duplicated rows
# This sounds contradictory but the intended purpose of this module is to check for
# duplicated submissions across a right's submissions, not for duplicates of the same year-month pair
Duplicate_Values_Months_and_Years_FINAL <- Duplicate_Values_Months_and_Years %>%
  filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE")) %>% 
  unique()

# Output a CSV
write.csv(Duplicate_Values_Months_and_Years_FINAL,"IntermediateData/Duplicate_Values_Months_and_Years_FINAL.csv", row.names = FALSE)


###################################################################Statistics############################################################

# Get statistical data next

# Read in a CSV 
Statistics <- read.csv("IntermediateData/water_use_report_DATE.csv")


# Keep a subset of the columns
Statistics_FINAL  <- Statistics %>%
  select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE)


# Output the data
write.csv(Statistics_FINAL ,"IntermediateData/Statistics_FINAL.csv", row.names = FALSE)


# Read in another CSV next 
Statistics_FaceValue_IniDiv <- read.csv("IntermediateData/ewrims_flat_file_WITH_FILTERS.csv")


# Remove most variables from the data frame
Statistics_FaceValue_IniDiv_Final  <- Statistics_FaceValue_IniDiv %>%
  select(APPLICATION_NUMBER, INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT,
         FACE_VALUE_AMOUNT, FACE_VALUE_UNITS)


# Output results to a file structure
write.csv(Statistics_FaceValue_IniDiv_Final ,"IntermediateData/Statistics_FaceValue_IniDiv_Final.csv", row.names = FALSE)


################################################################### Diversion out of Season Part A ############################################################

# Write a CSV file for the first Diversion out of Season module

# Read in the use season flat file
Diversion_out_of_Season_Part_A <- read.csv("IntermediateData/ewrims_flat_file_use_season_WITH_FILTERS.csv")


# Extract a portion of the table
Diversion_out_of_Season_Part_A_FINAL <- Diversion_out_of_Season_Part_A %>%
  select(APPLICATION_NUMBER, USE_STATUS, DIRECT_SEASON_START_MONTH_1, DIRECT_SEASON_START_MONTH_2,
         DIRECT_DIV_SEASON_END_MONTH_1, DIRECT_DIV_SEASON_END_MONTH_2, STORAGE_SEASON_START_MONTH_1,
         STORAGE_SEASON_START_MONTH_2, STORAGE_SEASON_END_MONTH_1, STORAGE_SEASON_END_MONTH_2) %>%
  unique()


# Output the data to a file
write.csv(Diversion_out_of_Season_Part_A_FINAL,"IntermediateData/Diversion_out_of_Season_Part_A_FINAL.csv", row.names = FALSE)


###################################################################Diversion out of Season Part B############################################################

# Write a CSV file for the second Diversion out of Season module

# Read in a flat file
Diversion_out_of_Season_Part_B <- read.csv("IntermediateData/water_use_report_DATE.csv")


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
write.csv(Diversion_out_of_Season_Part_B_FINAL,"IntermediateData/Diversion_out_of_Season_Part_B_FINAL.csv", row.names = FALSE)


###################################################################Duplicate_Reports_Same Owner_Multiple_WR############################################################

# Create the input file for the duplicate owner module

# Read in the CSV
Duplicate_Reports_Same_Owner_Multiple_WR <- read.csv("IntermediateData/water_use_report_DATE.csv")


# Save a subset of the columns
# Also, filter the table to only diversion types that are "DIRECT" or "STORAGE"
# unique() will be used to remove duplicated rows
# This sounds contradictory but the intended purpose of this module is to check for
# duplicated submissions across a right's submissions, not for duplicates of the same year-month pair
Duplicate_Reports_Same_Owner_Multiple_WR_FINAL <- Duplicate_Reports_Same_Owner_Multiple_WR %>%
  select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE) %>%
  filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE")) %>% 
  unique()


# Output the variable to a file structure
write.csv(Duplicate_Reports_Same_Owner_Multiple_WR_FINAL,"IntermediateData/Duplicate_Reports_Same_Owner_Multiple_WR_FINAL.csv", row.names = FALSE)


# Remove unnecessary variables at this step to free up memory
remove(Beneficial_Use_and_Return_Flow, Beneficial_Use_and_Return_Flow_FINAL,
       Diversion_out_of_Season_Part_A, Diversion_out_of_Season_Part_A_FINAL,
       Diversion_out_of_Season_Part_B_N, Diversion_out_of_Season_Part_B,
       Diversion_out_of_Season_Part_B_FINAL, Duplicate_Reports_POD,
       Duplicate_Reports_POD_FINAL_List, Duplicate_Reports_Same_Owner_Multiple_WR,
       Duplicate_Reports_Same_Owner_Multiple_WR_FINAL, Duplicate_Values_Months_and_Years,
       Duplicate_Values_Months_and_Years_FINAL, 
       Statistics, Statistics_FaceValue_IniDiv, Statistics_FaceValue_IniDiv_Final,Statistics_FINAL)


###################################################################Missing RMS Reports############################################################

# Prepare the input file for the missing RMS reports module


# Read in a flat file CSV
Missing_RMS_Reports <- read.csv("IntermediateData/water_use_report_DATE.csv") %>%
  unique()


# Read in the results from the Priority Date module
Priority_Date <- read_xlsx("OutputData/Priority_Date_Scripted.xlsx", col_types = "text") %>%
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
write.csv(Missing_RMS_Reports_FINAL,"IntermediateData/Missing_RMS_Reports_FINAL.csv", row.names = FALSE)


######################################## QAQC Working Files###################################

####################Application Numbers############################

# Use "RR_pod_points_Merge_filtered_PA_[DATE].xlsx", extract two columns
Application_Number <- read_xlsx("InputData/RR_pod_points_Merge_filtered_PA_2023-09-19.xlsx") %>%
  group_by(APPLICATION_NUMBER, POD_ID) %>%
  summarize(FREQUENCY = n(), .groups = "drop") %>%
  select(APPLICATION_NUMBER, FREQUENCY) %>%
  unique()


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
write.csv(ewrims_flat_file_Working_File,"IntermediateData/ewrims_flat_file_Working_File.csv", row.names = FALSE)


####################################################Contact Information#################################################

# Load in the party flat file for contact information
# Keep only a subset of the columns
ewrims_flat_file_party <- read.csv("rawData/ewrims_flat_file_party.csv") %>%
  select(APPLICATION_ID, CONTACT_INFORMATION_PHONE, CONTACT_INFORMATION_EMAIL)


# Remove entries with duplicate application IDs (only the first instance of each ID will remain) 
ewrims_flat_file_party_APPLICATION_ID <- ewrims_flat_file_party[!duplicated(ewrims_flat_file_party$APPLICATION_ID), ]


# Get a subset with blank phone numbers 
Phone <- ewrims_flat_file_party_APPLICATION_ID %>%
  filter(CONTACT_INFORMATION_PHONE == "")


# Get a subset with blank emails
Email <- ewrims_flat_file_party_APPLICATION_ID %>%
  filter(CONTACT_INFORMATION_EMAIL == "")


# Get a subset with 999-999-9999 for phone numbers
Nine <- ewrims_flat_file_party_APPLICATION_ID %>%
  filter(CONTACT_INFORMATION_PHONE == "999-999-9999")


# Combine these three data frames
ewrims_flat_file_party_Final <- rbind(Phone, Nine, Email)


# Output the data to a CSV file
write.csv(ewrims_flat_file_party_Final,"IntermediateData/Missing_Contact_Information.csv", row.names = FALSE)



# Finally, remove all variables from the workspace
remove(ewrims_flat_file, ewrims_flat_file_one, ewrims_flat_file_party,
       ewrims_flat_file_party_APPLICATION_ID, ewrims_flat_file_party_Final,
       ewrims_flat_file_Three, ewrims_flat_file_Working_File, Application_Number,
       Nine, Phone, Priority_Date, Email, Missing_RMS_Reports, Missing_RMS_Reports_FINAL,
       Missing_RMS_Reports_Priority_Date_Combined)

print("Priority_Date_Postprocessing.R has finished running!")
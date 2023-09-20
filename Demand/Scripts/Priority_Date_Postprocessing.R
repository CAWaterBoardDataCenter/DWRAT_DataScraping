# Run the scripts one chunk at a time to insure that everything is working correctly. When you become more familiar with the code you can run in larger sections. 
#Install if you do not have in your current packages or are not up to date.----
# install.packages("tidyverse")
# install.packages("readxl")
#Load Packages- This step must be done each time the project is opened. ----
library(tidyverse)
library(readxl)



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

# Use "RR_pod_points_MAX_MAF_[DATE].xlsx", extract two columns
Application_Number <- read_xlsx("InputData/RR_POD_MAX_MAF_PA_2023-09-19.xlsx") %>%
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
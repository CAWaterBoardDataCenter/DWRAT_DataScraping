# Run the scripts one chunk at a time to insure that everything is working correctly. When you become more familiar with the code you can run in larger sections. 
#Install if you do not have in your current packages or are not up to date.----
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("geosphere")
# install.packages("stringi")
# install.packages("stringr")
# install.packages("pracma")
#Load Packages- This step must be done each time the project is opened. ----
library(tidyverse)
library(readxl)
library(geosphere)
library(stringi)
library(stringr)
library(pracma) #required for strcmpi function



######################################################################## List of Application from GIS Step ####################################################################################

# Import GIS data reviewed by SDU on 7/17/2023
Application_Number <- read_xlsx("InputData/RR_pod_points_MAX_MAF__20230717.xlsx") 


# Change the column name from "APPL_ID" to "APPLICATION_NUMBER"
# Keep only the "APPLICATION_NUMBER" and "FREQUENCY" columns
Application_Number <- Application_Number %>%
  rename(APPLICATION_NUMBER = APPL_ID) %>%
  select(APPLICATION_NUMBER, FREQUENCY)


# Read in the eWRIMS Flat File
ewrims_flat_file <- read.csv("RawData/ewrims_flat_file.csv")


# Perform an inner join using "APPLICATION_NUMBER"
# Multiple rows in 'Application_Number' may match with multiple rows in 'ewrims_flat_file'
ewrims_flat_file_Combined <- inner_join(Application_Number, ewrims_flat_file, by = "APPLICATION_NUMBER",
                                        relationship = "many-to-many")

# Output 'ewrims_flat_file_Combined' to a folder
write.csv(ewrims_flat_file_Combined,"InputData/ewrims_flat_file_WITH_FILTERS.csv", row.names = FALSE)


######################################################################## Break ####################################################################################

# Each of the spreadsheets that use the water use report need different filters so only the date is filtered here 


# Read in the (very large) water use report flat file
water_use_report <- read.csv("RawData/water_use_report.csv")


# Rename "APPL_ID" to "APPLICATION_NUMBER" to allow joins with 'Application_Number'
water_use_report <- water_use_report %>%
  rename(APPLICATION_NUMBER = APPL_ID)


# Perform an inner join (it is a many-to-many relationship once again)
water_use_report_Combined <- inner_join(Application_Number, water_use_report, by = "APPLICATION_NUMBER",
                                        relationship = "many-to-many")


# Remove all data from before 2014 because this is when the data structure changes in the system
water_use_report_Date <- water_use_report_Combined %>%
  filter(YEAR >= 2014)


# Output the data to a CSV file
write.csv(water_use_report_Date,"InputData/water_use_report_DATE.csv", row.names = FALSE)


# Remove variables from the environment that will no longer be used (free up memory)
remove(ewrims_flat_file_Combined, water_use_report, water_use_report_Combined, water_use_report_Date)

######################################################################## Break ####################################################################################

# Read the POD flat file next
ewrims_flat_file_use_season <- read.csv("RawData/ewrims_flat_file_pod.csv")


# Perform another inner join
ewrims_flat_file_use_season_Combined <- inner_join(Application_Number, ewrims_flat_file_use_season, by = "APPLICATION_NUMBER", 
                                                   relationship = "many-to-many")


# Remove rows where "APPLICATION_NUMBER" starts with "S" (statements of diversion and use)
ewrims_flat_file_use_season_Combined <- ewrims_flat_file_use_season_Combined %>%
  filter(!grepl("^S", APPLICATION_NUMBER)) 


# Filter by use status next
ewrims_flat_file_use_season_Combined_USE_STATUS <- ewrims_flat_file_use_season_Combined %>%
  filter(USE_STATUS %in% c("Added by change order", "Added by correction order",
                           "Added under section 798 of Regs", "Migrated from old WRIMS data",
                           "Requested when filed", ""))


# Perform additional filters for collection season status
ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS <- ewrims_flat_file_use_season_Combined_USE_STATUS %>%
  filter(COLLECTION_SEASON_STATUS %in% c("Migrated from old WRIMS data", "Reduced by order",
                                         "Reduced when licensed", "Requested when filed", ""))


################################################################# DIRECT_DIV_SEASON_STATUS ########################################################################


# Filter 'ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS' further
# This time, check the three different status columns
ewrims_flat_file_use_season_Combined_DIRECT_DIV_SEASON_STATUS <- ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS %>%
  filter(DIRECT_DIV_SEASON_STATUS %in% c("Migrated from old WRIMS data", "Reduced by order",
                                         "Reduced when licensed", "Requested when filed", ""))

# Write the output to a file
write.csv(ewrims_flat_file_use_season_Combined_DIRECT_DIV_SEASON_STATUS,
          "InputData/ewrims_flat_file_use_season_WITH_FILTERS.csv", row.names = FALSE)


# Remove unnecessary variables again to save memory
remove(ewrims_flat_file, ewrims_flat_file_use_season, ewrims_flat_file_use_season_Combined,
       ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS,
       ewrims_flat_file_use_season_Combined_DIRECT_DIV_SEASON_STATUS,
       ewrims_flat_file_use_season_Combined_USE_STATUS)


###########################################################################Duplicate_Reports_POD(F1)###################################################################
#################################################################################### Break ############################################################################


# Read back in the filtered ewrims flat file
Duplicate_Reports_POD <- read.csv("InputData/ewrims_flat_file_WITH_FILTERS.csv")


# Select a subset of these columns
Duplicate_Reports_POD_FINAL_List <- Duplicate_Reports_POD %>%
  select("WR_WATER_RIGHT_ID", "APPLICATION_NUMBER","POD_ID","LATITUDE","LONGITUDE","SOURCE_NAME","APPLICATION_PRIMARY_OWNER" ,"PRIMARY_OWNER_NAME","CERTIFICATE_ID","PERMIT_ID",	
         "LICENSE_ID",	"WATER_RIGHT_TYPE",	"WATER_RIGHT_STATUS",	"PRIMARY_OWNER_ENTITY_TYPE","MAX_DD_APPL","MAX_DD_UNITS","MAX_DD_ANN","MAX_STORAGE","MAX_TAKEN_FROM_SOURCE","USE_DIRECT_DIV_ANNUAL_AMOUNT",	
         "USE_STORAGE_AMOUNT",	"POD_NUMBER",	"POD_STATUS",	"DIRECT_DIVERSION_RATE",	"POD_TYPE")


# Write the shortened variable to a new CSV file
write.csv(Duplicate_Reports_POD_FINAL_List,"InputData/Duplicate_Reports_POD_FINAL_List.csv", row.names = FALSE)



# At this point in the script, the original file had a procedure that produced 
# a CSV file called 'Overlapping_Water_Rights.csv'

# It is comparable to the Excel modules for QA/QC
# It highlights likely duplicated points of diversion in the dataset

# This version of the script does not include that procedure, but the original
# version can be referenced if that code is ever needed

# The script is "QAQC_Combine_Flat_Files" in "Supply and Demand Assessment - Documents\Onboarding Materials\demandanalysis_040722\Pre Processing Scripts\QAQC_Scripts"
# The output file is 'Overlapping_Water_Rights.csv' in "Supply and Demand Assessment - Documents\Onboarding Materials\demandanalysis_040722\Pre Processing Scripts\QAQC_Scripts\Output"


################################################################### Priority Date ############################################################

# Produce the input file for the Priority Date module


# First, read in a flat file
Priority_Date <- read.csv("InputData/ewrims_flat_file_WITH_FILTERS.csv")


# Extract a subset of the columns
Priority_Date_FINAL <- Priority_Date %>%
  select(APPLICATION_NUMBER, WATER_RIGHT_TYPE, PRIORITY_DATE, APPLICATION_RECD_DATE, APPLICATION_ACCEPTANCE_DATE, SUB_TYPE, YEAR_DIVERSION_COMMENCED)


# Output that variable to a CSV file
write.csv(Priority_Date_FINAL,"InputData/Priority_Date_FINAL.csv", row.names = FALSE)


################################################################### Beneficial Use and Return Flow ############################################################

# Prepare the input file for the beneficial use module next

# Read in the CSV
Beneficial_Use_and_Return_Flow <- read.csv("InputData/ewrims_flat_file_use_season_WITH_FILTERS.csv")


# Keep a subset of the columns
Beneficial_Use_and_Return_Flow_FINAL <- Beneficial_Use_and_Return_Flow %>%
  select(APPLICATION_NUMBER, USE_CODE, WATER_RIGHT_TYPE, FACE_VALUE_AMOUNT,
         INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT, APPLICATION_PRIMARY_OWNER,
         PRIMARY_OWNER_ENTITY_TYPE)


####Output the variable to a file
write.csv(Beneficial_Use_and_Return_Flow_FINAL,"InputData/Beneficial_Use_and_Return_Flow_FINAL.csv", row.names = FALSE)


###################################################################Duplicate Values - Months and Years############################################################

# Create the input file for the duplicate months and years module

# Read in the CSV 
Duplicate_Values_Months_and_Years <- read.csv("InputData/water_use_report_DATE.csv")


# Keep only necessary columns
Duplicate_Values_Months_and_Years <- Duplicate_Values_Months_and_Years %>%
  select(APPLICATION_NUMBER, WATER_RIGHT_ID, YEAR, MONTH, AMOUNT, DIVERSION_TYPE)


# Remove USE from DIVERSION_TYPE
Duplicate_Values_Months_and_Years_FINAL <- Duplicate_Values_Months_and_Years %>%
  filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE"))


# Output a CSV
write.csv(Duplicate_Values_Months_and_Years_FINAL,"InputData/Duplicate_Values_Months_and_Years_FINAL.csv", row.names = FALSE)


###################################################################Statistics############################################################

# Get statistical data next

# Read in a CSV 
Statistics <- read.csv("InputData/water_use_report_DATE.csv")


# Keep a subset of the columns
Statistics_FINAL  <- Statistics %>%
  select(APPLICATION_NUMBER, WATER_RIGHT_ID, YEAR, MONTH, AMOUNT, DIVERSION_TYPE)


# Output the data
write.csv(Statistics_FINAL ,"InputData/Statistics_FINAL.csv", row.names = FALSE)


# Read in another CSV next 
Statistics_FaceValue_IniDiv <- read.csv("InputData/ewrims_flat_file_WITH_FILTERS.csv")


# Remove most variables from the data frame
Statistics_FaceValue_IniDiv_Final  <- Statistics_FaceValue_IniDiv %>%
  select(APPLICATION_NUMBER, INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT,
         FACE_VALUE_AMOUNT, FACE_VALUE_UNITS)


# Output results to a file structure
write.csv(Statistics_FaceValue_IniDiv_Final ,"InputData/Statistics_FaceValue_IniDiv_Final.csv", row.names = FALSE)


################################################################### Diversion out of Season Part A ############################################################

# Write a CSV file for the first Diversion out of Season module

# Read in the use season flat file
Diversion_out_of_Season_Part_A <- read.csv("RawData/ewrims_flat_file_use_season.csv")



# Extract a portion of the table
Diversion_out_of_Season_Part_A_FINAL <- Diversion_out_of_Season_Part_A %>%
  select(APPLICATION_NUMBER, USE_STATUS, DIRECT_SEASON_START_MONTH_1, DIRECT_SEASON_START_MONTH_2,
         DIRECT_DIV_SEASON_END_MONTH_1, DIRECT_DIV_SEASON_END_MONTH_2, STORAGE_SEASON_START_MONTH_1,
         STORAGE_SEASON_START_MONTH_2, STORAGE_SEASON_END_MONTH_1, STORAGE_SEASON_END_MONTH_2)


# Output the data to a file
write.csv(Diversion_out_of_Season_Part_A_FINAL,"InputData/Diversion_out_of_Season_Part_A_FINAL.csv", row.names = FALSE)


###################################################################Diversion out of Season Part B############################################################

# Write a CSV file for the second Diversion out of Season module

# Read in a flat file
Diversion_out_of_Season_Part_B <- read.csv("InputData/water_use_report_DATE.csv")


# Filter down the table to remove application numbers that start with "S" (statements of diversion and use)
# Also, keep only entries that have "DIRECT" or "STORAGE" as a diversion type
Diversion_out_of_Season_Part_B_N <- Diversion_out_of_Season_Part_B %>%
  filter(!grepl("^S", APPLICATION_NUMBER)) %>%
  filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE"))


# Extract a subset of the columns after that
Diversion_out_of_Season_Part_B_FINAL <- Diversion_out_of_Season_Part_B_N %>%
  select(APPLICATION_NUMBER, YEAR, MONTH, AMOUNT, DIVERSION_TYPE)


# Output a CSV file
write.csv(Diversion_out_of_Season_Part_B_FINAL,"InputData/Diversion_out_of_Season_Part_B_FINAL.csv", row.names = FALSE)


###################################################################Duplicate_Reports_Same Owner_Multiple_WR############################################################

# Create the input file for the duplicate owner module

# Read in the CSV
Duplicate_Reports_Same_Owner_Multiple_WR <- read.csv("InputData/water_use_report_DATE.csv")


# Save a subset of the columns
# Also, filter the table to only diversion types that are "DIRECT" or "STORAGE"
Duplicate_Reports_Same_Owner_Multiple_WR_FINAL <- Duplicate_Reports_Same_Owner_Multiple_WR %>%
  select(APPLICATION_NUMBER, WATER_RIGHT_ID, YEAR, MONTH, AMOUNT, DIVERSION_TYPE) %>%
  filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE"))


# Output the variable to a file structure
write.csv(Duplicate_Reports_Same_Owner_Multiple_WR_FINAL,"InputData/Duplicate_Reports_Same_Owner_Multiple_WR_FINAL.csv", row.names = FALSE)


# Remove unnecessary variables at this step to free up memory
remove(Beneficial_Use_and_Return_Flow, Beneficial_Use_and_Return_Flow_FINAL,
       Diversion_out_of_Season_Part_A, Diversion_out_of_Season_Part_A_FINAL,
       Diversion_out_of_Season_Part_B_N, Diversion_out_of_Season_Part_B,
       Diversion_out_of_Season_Part_B_FINAL, Duplicate_Reports_POD,
       Duplicate_Reports_POD_FINAL_List, Duplicate_Reports_Same_Owner_Multiple_WR,
       Duplicate_Reports_Same_Owner_Multiple_WR_FINAL, Duplicate_Values_Months_and_Years,
       Duplicate_Values_Months_and_Years_FINAL, Priority_Date, Priority_Date_FINAL, 
       Statistics, Statistics_FaceValue_IniDiv, Statistics_FaceValue_IniDiv_Final,Statistics_FINAL)


###################################################################Missing RMS Reports############################################################

# Prepare the input file for the missing RMS reports module


# Read in a flat file CSV
Missing_RMS_Reports <- read.csv("InputData/water_use_report_DATE.csv")


# Priority Date script runs here


# Read in from Priority date calculator  
Priority_Date <- read.csv("Input_Files/Priority_Date.csv")
#### Matching column names to merge
colnames(water_use_report)[2] <- "APPLICATION_NUMBER"
####Merge with Application list to reduce data
Missing_RMS_Reports_Priority_Date_Combined <- merge(Priority_Date,Missing_RMS_Reports,by="APPLICATION_NUMBER")
####Keep necessary columns
Missing_RMS_Reports <- Missing_RMS_Reports_Priority_Date_Combined[ , c("APPLICATION_NUMBER", "WATER_RIGHT_ID","YEAR","MONTH","AMOUNT","DIVERSION_TYPE","ASSIGNED_PRIORITY_DATE")]
####Keep necessary columns
Missing_RMS_Reports_FINAL <-Missing_RMS_Reports [Missing_RMS_Reports $DIVERSION_TYPE == "DIRECT" | 
                                                   Missing_RMS_Reports $DIVERSION_TYPE== "STORAGE"
                                                 , ] 
####Output to file structure
write.csv(Missing_RMS_Reports_FINAL,"Output\\Missing_RMS_Reports_FINAL.csv", row.names = FALSE)
######################################## QAQC Working Files###################################
####################Application Numbers############################
Application_Number <- read_xlsx("Input_Files/APPID_MAX_MAF.xlsx") #revised on 2/3/2023 by Payman Alemi to account for xlsx extension
#### Matching column names to merge
colnames(Application_Number )[1] <- "APPLICATION_NUMBER" #revised column 2 to 1 on 2/3/2023 by Payman Alemi
####Keep necessary columns
Application_Number <- Application_Number[ , c("APPLICATION_NUMBER", "FREQUENCY")]
####Keep necessary columns
ewrims_flat_file  <- ewrims_flat_file [ , c("APPLICATION_NUMBER", "WATER_RIGHT_TYPE","WATER_RIGHT_STATUS","PRIMARY_OWNER_ENTITY_TYPE","APPLICATION_PRIMARY_OWNER","SOURCE_NAME","TRIB_DESC","WATERSHED")]
####Merge with Application list to reduce data
ewrims_flat_file_one <- merge(Application_Number,ewrims_flat_file,by="APPLICATION_NUMBER", all=TRUE)
####Merge with Application list to reduce data
ewrims_flat_file_two <- merge(Application_Number,ewrims_flat_file_one,by="APPLICATION_NUMBER")
######Remove duplicate application numbers in the flat file. 
ewrims_flat_file_Three<- ewrims_flat_file_two[!duplicated(ewrims_flat_file_two$APPLICATION_NUMBER), ]
####Keep necessary columns
ewrims_flat_file_Working_File <- ewrims_flat_file_Three[ , c("APPLICATION_NUMBER", "WATER_RIGHT_TYPE","WATER_RIGHT_STATUS","PRIMARY_OWNER_ENTITY_TYPE","APPLICATION_PRIMARY_OWNER","SOURCE_NAME","TRIB_DESC","WATERSHED")]
####Output to file structure
write.csv(ewrims_flat_file_Working_File,"Output\\ewrims_flat_file_Working_File.csv", row.names = FALSE)
####################################################Contact Information#################################################
#### Download ewrims flat file Party for contact information
ewrims_flat_file_party <- read.csv(url("http://intapps.waterboards.ca.gov/downloadFile/faces/flatFilesEwrims.xhtml?fileName=ewrims_flat_file_party.csv"))
####Keep necessary columns
ewrims_flat_file_party <- ewrims_flat_file_party[ , c("APPLICATION_ID","CONTACT_INFORMATION_PHONE", "CONTACT_INFORMATION_EMAIL")]
####Remove Duplicates 
ewrims_flat_file_party_APPLICATION_ID <- ewrims_flat_file_party [!duplicated(ewrims_flat_file_party $APPLICATION_ID), ]
#########Subset blank phone numbers 
Phone <- ewrims_flat_file_party_APPLICATION_ID [ which(ewrims_flat_file_party_APPLICATION_ID$CONTACT_INFORMATION_PHONE =="" ), ]
#########Subset blank emails 
Email <- ewrims_flat_file_party_APPLICATION_ID [ which(ewrims_flat_file_party_APPLICATION_ID$CONTACT_INFORMATION_EMAIL ==""), ]
#########Subset 999-999-9999 from Phone numbers
Nine <- ewrims_flat_file_party_APPLICATION_ID [ which(ewrims_flat_file_party_APPLICATION_ID$CONTACT_INFORMATION_PHONE =="999-999-9999" ), ]
#########Combine the three different dataframes
ewrims_flat_file_party_Final <- rbind(Phone, Nine, Email)
#########Output to files in folder structure
write.csv(ewrims_flat_file_party_Final,"Output\\Missing_Contact_Information.csv", row.names = FALSE)




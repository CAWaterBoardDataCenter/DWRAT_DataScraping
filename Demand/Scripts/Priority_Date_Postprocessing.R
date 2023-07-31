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



###################################################################Missing RMS Reports############################################################

# Prepare the input file for the missing RMS reports module


# Read in a flat file CSV
Missing_RMS_Reports <- read.csv("InputData/water_use_report_DATE.csv")


# Priority Date script runs here


# Read in from Priority date calculator  
Priority_Date <- read_xlsx()

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




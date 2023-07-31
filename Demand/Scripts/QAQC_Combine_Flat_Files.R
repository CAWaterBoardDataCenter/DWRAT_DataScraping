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
#On some laptops/PC's there is not enough memory allotted for calculations.These codes will increase your memory size for R. 
#memory.limit()
#memory.limit(size=56000)
############################################################Filtering Flat File for GIS Step ##############################################################################################################
####Download POD flat file from the URL(Public)
##Water_Rights_Master_Flat_File_Points_of_Diversion <- read_csv(url("https://data.ca.gov/dataset/65296e4a-2417-466b-af8c-577f1936ba75/resource/0cf80c6f-3675-43be-b555-8d80ea469d69/download/ewrims_flat_file_pod.csv"))
####Download POD flat file from the URL(Internal DWR)
# Water_Rights_Master_Flat_File_Points_of_Diversion <- read.csv(url("http://intapps.waterboards.ca.gov/downloadFile/faces/flatFilesEwrims.xhtml?fileName=ewrims_flat_file_pod.csv"))
# #### Make sure that the POD_Status is Active
# Water_Rights_Master_Flat_File_Points_of_Diversion_POD_STATUS <- Water_Rights_Master_Flat_File_Points_of_Diversion[Water_Rights_Master_Flat_File_Points_of_Diversion$POD_STATUS == "Active" , ] 
# ####Filter by Water Right Type
# Water_Rights_Master_Flat_File_Points_of_Diversion_WATER_RIGHT_TYPE <- Water_Rights_Master_Flat_File_Points_of_Diversion_POD_STATUS[Water_Rights_Master_Flat_File_Points_of_Diversion_POD_STATUS$WATER_RIGHT_TYPE == "Appropriative" | 
#                                                                                                                                      Water_Rights_Master_Flat_File_Points_of_Diversion_POD_STATUS$WATER_RIGHT_TYPE == "Federal Claims" | 
#                                                                                                                                      Water_Rights_Master_Flat_File_Points_of_Diversion_POD_STATUS$WATER_RIGHT_TYPE == "Federal Stockponds"|
#                                                                                                                                      Water_Rights_Master_Flat_File_Points_of_Diversion_POD_STATUS$WATER_RIGHT_TYPE == "Registration Cannabis"|
#                                                                                                                                      Water_Rights_Master_Flat_File_Points_of_Diversion_POD_STATUS$WATER_RIGHT_TYPE == "Registration Domestic"|
#                                                                                                                                      Water_Rights_Master_Flat_File_Points_of_Diversion_POD_STATUS$WATER_RIGHT_TYPE == "Registration Irrigation"|
#                                                                                                                                      Water_Rights_Master_Flat_File_Points_of_Diversion_POD_STATUS$WATER_RIGHT_TYPE == "Registration Livestock"|
#                                                                                                                                      Water_Rights_Master_Flat_File_Points_of_Diversion_POD_STATUS$WATER_RIGHT_TYPE == "Statement of Div and Use"|                                                                                                                                                                                                                       
#                                                                                                                                      Water_Rights_Master_Flat_File_Points_of_Diversion_POD_STATUS$WATER_RIGHT_TYPE == "Stockpond"|
#                                                                                                                                      Water_Rights_Master_Flat_File_Points_of_Diversion_POD_STATUS$WATER_RIGHT_TYPE == "", ] 
# 
# 
# 
# 
# ####Filter by Water Right Status
# Water_Rights_Master_Flat_File_Points_of_Diversion_Final <- Water_Rights_Master_Flat_File_Points_of_Diversion_WATER_RIGHT_TYPE[Water_Rights_Master_Flat_File_Points_of_Diversion_WATER_RIGHT_TYPE$WATER_RIGHT_STATUS == "Active" | 
#                                                                                                                                 Water_Rights_Master_Flat_File_Points_of_Diversion_WATER_RIGHT_TYPE$WATER_RIGHT_STATUS == "Claimed - Local Oversight"|
#                                                                                                                                 Water_Rights_Master_Flat_File_Points_of_Diversion_WATER_RIGHT_TYPE$WATER_RIGHT_STATUS == "Certified" | 
#                                                                                                                                 Water_Rights_Master_Flat_File_Points_of_Diversion_WATER_RIGHT_TYPE$WATER_RIGHT_STATUS == "Claimed"|
#                                                                                                                                 Water_Rights_Master_Flat_File_Points_of_Diversion_WATER_RIGHT_TYPE$WATER_RIGHT_STATUS == "Completed"|
#                                                                                                                                 Water_Rights_Master_Flat_File_Points_of_Diversion_WATER_RIGHT_TYPE$WATER_RIGHT_STATUS == "Licensed"|
#                                                                                                                                 Water_Rights_Master_Flat_File_Points_of_Diversion_WATER_RIGHT_TYPE$WATER_RIGHT_STATUS == "Permitted"|
#                                                                                                                                 Water_Rights_Master_Flat_File_Points_of_Diversion_WATER_RIGHT_TYPE$WATER_RIGHT_STATUS == "Registered"|
#                                                                                                                                 Water_Rights_Master_Flat_File_Points_of_Diversion_WATER_RIGHT_TYPE$WATER_RIGHT_STATUS == "" , ] 
# #######################################USE THIS FILE FOR THE GIS STEP##########################################################################################################################################################################
# ####Check your output file
# write.csv(Water_Rights_Master_Flat_File_Points_of_Diversion_Final ,"Output\\Water_Rights_Master_Flat_File_Points_of_Diversion_Final.csv", row.names = FALSE)
###################################################################################STOP GIS STEP######################################################################################################
######################################################################## List of Application from GIS Step ####################################################################################
Application_Number <- read_xlsx("InputData/RR_pod_points_MAX_MAF__20230717.xlsx") # Importing GIS data reviewed by SDU on 7/17/2023
colnames(Application_Number)[2] <- "APPLICATION_NUMBER" 
Application_Number <- Application_Number[ , c("APPLICATION_NUMBER", "FREQUENCY")]
################# Use the URL's to pull the flat files to the correct location. Do not run these lines again so there is a static data source. ###########################################
####Download POD flat file from the URL(Public)
##ewrims_flat_file <- read.csv(url("https://data.ca.gov/dataset/65296e4a-2417-466b-af8c-577f1936ba75/resource/e8235902-adc3-48ce-ab96-fcf230a09208/download/ewrims_flat_file.csv"))
##Download POD flat file from the URL(Internal DWR)
ewrims_flat_file <- read.csv(url("http://intapps.waterboards.ca.gov/downloadFile/faces/flatFilesEwrims.xhtml?fileName=ewrims_flat_file.csv"))
##Merge with Application list to reduce data
ewrims_flat_file_Combined <- merge(Application_Number,ewrims_flat_file,by="APPLICATION_NUMBER")
####Output
write.csv(ewrims_flat_file_Combined,"OutputData/ewrims_flat_file_WITH_FILTERS.csv", row.names = FALSE)
######################################################################## Break ####################################################################################
#Each of the spreadsheets that use the water use report need different filters so only the date was filtered here. 
####Download POD flat file from the URL(Public)
##water_use_report  <- read.csv(url("https://data.ca.gov/dataset/65296e4a-2417-466b-af8c-577f1936ba75/resource/e0e46a85-3d46-4c15-ae3e-9cf873a49bb4/download/water_use_report.csv"))
####Download POD flat file from the URL(Internal DWR)
water_use_report <- read.csv(url("http://intapps.waterboards.ca.gov/downloadFile/faces/flatFilesEwrims.xhtml?fileName=water_use_report.csv"))
#### Matching column names to merge
colnames(water_use_report)[2] <- "APPLICATION_NUMBER"
####Merge with Application list to reduce data
water_use_report_Combined <- merge(Application_Number,water_use_report,by="APPLICATION_NUMBER")
####Remove all dates from 2014 and before becuse this is when the data structure changes in the system. 
water_use_report_Date <- subset(water_use_report_Combined , water_use_report_Combined $YEAR>= 2014)
####Output
write.csv(water_use_report_Date,"Flat_Files\\water_use_report_DATE.csv", row.names = FALSE)
######################################################################## Break ####################################################################################
####Download POD flat file from the URL(Public)
##ewrims_flat_file_use_season   <- read.csv(url("https://data.ca.gov/dataset/65296e4a-2417-466b-af8c-577f1936ba75/resource/fcf8b0d1-8775-4dfb-8a43-7a25f625c4e6/download/ewrims_flat_file_use_season-.csv"))
####Download POD flat file from the URL(Internal DWR)
ewrims_flat_file_use_season <- read.csv(url("http://intapps.waterboards.ca.gov/downloadFile/faces/flatFilesEwrims.xhtml?fileName=ewrims_flat_file_use_season.csv"))
####Merge with Application list to reduce data
ewrims_flat_file_use_season_Combined <- merge(Application_Number,ewrims_flat_file_use_season,by="APPLICATION_NUMBER")
###################################################Remove Statements ############################################
ewrims_flat_file_use_season_Combined <- ewrims_flat_file_use_season_Combined  %>% filter(!grepl("^S", APPLICATION_NUMBER))
#################################################################Filter by Use Status ############################################################################
ewrims_flat_file_use_season_Combined_USE_STATUS <- ewrims_flat_file_use_season_Combined[ewrims_flat_file_use_season_Combined$USE_STATUS == "Added by change order" | 
                                                                                          ewrims_flat_file_use_season_Combined$USE_STATUS == "Added by correction order"|
                                                                                          ewrims_flat_file_use_season_Combined$USE_STATUS == "Added under section 798 of Regs"|
                                                                                          ewrims_flat_file_use_season_Combined$USE_STATUS == "Migrated from old WRIMS data"|
                                                                                          ewrims_flat_file_use_season_Combined$USE_STATUS == "Requested when filed"|
                                                                                          ewrims_flat_file_use_season_Combined$USE_STATUS == "", ] 
#################################################################Filter by Use Status ############################################################################
ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS <- ewrims_flat_file_use_season_Combined_USE_STATUS[ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_1 == "Migrated from old WRIMS data" | 
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_1 == "Reduced by order"|
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_1 == "Reduced when licensed"|
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_1 == "Requested when filed"|
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_1 == ""|
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_2 == "Migrated from old WRIMS data" | 
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_2 == "Reduced by order"|
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_2 == "Reduced when licensed"|
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_2 == "Requested when filed"|
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_2 == ""|
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_3 == "Migrated from old WRIMS data" | 
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_3 == "Reduced by order"|
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_3 == "Reduced when licensed"|
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_3 == "Requested when filed"|
                                                                                                                   ewrims_flat_file_use_season_Combined_USE_STATUS$COLLECTION_SEASON_STATUS_3 == "", ] 
################################################################# DIRECT_DIV_SEASON_STATUS ########################################################################
ewrims_flat_file_use_season_Combined_DIRECT_DIV_SEASON_STATUS <- ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS [ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_1 == "Migrated from old WRIMS data" | 
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_1 == "Reduced by order"|
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_1 == "Reduced when licensed"|
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_1 == "Requested when filed"|
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_1 == ""|
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_2 == "Migrated from old WRIMS data" | 
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_2 == "Reduced by order"|
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_2 == "Reduced when licensed"|
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_2 == "Requested when filed"|
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_2 == ""|
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_3 == "Migrated from old WRIMS data" | 
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_3 == "Reduced by order"|
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_3 == "Reduced when licensed"|
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_3 == "Requested when filed"|
                                                                                                                                  ewrims_flat_file_use_season_Combined_COLLECTION_SEASON_STATUS$DIRECT_DIV_SEASON_STATUS_3 == "", ] 
####Output
write.csv(ewrims_flat_file_use_season_Combined_DIRECT_DIV_SEASON_STATUS,"Flat_Files\\ewrims_flat_file_use_season_WITH_FILTERS.csv", row.names = FALSE)
###########################################################################Duplicate_Reports_POD(F1)###################################################################
#################################################################################### Break ############################################################################
Duplicate_Reports_POD <- read.csv("Flat_Files/ewrims_flat_file_WITH_FILTERS.csv")
Duplicate_Reports_POD_FINAL_List <- Duplicate_Reports_POD[ , c("WR_WATER_RIGHT_ID", "APPLICATION_NUMBER","POD_ID","LATITUDE","LONGITUDE","SOURCE_NAME","APPLICATION_PRIMARY_OWNER" ,"PRIMARY_OWNER_NAME","CERTIFICATE_ID","PERMIT_ID",	
                                                               "LICENSE_ID",	"WATER_RIGHT_TYPE",	"WATER_RIGHT_STATUS",	"PRIMARY_OWNER_ENTITY_TYPE","MAX_DD_APPL","MAX_DD_UNITS","MAX_DD_ANN","MAX_STORAGE","MAX_TAKEN_FROM_SOURCE","USE_DIRECT_DIV_ANNUAL_AMOUNT",	
                                                               "USE_STORAGE_AMOUNT",	"POD_NUMBER",	"POD_STATUS",	"DIRECT_DIVERSION_RATE",	"POD_TYPE")]
write.csv(Duplicate_Reports_POD_FINAL_List,"Input_Files\\Duplicate_Reports_POD_FINAL_List.csv", row.names = FALSE)
flatf<-read.csv("Input_Files\\Duplicate_Reports_POD_FINAL_List.csv",header=T,fill=T)
flatf<-as.matrix(flatf)
nr<-nrow(flatf)
flatf[which(is.na(flatf)==TRUE)]<--999
i=3
dd<-matrix(0,nrow=nr, ncol=7)
cdup<-0
capp<-0
distance=0	
for (i in 1:nr){
  lat<-as.numeric(flatf[i,4])
  lon<-as.numeric(flatf[i,5])
  appi<-paste(substr(flatf[i,2],1,1))
  if ((strcmpi(appi,"s")=="FALSE")&&(lat!=-999)&&(lon!=-999)){
    npd<-0
    cpd<-0
    npn<-0
    cpn<-0
    npa<-0
    cpa<-0
    rr=5
    for(rr in 1:nr){
      lat1<-as.numeric(flatf[rr,4])
      lon1<-as.numeric(flatf[rr,5])
      appi1<-paste(substr(flatf[rr,2],1,1))
      if ((strcmpi(appi1,"s")=="TRUE")&&(rr!=i)&&(lat1!=-999)&&(lon1!=-999)){
        if (((lat1<=lat+.007)&&(lat1>=lat-.007)) && ((lon1<=lon+.009)&&(lon1>=lon-.009))){
          dist<-distGeo(c(lon, lat), c(lon1, lat1), a=6378137, f=1/298.257222101)
          if ((dist<=500)&&(strcmpi(flatf[i,2],flatf[rr,2])=="FALSE")) {
            npd<-npd+1
            cpd<-paste(cpd,flatf[rr,3],sep=",")
            if ((strcmpi(flatf[i,7],flatf[rr,7])=="TRUE") && (strcmpi(flatf[i,8],flatf[rr,8])=="TRUE")){
              npn<-npn+1
              cpn<-paste(cpn,flatf[rr,3],sep=",")
              cdup<-c(cdup,flatf[rr,3])
              capp<-c(capp,flatf[i,2])
              distance<-c(distance,dist)	
            }
            if (((as.numeric(flatf[rr,19])>0)&&((as.numeric(flatf[i,19])>=(as.numeric(flatf[rr,19])*0.5))&&(as.numeric(flatf[i,19])<=(as.numeric(flatf[rr,19])*1.5))))||
                ((as.numeric(flatf[rr,20])>0)&&((as.numeric(flatf[i,20])>=(as.numeric(flatf[rr,20])*0.5))&&(as.numeric(flatf[i,20])<=(as.numeric(flatf[rr,20])*1.5))))||
                ((as.numeric(flatf[rr,21])>0)&&((as.numeric(flatf[i,21])>=(as.numeric(flatf[rr,21])*0.5))&&(as.numeric(flatf[i,21])<=(as.numeric(flatf[rr,21])*1.5))))){
              npa<-npa+1
              cpa<-paste(cpa,flatf[rr,3],sep=",")
            }
          }#if4
        }#if3
      }#if2
    }#for2
  }#if1
  dd[i,1]<-flatf[i,3]
  if (npd>0){
    dd[i,2]<-npd
    dd[i,3]<-substr(cpd,3,nchar(cpd))
  }
  if (npn>0){
    dd[i,4]<-npn
    dd[i,5]<-substr(cpn,3,nchar(cpn))
  }
  if (npa>0){
    dd[i,6]<-npa
    dd[i,7]<-substr(cpa,3,nchar(cpa))
  }
}#for1
cdup<-cdup[-c(1)]
capp<-capp[-c(1)]
dupapp2<-cbind(cdup,capp,distance)
dupapp<-dupapp2[ , -c(3)]

colnames(	dupapp )[1] <- "POD_ID"
colnames(	dupapp )[2] <- "APPLICATION_NUMBER"
colnames(	dupapp2 )[1] <- "POD_ID"
colnames(	dupapp2 )[2] <- "APPLICATION_NUMBER"

write.csv(dupapp,"Input_files\\Flag1_Non_Main_500m_Radius_Duplicate_PODs_Apps.csv", row.names = FALSE)
dupapp <- read.csv("Input_files\\Flag1_Non_Main_500m_Radius_Duplicate_PODs_Apps.csv")
POD_Flat_File <- read.csv("Output\\Water_Rights_Master_Flat_File_Points_of_Diversion_Final.csv")

Overlapping_Water_Rights_DUPLICATE<- merge(dupapp ,POD_Flat_File,by="POD_ID")
Overlapping_Water_Rights  <- Overlapping_Water_Rights_DUPLICATE [ , c(1,2,18)]
colnames(Overlapping_Water_Rights )[2] <- "APPLICATION_NUMBER"
POD_Flat_File_Overlapping <- POD_Flat_File
colnames(POD_Flat_File_Overlapping)[2] <- "APPLICATION_NUMBER"
Overlapping_Water_Rights_PRIMARY<- merge(Overlapping_Water_Rights,POD_Flat_File_Overlapping,by="APPLICATION_NUMBER")
Overlapping_Water_Rights_PRIMARY <- Overlapping_Water_Rights_PRIMARY [ , c("APPLICATION_NUMBER","POD_ID.x","APPLICATION_PRIMARY_OWNER",
                                                                           "PRIMARY_OWNER_ENTITY_TYPE","FACE_VALUE_AMOUNT","INI_REPORTED_DIV_AMOUNT","INI_REPORTED_DIV_UNIT")]
colnames(Overlapping_Water_Rights_PRIMARY)[2] <- "POD_ID"



Overlapping_Water_Rights_DUPLICATE<- merge(Overlapping_Water_Rights_PRIMARY ,POD_Flat_File,by="POD_ID")




Overlapping_Water_Rights<- Overlapping_Water_Rights_DUPLICATE [ , c("POD_ID","APPLICATION_NUMBER","APPL_ID","APPLICATION_PRIMARY_OWNER.x","PRIMARY_OWNER_ENTITY_TYPE.x","FACE_VALUE_AMOUNT.x",
                                                                    "INI_REPORTED_DIV_AMOUNT.x","APPLICATION_PRIMARY_OWNER.y","PRIMARY_OWNER_ENTITY_TYPE.y","FACE_VALUE_AMOUNT.y"
                                                                    ,"INI_REPORTED_DIV_AMOUNT.y","INI_REPORTED_DIV_UNIT.y")]

Overlapping_Water_Rights<- merge(Overlapping_Water_Rights ,dupapp2,by="POD_ID")


Overlapping_Water_Rights  <- 	Overlapping_Water_Rights [ , -c(1,13)]



write.csv(Overlapping_Water_Rights,"Output\\Overlapping_Water_Rights.csv", row.names = FALSE)





################################################################### Priority Date ############################################################
#### Read in CSV 
Priority_Date <- read.csv("Flat_Files/ewrims_flat_file_WITH_FILTERS.csv")
####Keep necessary columns
Priority_Date_FINAL <- Priority_Date[ , c("APPLICATION_NUMBER","WATER_RIGHT_TYPE","PRIORITY_DATE","APPLICATION_RECD_DATE","APPLICATION_ACCEPTANCE_DATE", "SUB_TYPE","YEAR_DIVERSION_COMMENCED" )]
####Output to file structure
write.csv(Priority_Date_FINAL,"Output\\Priority_Date_FINAL.csv", row.names = FALSE)
################################################################### Beneficial Use and Return Flow ############################################################
#### Read in CSV 
Beneficial_Use_and_Return_Flow <- read.csv("Flat_Files/ewrims_flat_file_use_season_WITH_FILTERS.csv")
####Keep necessary columns
Beneficial_Use_and_Return_Flow_FINAL <- Beneficial_Use_and_Return_Flow[ , c("APPLICATION_NUMBER", "USE_CODE","WATER_RIGHT_TYPE","FACE_VALUE_AMOUNT","INI_REPORTED_DIV_AMOUNT","INI_REPORTED_DIV_UNIT" ,"APPLICATION_PRIMARY_OWNER","PRIMARY_OWNER_ENTITY_TYPE")]
####Output to file structure
write.csv(Beneficial_Use_and_Return_Flow_FINAL,"Output\\Beneficial_Use_and_Return_Flow_FINAL.csv", row.names = FALSE)
###################################################################Duplicate Values - Months and Years############################################################
#### Read in CSV 
Duplicate_Values_Months_and_Years <- read.csv("Flat_Files/water_use_report_DATE.csv")
####Keep necessary columns
Duplicate_Values_Months_and_Years <- Duplicate_Values_Months_and_Years[ , c("APPLICATION_NUMBER", "WATER_RIGHT_ID","YEAR","MONTH","AMOUNT","DIVERSION_TYPE")]
#### Remove USE from DIVERSION_TYPE
Duplicate_Values_Months_and_Years_FINAL <- Duplicate_Values_Months_and_Years[Duplicate_Values_Months_and_Years$DIVERSION_TYPE == "DIRECT" | 
                                                                               Duplicate_Values_Months_and_Years$DIVERSION_TYPE== "STORAGE"
                                                                             , ] 
####Output to file structure
write.csv(Duplicate_Values_Months_and_Years_FINAL,"Output\\Duplicate_Values_Months_and_Years_FINAL.csv", row.names = FALSE)
###################################################################Statistics############################################################
#### Read in CSV 
Statistics <- read.csv("Flat_Files/water_use_report_DATE.csv")
####Keep necessary columns
Statistics_FINAL  <- Statistics [ , c("APPLICATION_NUMBER", "WATER_RIGHT_ID","YEAR","MONTH","AMOUNT","DIVERSION_TYPE")]
####Output to file structure
write.csv(Statistics_FINAL ,"Output\\Statistics_FINAL.csv", row.names = FALSE)
#### Read in CSV 
Statistics_FaceValue_IniDiv <- read.csv("Flat_Files/ewrims_flat_file_WITH_FILTERS.csv")
####Keep necessary columns
Statistics_FaceValue_IniDiv_Final  <- Statistics_FaceValue_IniDiv [ , c("APPLICATION_NUMBER", "INI_REPORTED_DIV_AMOUNT","INI_REPORTED_DIV_UNIT","FACE_VALUE_AMOUNT","FACE_VALUE_UNITS")]
####Output to file structure
write.csv(Statistics_FaceValue_IniDiv_Final ,"Output\\Statistics_FaceValue_IniDiv_Final .csv", row.names = FALSE)
################################################################### Diversion out of Season Part A ############################################################
#### Read in CSV
Diversion_out_of_Season_Part_A <- read.csv("Flat_Files/ewrims_flat_file_use_season_WITH_FILTERS.csv")
####Keep necessary columns
Diversion_out_of_Season_Part_A_FINAL <- Diversion_out_of_Season_Part_A [ , c("APPLICATION_NUMBER","USE_STATUS","DIRECT_SEASON_START_MONTH_1", "DIRECT_DIV_SEASON_END_MONTH_1","DIRECT_SEASON_START_MONTH_2","DIRECT_DIV_SEASON_END_MONTH_2","STORAGE_SEASON_START_MONTH_1","STORAGE_SEASON_END_MONTH_1","STORAGE_SEASON_START_MONTH_2","STORAGE_SEASON_END_MONTH_2")]
####Output to file structure
write.csv(Diversion_out_of_Season_Part_A_FINAL,"Output\\Diversion_out_of_Season_Part_A_FINAL.csv", row.names = FALSE)
###################################################################Diversion out of Season Part B############################################################
#### Read in CSV
Diversion_out_of_Season_Part_B <- read.csv("Flat_Files/water_use_report_DATE.csv")
#### Remove Statements
Diversion_out_of_Season_Part_B_N <- Diversion_out_of_Season_Part_B  %>% filter(!grepl("^S", APPLICATION_NUMBER))
####Keep necessary columns
Diversion_out_of_Season_Part_B <- Diversion_out_of_Season_Part_B_N [ , c("APPLICATION_NUMBER","YEAR","MONTH","AMOUNT","DIVERSION_TYPE")]
####Keep necessary columns
Diversion_out_of_Season_Part_B_FINAL <- Diversion_out_of_Season_Part_B[Diversion_out_of_Season_Part_B$DIVERSION_TYPE == "DIRECT" | 
                                                                         Diversion_out_of_Season_Part_B$DIVERSION_TYPE== "STORAGE"
                                                                       , ] 
####Output to file structure
write.csv(Diversion_out_of_Season_Part_B_FINAL,"Output\\Diversion_out_of_Season_Part_B_FINAL.csv", row.names = FALSE)
###################################################################Duplicate_Reports_Same Owner_Multiple_WR############################################################
#### Read in CSV
Duplicate_Reports_Same_Owner_Multiple_WR <- read.csv("Flat_Files/water_use_report_DATE.csv")
####Keep necessary columns
Duplicate_Reports_Same_Owner_Multiple_WR <- Duplicate_Reports_Same_Owner_Multiple_WR[ , c("APPLICATION_NUMBER", "WATER_RIGHT_ID","YEAR","MONTH","AMOUNT","DIVERSION_TYPE")]
####Keep necessary columns
Duplicate_Reports_Same_Owner_Multiple_WR_FINAL <- Duplicate_Reports_Same_Owner_Multiple_WR[Duplicate_Reports_Same_Owner_Multiple_WR$DIVERSION_TYPE == "DIRECT" | 
                                                                                             Duplicate_Reports_Same_Owner_Multiple_WR$DIVERSION_TYPE== "STORAGE"
                                                                                           , ] 
####Output to file structure
write.csv(Duplicate_Reports_Same_Owner_Multiple_WR_FINAL,"Output\\Duplicate_Reports_Same_Owner_Multiple_WR_FINAL.csv", row.names = FALSE)
###################################################################Missing RMS Reports############################################################
#### Read in CSV
Missing_RMS_Reports <- read.csv("Flat_Files/water_use_report_DATE.csv")
#*#** Read in from Priority date calculator  
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




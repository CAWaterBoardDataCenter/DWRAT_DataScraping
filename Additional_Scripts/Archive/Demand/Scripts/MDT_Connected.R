#----PURPOSE:----


# This script generates the input ("child") demand files for the Connected version of DWRAT; 
# we will create 2 sets of input demand files for the 2017-2024 demand dataset excluding drought years
# 2021-2022

# Last Updated by: Payman Alemi on 7/9/2025

#Load Libraries----
library(tidyverse)
library(here)
source("Scripts/Shared_Functions_Demand.R")

#Import the demand CSVs
filePathFragment = "DWRAT/SDU_Runs/Demand_Datasets/RR_2017_2024_MDT_2025-04-04.csv"
Demand_Path = makeSharePointPath(filePathFragment = filePathFragment)
MDT = read.csv(file = Demand_Path)
                                   
                                
#Rename columns----
  #Rename Application_Number to USER
  MDT = rename(MDT, USER = APPLICATION_NUMBER)
  
  #Set Diversion Year--change depending on the flow month you're modeling
  Diversion_Year = "2025"
    
  #Rename Diversion Columns
  Diversion_Columns = c(paste0(Diversion_Year,"-01"), 
                        paste0(Diversion_Year,"-02"),
                        paste0(Diversion_Year,"-03"),
                        paste0(Diversion_Year,"-04"),
                        paste0(Diversion_Year,"-05"),
                        paste0(Diversion_Year,"-06"),
                        paste0(Diversion_Year,"-07"),
                        paste0(Diversion_Year,"-08"),
                        paste0(Diversion_Year,"-09"),
                        paste0(Diversion_Year,"-10"),
                        paste0(Diversion_Year,"-11"),
                        paste0(Diversion_Year,"-12"))
                        
  colnames(MDT)[2:13] = Diversion_Columns

  #Rename TOTAL_MAY_SEPT_DIV to MAY_SEPT_ZERO_DEMAND
  MDT = rename(MDT, MAY_SEPT_ZERO_DEMAND = TOTAL_MAY_SEPT_DIV)
  
  #Rename POWER_DEMAND_ZEROED to DEMAND_ZEROED_POWER
  MDT = rename(MDT, DEMAND_ZEROED_POWER = POWER_DEMAND_ZEROED)
  
  #Rename MAINSTEM_RR to MAINSTEM
  MDT = rename(MDT, MAINSTEM = MAINSTEM_RR)
  
  #Rename ASSIGNED_PRIORITY_DATE_SUB to PRIORITY
  MDT =rename(MDT, PRIORITY = ASSIGNED_PRIORITY_DATE_SUB)

  
#Remove unnecessary columns----
  #Define the columns to delete
  cols_to_delete <- c(
    'TOTAL_EXPECTED_ANNUAL_DIVERSION', 'PRIMARY_OWNER_TYPE', 'SOURCE_NAME', 
    'TRIB_DESC', 'FULLY NON-CONSUMPTIVE', 'PRE_1914', 'APPROPRIATIVE',
    'FACE_VALUE_AMOUNT_AF', 'INI_REPORTED_DIV_AMOUNT_AF', 'PERCENT_FACE'
  )
  
  # Remove the specified columns
  MDT <- MDT[, !names(MDT) %in% cols_to_delete]
  
#Separate MDTs into URR and LRR----
  URR_MDT = MDT %>% filter(UPPER_RUSSIAN == 'Y')
  
  LRR_MDT= MDT %>% filter(UPPER_RUSSIAN == 'N')
  
#Separate MDTs into riparian and appropriative---
  urr_rip_mdt = URR_MDT %>% filter(RIPARIAN == 'Y')
  
  urr_app_mdt = URR_MDT %>% filter(RIPARIAN == 'N')

  lrr_rip_mdt = LRR_MDT %>% filter(RIPARIAN == 'Y')
  
  lrr_app_mdt = LRR_MDT %>% filter(RIPARIAN == 'N')


#Export the MDTs into CSVs----
  #Create an Export List
  mdt_list = list(lrr_app_mdt,
                  lrr_rip_mdt,
                  urr_app_mdt,
                  urr_rip_mdt)
  
  #You have to manually assign the dataframe names to each item in the list, 
  # otherwise the export loop will fail because the dataframe names are null; 
  #lists wipe out dataframe names by default
  names(mdt_list) = c("lrr_app_mdt_2017_2024_2025_04-04", #"lrr_app_mdt_2017_2022",
                  "lrr_rip_mdt_2017_2024_2025_04-04", #"lrr_rip_mdt_2017_2022",
                  "urr_app_mdt__2017_2024_2025_04-04", #"urr_app_mdt_2017_2022",
                  "urr_rip_mdt_2017_2024_2025_04-04") #,"urr_rip_mdt_2017_2022")
                  
#Use a for loop to export each dataframe
  for (i in seq_along(mdt_list)) {
    filename <- names(mdt_list)[i]
    write.csv(mdt_list[[i]], file = paste0("OutputData/",filename, ".csv"), row.names = FALSE) 
  }
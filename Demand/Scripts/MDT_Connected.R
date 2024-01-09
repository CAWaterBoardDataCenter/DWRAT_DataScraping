# The purpose of this script is to generate the input demand files for 
# the Connected version of DWRAT; we will create 2 sets of input demand files: 
# for the 2017-2020 Russian River Master Demand Table and 2017-2022
# Russian River Master Demand Table

#Load Libraries----
library(tidyverse)
library(here)

#Import the original 2017-2019 demand CSVs
MDT_2017_2020 = read.csv(file = "2017-2020_RR_MasterDemandTable.csv")
MDT_2017_2022 = read.csv(file = "2017-2022_RR_MasterDemandTable.csv")

#Rename columns----
  #Rename Application_Number to USER
  MDT_2017_2020 = rename(MDT_2017_2020, USER = APPLICATION_NUMBER)
  MDT_2017_2022 = rename(MDT_2017_2022, USER = APPLICATION_NUMBER)
  
  #Rename Diversion Columns
  Diversion_Columns = c("2023-01", "2023-02", "2023-03", "2023-04",
                        "2023-05", "2023-06", "2023-07", "2023-08",
                        "2023-09", "2023-10", "2023-11", "2023-12")
  colnames(MDT_2017_2020)[2:13] = Diversion_Columns
  colnames(MDT_2017_2022)[2:13] = Diversion_Columns
  
  #Rename TOTAL_MAY_SEPT_DIV to MAY_SEPT_ZERO_DEMAND
  MDT_2017_2020 = rename(MDT_2017_2020, MAY_SEPT_ZERO_DEMAND = TOTAL_MAY_SEPT_DIV)
  MDT_2017_2022 = rename(MDT_2017_2022, MAY_SEPT_ZERO_DEMAND = TOTAL_MAY_SEPT_DIV)
  
  #Rename POWER_DEMAND_ZEROED to DEMAND_ZEROED_POWER
  MDT_2017_2020 = rename(MDT_2017_2020, DEMAND_ZEROED_POWER = POWER_DEMAND_ZEROED)
  MDT_2017_2022 = rename(MDT_2017_2022, DEMAND_ZEROED_POWER = POWER_DEMAND_ZEROED)
  
  #Rename MAINSTEM_RR to MAINSTEM
  MDT_2017_2020 = rename(MDT_2017_2020, MAINSTEM = MAINSTEM_RR)
  MDT_2017_2022 = rename(MDT_2017_2022, MAINSTEM = MAINSTEM_RR)
  
#Remove unnecessary columns----
  #Define the columns to delete
  cols_to_delete <- c(
    'TOTAL_EXPECTED_ANNUAL_DIVERSION', 'PRIMARY_OWNER_TYPE', 'SOURCE_NAME', 
    'TRIB_DESC', 'FULLY NON-CONSUMPTIVE', 'PRE_1914', 'APPROPRIATIVE',
    'FACE_VALUE_AMOUNT_AF', 'INI_REPORTED_DIV_AMOUNT_AF', 'PERCENT_FACE'
  )
  
  # Remove the specified columns
  MDT_2017_2020 <- MDT_2017_2020[, !names(MDT_2017_2020) %in% cols_to_delete]
  MDT_2017_2022 <- MDT_2017_2022[, !names(MDT_2017_2022) %in% cols_to_delete]
  

#Separate MDTs into URR and LRR----
  URR_MDT_2017_2020 = MDT_2017_2020 %>% filter(UPPER_RUSSIAN == 'Y')
  URR_MDT_2017_2022 = MDT_2017_2022 %>% filter(UPPER_RUSSIAN == 'Y')
  
  LRR_MDT_2017_2020 = MDT_2017_2020 %>% filter(UPPER_RUSSIAN == 'N')
  LRR_MDT_2017_2022 = MDT_2017_2022 %>% filter(UPPER_RUSSIAN == 'N')
  
#Separate MDTs into riparian and appropriative---
  urr_rip_mdt_2017_2020 = URR_MDT_2017_2020 %>% filter(RIPARIAN == 'N')
  urr_rip_mdt_2017_2022 = URR_MDT_2017_2022 %>% filter(RIPARIAN == 'N')
  urr_app_mdt_2017_2020 = URR_MDT_2017_2020 %>% filter(RIPARIAN == 'Y')
  urr_app_mdt_2017_2022 = URR_MDT_2017_2022 %>% filter(RIPARIAN == 'Y')
  
  lrr_rip_mdt_2017_2020 = LRR_MDT_2017_2020 %>% filter(RIPARIAN == 'N')
  lrr_rip_mdt_2017_2022 = LRR_MDT_2017_2022 %>% filter(RIPARIAN == 'N')
  lrr_app_mdt_2017_2020 = LRR_MDT_2017_2020 %>% filter(RIPARIAN == 'Y')
  lrr_app_mdt_2017_2022 = LRR_MDT_2017_2022 %>% filter(RIPARIAN == 'Y')
  

#Export the MDTs into CSVs----
  #Create an Export List
  MDT_List = list(lrr_app_mdt_2017_2020, lrr_app_mdt_2017_2022,
                  lrr_rip_mdt_2017_2020, lrr_rip_mdt_2017_2022,
                  urr_app_mdt_2017_2020, urr_app_mdt_2017_2022,
                  urr_rip_mdt_2017_2020, urr_rip_mdt_2017_2022)
                  
#Use a for loop to export each dataframe
  for (i in seq_along(MDT_List)) {
    filename <- names(MDT_List)[i]
    write.csv(MDT_List[[i]], file = paste0(filename, ".csv"), row.names = FALSE) 
  }
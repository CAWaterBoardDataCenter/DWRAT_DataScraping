#Load Libraries
library(tidyverse)
library(data.table)
library(here)

#For this script, it's useful to simultaneously open GIS_POD_Flat_File_Prep.R as well

source_folder = here("OutputData")

#Import Datasets----
GIS_output = read_xlsx("InputData/RR_pod_points_Merge_filtered_PA_2023-09-19.xlsx")
Effective_Dates_All_Rights = read.csv("InputData/Effective_Dates_All_Rights.csv")

#Import Demand Datasets
MDT_2017_2019 = read.csv(file = paste0(source_folder,"/", "2017-2019_RR_MasterDemandTable.csv"))
MDT_Connected_2017_2019 = read.csv(file = paste0(source_folder,"/", "2017-2019_RR_MasterDemandTable.csv"))
MDT_2017_2020 = read.csv(file = paste0(source_folder, "/", "2017-2020_RR_MasterDemandTable.csv"))
MDT_2017_2022 = read.csv(file = paste0(source_folder, "/", "2017-2022_RR_MasterDemandTable.csv"))

#Missing Rights Analysis----
##Compare MDT_2017-2019 to MDT_Connected_2017_2019----
  #Goal of this exercise is to prove that MDT_2017-2019 originated from the 
  #compilation of the 4 input CSVs from the Connected_DWRAT:
    #URR appropriative demand
    #URR riparian demand
    #LRR appropriative demand
    #LRR riparian demand

MDT_2017_Missing1 = anti_join(x = MDT_2017_2019,
                              y = MDT_Connected_2017_2019,
                              by = "APPLICATION_NUMBER")
#Returns 0 records

MDT_2017_Missing2 = anti_join(x = MDT_Connected_2017_2019,
                           y = MDT_2017_2019,
                           by = "APPLICATION_NUMBER")
#Returns 0 records
#We just established that MDT_2017-2019 is the compilation of the 4 input CSVs from the Connected_DWRAT

#Identify rights that exist in the 2017-2019 dataset that don't exist in the 2017-2020 dataset
MissingRights2020 = anti_join(x = MDT_2017_2019, 
                          y = MDT_2017_2020,
                          by = "APPLICATION_NUMBER")

#Whittle down MissingRights2020 to these 5 records, which belong in the RR dataset, but were not plotted 
#in eWRIMS until 12/27/2023

# Missing5 = c("S008163", "S015203", "S015206", "S015208", "D032464") %>% as.data.frame()
# colnames(Missing5) = "APPLICATION_NUMBER"
# Missing5_Details = inner_join(x = MissingRights2020, y = Missing5, by = "APPLICATION_NUMBER")
# colnames(Missing5_Details)
# 
# Missing5_Details = Missing5_Details %>% select("APPLICATION_NUMBER", "APPLICATION_PRIMARY_OWNER", "RIPARIAN", 
#                           "BASIN", "PRIORITY_CLASS", "JAN_MEAN_DIV", "FEB_MEAN_DIV", "MAR_MEAN_DIV", 
#                           "APR_MEAN_DIV", "MAY_MEAN_DIV", "JUN_MEAN_DIV", "JUL_MEAN_DIV", 
#                           "AUG_MEAN_DIV", "SEP_MEAN_DIV", "OCT_MEAN_DIV", "NOV_MEAN_DIV", 
#                           "DEC_MEAN_DIV", "NULL_DEMAND", "ZERO_DEMAND", "MAY_SEPT_ZERO_DEMAND")
# 
# ##Export Missing5_Details to CSV
# write.csv(Missing5_Details, here("IntermediateData/Missing5_Details.csv"), row.names= FALSE)

#Whittle down MissingRights2020 to just a few columns
MissingRights2020 = MissingRights2020 %>% select(APPLICATION_NUMBER, RIPARIAN, BASIN, UPPER_RUSSIAN)

#Inner_Join MissingRights2020 to the eWRIMS POD Flat File
  #The purpose of this exercise is to determine if the missing rights are tied to inactive PODs as of 12/18/2023, the download date of the eWRIMS POD Flat File
ewrims_pod_flat_file = read.csv("RawData/ewrims_flat_file_pod.csv")
MissingRights2020_POD_Data = inner_join(x = MissingRights2020,
                                        y = ewrims_pod_flat_file,
                                        by = "APPLICATION_NUMBER")

#Whittle MissingRights2020 to just the useful columns
cols_to_keep <- c("APPLICATION_NUMBER", "COUNTY", "LATITUDE", "LOCATION_METHOD", "LONGITUDE", 
                  "POD_ID", "POD_LAST_UPDATE_DATE", "POD_NUMBER",
                  "POD_STATUS", "POD_TYPE", "TRIB_DESC",
                  "WATER_RIGHT_STATUS","WATER_RIGHT_TYPE", "WATERSHED")

MissingRights2020_POD_Data = MissingRights2020_POD_Data %>% select(cols_to_keep)

#Export MissingRights2020_POD_Data to CSV for further analysis
write.csv(MissingRights2020_POD_Data,
          paste0("OutputData/MissingRights2020_POD_Data.csv"),  
          row.names = FALSE)
  #12 of the 30 records correspond to inactive water rights
  #10 of the 30 records correspond to active water rights but inactive PODs

#Investigate the 8 missing RR records that aren't inactive in any way
Missing8 = filter(MissingRights2020_POD_Data, POD_STATUS == "Active")
Missing8

Missing8 = filter(Missing8, WATER_RIGHT_STATUS!= "Revoked" & WATER_RIGHT_STATUS != "Inactive")
Missing8
#Do the missing 8 records exist in Flat_File_PODs_Status?
inner_join(x = Missing8, y = Flat_File_PODs_Status, by = "APPLICATION_NUMBER") %>% 
  select(APPLICATION_NUMBER)%>% nrow() #All 8 records exist, must have been filtered out later

#Do the missing 8 records exist in Flat_File_PODs_WR_Type?
inner_join(x = Missing8, y = Flat_File_PODs_WR_Type, by = "APPLICATION_NUMBER") %>% 
  select(APPLICATION_NUMBER)%>% nrow() #All 8 records exist, must have been filtered out later

#Do the missing 8 records exist in Flat_File_eWRIMS?
inner_join(x = Missing8, y = Flat_File_eWRIMS, by = "APPLICATION_NUMBER") %>% 
  select(APPLICATION_NUMBER)%>% nrow() #All 8 records exist, must have been filtered out in the demand dataset methodology

#Do the missing 8 records exist in the output file of the GIS pre-processing effort?
library(readxl)
GIS_input = read_excel("InputData/RR_pod_points_Merge_filtered_PA_2023-09-19.xlsx")
inner_join(x = Missing8, y = GIS_input, by = "APPLICATION_NUMBER") %>% 
  select(APPLICATION_NUMBER) #None of the records exist, appear to have been filtered out by the GIS pre-processing effort,
#Cross checking the GIS pre-processing manual review spreadsheet for these 8 records to confirm

#More Comparisons----
#Identify useful columns in MDT_2017-2019
MDT_columns = colnames(MDT_2017_2019)
MDT_columns 

Useful_columns = c('APPLICATION_NUMBER','JAN_MEAN_DIV','FEB_MEAN_DIV','MAR_MEAN_DIV',
                    'APR_MEAN_DIV','MAY_MEAN_DIV','JUN_MEAN_DIV','JUL_MEAN_DIV',
                   'AUG_MEAN_DIV','SEP_MEAN_DIV','OCT_MEAN_DIV','NOV_MEAN_DIV','DEC_MEAN_DIV')
Useful_columns

#Whittle the 3 datasets to just the useful columns
MDT_2017_2019 = MDT_2017_2019 %>% select(Useful_columns)
MDT_2017_2020 = MDT_2017_2020 %>% select(Useful_columns)
MDT_2017_2022 = MDT_2017_2022 %>% select(Useful_columns)

#Add an Annual_Demand column to all 3 dataframes
  #use rowSums function to sum the 4th -15th columns (JAN - DEC) for each row (water right)
MDT_2017_2019$Annual_Demand = rowSums(x = MDT_2017_2019[,2:13])
MDT_2017_2019$Annual_Demand

MDT_2017_2020$Annual_Demand= rowSums(x = MDT_2017_2020[,2:13])
MDT_2017_2020$Annual_Demand

MDT_2017_2022$Annual_Demand= rowSums(x = MDT_2017_2022[,2:13])
MDT_2017_2022$Annual_Demand

#Remove all the monthly diversion columns from the 3 dataframes
monthly_columns = c('JAN_MEAN_DIV','FEB_MEAN_DIV','MAR_MEAN_DIV',
'APR_MEAN_DIV','MAY_MEAN_DIV','JUN_MEAN_DIV','JUL_MEAN_DIV',
'AUG_MEAN_DIV','SEP_MEAN_DIV','OCT_MEAN_DIV','NOV_MEAN_DIV','DEC_MEAN_DIV')
monthly_columns

MDT_2017_2019 = MDT_2017_2019[, !colnames(MDT_2017_2019) %in% monthly_columns]
MDT_2017_2020 = MDT_2017_2020[, !colnames(MDT_2017_2020) %in% monthly_columns]
MDT_2017_2022 = MDT_2017_2022[, !colnames(MDT_2017_2022) %in% monthly_columns]

#Combine the 3 dataframes based on the APPLICATION_NUMBER field----
library(dplyr)

#Full Join MDT_2017_2019 and MDT_2017_2020
MDT2 <- full_join(x = MDT_2017_2019, y = MDT_2017_2020, by = "APPLICATION_NUMBER")

#Rename the Annual_Demand columns to reflect the source 
MDT2 = rename(MDT2, Annual_Demand_2017_2019 = Annual_Demand.x)
MDT2 = rename(MDT2, Annual_Demand_2017_2020 = Annual_Demand.y)
head(MDT2)


#Full Join MDT2 to MDT_2017_2022
MDT_Comparison = full_join(x = MDT2, y = MDT_2017_2022, by = "APPLICATION_NUMBER")
MDT_Comparison = rename(MDT_Comparison, Annual_Demand_2017_2022 = Annual_Demand)
head(MDT_Comparison)

#Round the columns in MDT_Comparison 
MDT_Comparison$Annual_Demand_2017_2019 = round(x = MDT_Comparison$Annual_Demand_2017_2019,2)
MDT_Comparison$Annual_Demand_2017_2020 = round(x = MDT_Comparison$Annual_Demand_2017_2020,2)
MDT_Comparison$Annual_Demand_2017_2022 = round(x = MDT_Comparison$Annual_Demand_2017_2022,2)

write.csv(MDT_Comparison, "MDT_Comparison.csv", row.names= FALSE)

#Count water rights that existed in 2017-2019 MDT, but not in 2017-2022 MDT
old_rights = anti_join(x = MDT_2017_2019, 
                        y = MDT_2017_2022,
                        by = "APPLICATION_NUMBER") %>%
              select(APPLICATION_NUMBER) %>% unique() %>% arrange() #keep just the application_number field
old_rights

#Count water rights that existed in 2017-2022 MDT, but not in 2017-2019 MDT
new_rights = anti_join(x = MDT_2017_2022, 
                                    y = MDT_2017_2019,
                                    by = "APPLICATION_NUMBER") %>%
  select(APPLICATION_NUMBER) %>% unique() %>% arrange()

new_rights


#Compare MDT_2017-2022 to output of GIS pre-processing step

GIS_output_unique_rights = GIS_output %>% select(APPLICATION_NUMBER) %>% unique()

GIS_comparison = anti_join(x = GIS_output_unique_rights, 
                          y = MDT_2017_2022, 
                          by = "APPLICATION_NUMBER")

#Compare 124 rights with effective dates in 2021-2023 to MDT_2017-2019
new_rights_effective_date = anti_join(x = Recent_RR_Rights,
                                      y = MDT_2017_2019,
                                      by = "APPLICATION_NUMBER") %>% 
  select(APPLICATION_NUMBER, EFFECTIVE_FROM_DATE) %>% unique() %>% arrange()
new_rights_effective_date
                


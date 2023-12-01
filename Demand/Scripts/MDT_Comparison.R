#Load Libraries
library(tidyverse)
library(data.table)
library(dplyr)
library(here)

source_folder = here("OutputData")

#Import Datasets----
Recent_RR_Rights = read.csv("IntermediateData/Recent_RR_Water_Rights.csv")
GIS_output = read_xlsx("InputData/RR_pod_points_Merge_filtered_PA_2023-09-19.xlsx")
Effective_Dates_All_Rights = read.csv("InputData/Effective_Dates_All_Rights.csv")

#Import Demand Datasets
MDT_2017_2019 = read.csv(file = paste0(source_folder,"/", "2017-2019_RR_MasterDemandTable.csv"))
MDT_2017_2020 = read.csv(file = paste0(source_folder, "/", "2017-2020_RR_MasterDemandTable.csv"))
MDT_2017_2022 = read.csv(file = paste0(source_folder, "/", "2017-2022_RR_MasterDemandTable.csv"))

#Identify useful columns
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
                


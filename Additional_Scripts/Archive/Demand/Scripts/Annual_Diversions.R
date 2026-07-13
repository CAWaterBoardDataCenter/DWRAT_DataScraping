#Load Packages- This step must be done each time the project is opened. ----
library(tidyverse)
library(writexl)

read_csv("http://intapps.waterboards.ca.gov/downloadFile/faces/flatFilesEwrims.xhtml?fileName=water_use_report.csv", show_col_types = FALSE) %>%
  write_csv("RawData/water_use_report.csv")

Water_Use_Report = read.csv("RawData/water_use_report.csv")

#Remove the Water_Right_ID field
Water_Use_Report$WATER_RIGHT_ID = NULL

# Filter out where DIVERSION_TYPE = "USE"
Water_Use_Report <- Water_Use_Report[Water_Use_Report$DIVERSION_TYPE != "USE",]

# Sum the annual diversion to storage and direct diversion amounts by right----

# Split the DIVERSION_TYPE column into two columns
Water_Use_Report <- Water_Use_Report %>%
  mutate(STORAGE = ifelse(DIVERSION_TYPE == "STORAGE", AMOUNT, 0),
         DIRECT = ifelse(DIVERSION_TYPE == "DIRECT", AMOUNT, 0))

#Remove DIVERSION_TYPE and AMOUNT columns
Water_Use_Report$DIVERSION_TYPE = NULL
Water_Use_Report$AMOUNT = NULL

Water_Use_Report_Annual <- Water_Use_Report %>%
  select(-MONTH) %>%
  group_by(APPL_ID, YEAR) %>%
  mutate(total_storage = sum(STORAGE)) %>%
  unique()
         
Water_Use_Report_Annual2  = Water_Use_Report %>%
  select(-MONTH) %>%
  group_by(APPL_ID, YEAR) %>%
  mutate(total_direct = sum(DIRECT)) %>%
  unique()

# Print the Water_Use_Report_Annual data frame
print(Water_Use_Report_Annual)


#Filter to Specific Water Rights  
S014230_Monthly = Water_Use_Report %>% filter(APPL_ID== "S014230")
S014231_Monthly = Water_Use_Report %>% filter(APPL_ID == "S014231")
A027892_Monthly = Water_Use_Report %>% filter(APPL_ID == "A027892")
A029632_Monthly = Water_Use_Report %>% filter(APPL_ID == "A029632")

#Write to CSV
write.csv(S014230_Monthly, "IntermediateData/S014230.csv")
write.csv(S014231_Monthly, "IntermediateData/S014231.csv")
write.csv(A027892_Monthly, "IntermediateData/A027892.csv")
write.csv(A029632_Monthly, "IntermediateData/A029632.csv")
  
  
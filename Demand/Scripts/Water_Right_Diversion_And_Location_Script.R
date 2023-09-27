#Load Packages- This step must be done each time the project is opened. ----
library(tidyverse)

# Download in advance all flat files that will be used in the procedures of this script and the other demand-related scripts
# They will be collected from *Internal URLs* that are updated daily; a Cal EPA network connection or VPN connection is required for this step

# Download the Water Rights Annual Water Use Report file next----
# read_csv() and write_csv() are used instead of download.file() because the file is very big (more than 370 MB)
# download.file() will fail if the required download time is greater than 60 seconds
read_csv("http://intapps.waterboards.ca.gov/downloadFile/faces/flatFilesEwrims.xhtml?fileName=water_use_report.csv", show_col_types = FALSE) %>%
  write_csv("RawData/water_use_report.csv")

Water_Use_Report = read.csv("RawData/water_use_report.csv")

# Aggregate Water Diversion Data for 2017-2023 by water right----
  #Remove the Water_Right_ID field
  Water_Use_Report$WATER_RIGHT_ID = NULL

  # Filter out all pre-2017 data
  Water_Use_Report <- Water_Use_Report[Water_Use_Report$YEAR >= 2017,]

  # Filter out where DIVERSION_TYPE = "USE"
  Water_Use_Report <- Water_Use_Report[Water_Use_Report$DIVERSION_TYPE != "USE",]
  
  # Sum all the DIVERSION_AMOUNT data by water right
  # Water_Use_Report_Summary <- Water_Use_Report %>%
  #   group_by(APPL_ID %>%
  #   summarise(DIVERSION_AMOUNT = sum(AMOUNT))
  
  
# Sum the annual diversion to storage and direct diversion amounts by right----
  
  # Split the DIVERSION_TYPE column into two columns
  Water_Use_Report <- Water_Use_Report %>%
    mutate(STORAGE = ifelse(DIVERSION_TYPE == "STORAGE", AMOUNT, 0),
           DIRECT = ifelse(DIVERSION_TYPE == "DIRECT", AMOUNT, 0))
  
  # Group the data frame by YEAR, MONTH, RIGHT, and DIVERSION_TYPE
  Water_Use_Report <- Water_Use_Report %>%
    group_by(YEAR, MONTH, APPL_ID, DIVERSION_TYPE)
  
  # Calculate the total amount of water used for STORAGE and DIRECT for each group
  Water_Use_Report <- Water_Use_Report %>%
      summarise(STORAGE = sum(STORAGE, na.rm = TRUE),
                DIRECT = sum(DIRECT, na.rm = TRUE))
  
  # Select the YEAR, MONTH, RIGHT, DIVERSION_TYPE, STORAGE, and DIRECT columns
  Water_Use_Report <- Water_Use_Report %>%
    select(YEAR, MONTH, APPL_ID, DIVERSION_TYPE, STORAGE, DIRECT)
  
  # Pivot the data frame to show the total amount of water used for STORAGE and DIRECT for each year and right
  Water_Use_Report <- Water_Use_Report %>%
    pivot_wider(names_from = DIVERSION_TYPE, values_from = c(STORAGE, DIRECT))
  
  # Select the YEAR, MONTH, RIGHT, STORAGE, and DIRECT columns
  Water_Use_Report <- Water_Use_Report %>%
    select(YEAR, APPL_ID, STORAGE, DIRECT)
  
  # Print the data frame
  print(Water_Use_Report)
  
  
  # Print the summary table
  print(Water_Use_Report_Summary)

# Save the POD flat file----
  download.file("http://intapps.waterboards.ca.gov/downloadFile/faces/flatFilesEwrims.xhtml?fileName=ewrims_flat_file_pod.csv", 
                "RawData/ewrims_flat_file_pod.csv", mode = "wb", quiet = TRUE)
  
Flat_File_PODs <- read.csv("RawData/ewrims_flat_file_pod.csv")

#Apply the proper filters to the POD Flat File----

##Filter for Active PODs----
Flat_File_PODs <- Flat_File_PODs[Flat_File_PODs$POD_STATUS == "Active", ]

#Water Right Types we ignore:
# Adjudicated
# Appropriative (State Filing)
# Cert of Right - Power
# Groundwater Recordation
# Non Jurisdictional
# Not Determined
# Section 12 File
# Temporary Permit
# Waste Water Change

##Filter by Water Right Type----
Flat_File_PODs <- Flat_File_PODs[Flat_File_PODs$WATER_RIGHT_TYPE == "Appropriative" | 
                                                  Flat_File_PODs$WATER_RIGHT_TYPE == "Federal Claims" | 
                                                  Flat_File_PODs$WATER_RIGHT_TYPE == "Federal Stockponds" |
                                                  Flat_File_PODs$WATER_RIGHT_TYPE == "Registration Cannabis" |
                                                  Flat_File_PODs$WATER_RIGHT_TYPE == "Registration Domestic" |
                                                  Flat_File_PODs$WATER_RIGHT_TYPE == "Registration Irrigation" |
                                                  Flat_File_PODs$WATER_RIGHT_TYPE == "Registration Livestock" |
                                                  Flat_File_PODs$WATER_RIGHT_TYPE == "Statement of Div and Use" |
                                                  Flat_File_PODs$WATER_RIGHT_TYPE == "Stockpond" |
                                                  Flat_File_PODs$WATER_RIGHT_TYPE == "",]

##Filter by Water Right Status----
Flat_File_PODs<- Flat_File_PODs[Flat_File_PODs$WATER_RIGHT_STATUS == "Active" | 
                                            Flat_File_PODs$WATER_RIGHT_STATUS == "Claimed - Local Oversight"|
                                            Flat_File_PODs$WATER_RIGHT_STATUS == "Certified" | 
                                            Flat_File_PODs$WATER_RIGHT_STATUS == "Claimed"|
                                            Flat_File_PODs$WATER_RIGHT_STATUS == "Completed"|
                                            Flat_File_PODs$WATER_RIGHT_STATUS == "Licensed"|
                                            Flat_File_PODs$WATER_RIGHT_STATUS == "Permitted"|
                                            Flat_File_PODs$WATER_RIGHT_STATUS == "Registered"|
                                            Flat_File_PODs$WATER_RIGHT_STATUS == "" , ] 

##Remove unnecessary columns from Flat File----
## GIS pre-processing steps require you to keep only these 9 columns----
cols_to_keep <- c("APPL_ID","POD_ID","POD_COUNT", "COUNTY","HUC_12_NAME", "HUC_12_NUMBER",
                  "HUC_8_NAME", "HUC_8_NUMBER", "LATITUDE", "LONGITUDE",
                  "TRIB_DESC", "WATERSHED")

Flat_File_eWRIMS <- Flat_File_PODs[, cols_to_keep, drop = FALSE]

## Convert Coordinate Fields From Character Format to Numeric Format----
Flat_File_eWRIMS <- Flat_File_eWRIMS %>%
  mutate_at(.vars = vars(LATITUDE, LONGITUDE), .funs = as.numeric)


#Inner Join Flat_File_eWRIMS to Water_Use_Report_Summary
WR_Div_Loc_Data = inner_join (x = Flat_File_eWRIMS, 
                                             y = Water_Use_Report_Summary,
                                             by = "APPL_ID")


####Check your output file
write.csv(WR_Div_Loc_Data,"IntermediateData/Flat_File_eWRIMS_2023-09-20.csv", row.names = FALSE)
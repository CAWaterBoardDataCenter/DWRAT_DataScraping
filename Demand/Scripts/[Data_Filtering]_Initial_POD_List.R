# Not all water rights in eWRIMS will be considered for the demand dataset
# Filter down 'ewrims_flat_file_pod.csv' to remove certain types of rights
# This initial list will be the starting point for any watershed and year range


#### Dependencies ####

require(tidyverse)

#### Procedure ####

cat("Starting '[Data_Filtering]_Initial_POD_List.R'...\n")



# Read in the POD flat file
Flat_File_PODs <- read.csv("RawData/ewrims_flat_file_pod.csv")



#Apply the proper filters----

##Filter for Active PODs----
Flat_File_PODs_Status <- Flat_File_PODs[Flat_File_PODs$POD_STATUS == "Active", ]

##Get unique water right types----
# WR_Types <- Flat_File_PODs$WATER_RIGHT_TYPE %>% 
#   unique() %>% #extracts unique values
#   trimws() %>% #removes leading and lagging spaces
#   sort() %>%  #sorts alphabetically
#   data.frame() %>% #converts the dataset into a dataframe
#   colnames = c("WR_Types") #Assigns the column name "WR_Types"

# WR_Types #print values

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
Flat_File_PODs_WR_Type <- Flat_File_PODs_Status[Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Appropriative" | 
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Federal Claims" | 
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Federal Stockponds" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Registration Cannabis" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Registration Domestic" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Registration Irrigation" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Registration Livestock" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Statement of Div and Use" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "Stockpond" |
                                                  Flat_File_PODs_Status$WATER_RIGHT_TYPE == "",]
##Get List of Water Right Statuses---- 
# WR_Statuses <- Flat_File_PODs$WATER_RIGHT_STATUS %>%
#   unique() %>% #Extracts unique values
#   trimws() %>% #Removes leading and lagging spaces
#   sort() %>%  #sorts alphabetically
#   data.frame() %>% #converts the dataset into a dataframe
#   colnames = c("WR_Statuses") #Assigns the column name "WR_Statuses"

#WR_Statuses #print values

##Filter by Water Right Status----
Flat_File_eWRIMS<- Flat_File_PODs_WR_Type[Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Active" | 
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Claimed - Local Oversight"|
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Certified" | 
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Claimed"|
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Completed"|
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Licensed"|
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Permitted"|
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "Registered"|
                                            Flat_File_PODs_WR_Type$WATER_RIGHT_STATUS == "" , ] 

##Remove unnecessary columns from Flat File----
#GIS pre-processing steps require you to keep only these 43 columns:
cols_to_keep <- c("APPLICATION_NUMBER", "CERTIFICATE_ID", "COUNTY", "EAST_COORD", "HUC_12_NAME", "HUC_12_NUMBER",
                  "HUC_8_NAME", "HUC_8_NUMBER", "LATITUDE", "LICENSE_ID", "LOCATION_METHOD", "LONGITUDE", "MERIDIAN", "NORTH_COORD",
                  "OBJECTID", "PARCEL_NUMBER", "PERMIT_ID", "POD_COUNT", "POD_ID", "POD_LAST_UPDATE_DATE", "POD_NUMBER",
                  "POD_NUMBER_GIS", "POD_STATUS", "POD_TYPE", "QUAD_MAP_NAME", "QUAD_MAP_NUMBER", "QUARTER", "QUARTER_QUARTER",
                  "RANGE_DIRECTION", "RANGE_NUMBER", "SECTION_CLASSIFIER", "SECTION_NUMBER", "SOURCE_NAME", "SP_ZONE",
                  "SPECIAL_USE_AREA", "TOWNSHIP_DIRECTION", "TOWNSHIP_NUMBER", "TRIB_DESC", "WATER_RIGHT_STATUS",
                  "WATER_RIGHT_TYPE", "WATERSHED", "WR_WATER_RIGHT_ID")

Flat_File_eWRIMS <- Flat_File_eWRIMS[, cols_to_keep, drop = FALSE]

#Replace Meridian Names with Meridian Short Names----
Flat_File_eWRIMS <- Flat_File_eWRIMS %>%
  mutate(MERIDIAN = case_when(
    MERIDIAN == " San Bernardino" ~"S",
    MERIDIAN == "Mount Diablo" ~"M",
    MERIDIAN == "Humboldt" ~"H",
    TRUE ~ MERIDIAN
  ))

#Add the FFMTRS field----
#This field serves as the Flat File Mountain Township Range Section field
#This field concatenates the Meridian, Township Number, Township Direction, Range Number, Range Direction, and Section Number fields
#This field is used as a basis of comparison with the MTRS field in the PLSS_Sections_Fill shapefile

Flat_File_eWRIMS$FFMTRS = paste0(Flat_File_eWRIMS$MERIDIAN, Flat_File_eWRIMS$TOWNSHIP_NUMBER, 
                                 Flat_File_eWRIMS$TOWNSHIP_DIRECTION, Flat_File_eWRIMS$RANGE_NUMBER, Flat_File_eWRIMS$RANGE_DIRECTION,
                                 Flat_File_eWRIMS$SECTION_NUMBER)

#Convert Coordinate Fields From Character Format to Numeric Format----
Flat_File_eWRIMS <- Flat_File_eWRIMS %>%
  mutate_at(.vars = vars(LATITUDE, LONGITUDE), .funs = as.numeric)

#######################################USE THIS FILE FOR THE GIS STEP##########################################################################################################################################################################
####Check your output file
write_csv(Flat_File_eWRIMS,
          paste0("IntermediateData/Flat_File_eWRIMS_", Sys.Date() - 1, ".csv"))



# Output a completion message
cat("Done!\n")


# Remove variables
remove(Flat_File_eWRIMS, Flat_File_PODs, Flat_File_PODs_Status,
       Flat_File_PODs_WR_Type, cols_to_keep)

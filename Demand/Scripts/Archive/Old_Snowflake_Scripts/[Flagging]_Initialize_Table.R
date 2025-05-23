# Create a new table that will contain flags related to the demand dataset
# Its source will be 'water_use_report_extended.csv'


#### Dependencies ####

require(data.table)

#### Procedure ####

cat("Starting '[Flagging]_Create_Initial_Table.R'...\n")



# Read in the watershed and data range selections
source("Scripts/Watershed_Selection.R")
source("Scripts/Dataset_Year_Range.R")



# Read in the preliminary POD list
podDF <- read_csv("RawData/Snowflake_ewrims_flat_file_pod.csv", 
                  show_col_types = FALSE, col_types = cols(.default = col_character()))



# Read in a subset of the extended water use flat file
# Only keep rights that appear within 'podDF' and 
# records that are between the desired year range
# Then, save it to a file in the "OutputData" folder
fread("RawData/Snowflake_water_use_report_extended.csv", 
      select = c("APPLICATION_NUMBER","YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE", "PARTY_ID")) %>% 
  unique() %>%
  filter(APPLICATION_NUMBER %in% podDF$APPLICATION_NUMBER) %>%
  filter(YEAR >= yearRange[1] & YEAR <= yearRange[2]) %>%
  write_csv(paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Flag_Table.csv"))



# Output a completion message
cat("Done!\n")



# Clean up the environment
remove(podDF)
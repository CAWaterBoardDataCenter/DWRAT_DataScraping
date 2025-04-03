# Initialize the flag table
# It will contain flags for different QA/QC concerns


#### Setup ####


remove(list = ls())


require(cli)
require(data.table)
require(tidyverse)


source("Scripts/New_Snowflake_Scripts/[HELPER]_1_Shared_Functions.R")


#### Procedure ####


print("Starting '[CA]_3_Flag_Table_Generation.R'...")



cat("\n\n")
cat(paste0("Data from the POD and extended tables will be used to initialize the flag table. ",
           "Subsequent scripts will append flagging columns to this table.") %>%
      strwrap(width = getOption("width") * 0.99) %>%
      paste0(collapse = "\n"))
cat("\n")



# Create variables with the file paths leading to the two files
# (The modified POD flat file and the extended flat file)
podDF <- makeSharePointPath("Program Watersheds\\7. Snowflake Demand Data Downloads\\eWRIMS Flat File POD Subset") %>%
  list.files(full.names = TRUE) %>%
  sort() %>% tail(1)


extendedDF <- makeSharePointPath("Program Watersheds\\7. Snowflake Demand Data Downloads\\Water Use Report Extended") %>%
  list.files(full.names = TRUE) %>%
  sort() %>% tail(1)



# To ensure that these two files are from the same version of the Snowflake database,
# Check the file creation dates of the *unedited* POD flat file and the extended flat file
# Throw an error if they were downloaded on different days 
if (as.Date(file.info(podDF %>% str_replace("POD Subset/(.+_)Flat_File_eWRIMS(_.+)$", "POD/\\1ewrims_flat_file_pod\\2"))$ctime, format = "%Y-%m-%d") != 
    as.Date(file.info(extendedDF)$ctime, format = "%Y-%m-%d")) {
  
  cat('\n\n')
  stop(paste0("'", podDF %>% str_extract("/[0-9]+.+$") %>% str_remove("^/"), "' and ",
              "'", extendedDF %>% str_extract("/[0-9]+.+$") %>% str_remove("^/"), "' ",
              "were created on different days. This may mean that they are from different versions ",
              "of the overall database.\n\nPlease rerun '[CA]_1_Snowflake_Data_Download.R' and ",
              "'[CA]_2_POD_Flat_File_Prep.R' to proceed.") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("created on different days", col_red("created on different days")) %>%
         str_replace_all("'(.+?)'", paste0("'", col_blue("\\1"), "'")) %>%
         str_replace("different versions", col_red("different versions")) %>%
         str_replace("rerun", col_green("rerun")))
  
}



# Read in the modified POD flat file
# (Use the file path already stored in 'podDF')
podDF <- podDF %>%
  fileRead("read_csv", col_types = cols(.default = col_character()))



# Read in a subset of the extended water use flat file
# Only keep rights that appear within 'podDF'
# Only keep reports from 2017 onwards
# Then, save it to a file 
extendedDF %>%
  fileRead("fread",
           select = c("APPLICATION_NUMBER","YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE", "PARTY_ID")) %>%
  unique() %>%
  filter(APPLICATION_NUMBER %in% podDF$APPLICATION_NUMBER) %>%
  filter(YEAR > 2016) %>%
  write_csv(makeSharePointPath(paste0("Program Watersheds/7. Snowflake Demand Data Downloads/Flag Table/",
                                      makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended/") %>% 
                                        list.files() %>% sort() %>% tail(1) %>% 
                                        str_replace("water_use_report_extended", "Flag_Table"))))



cat("\n\n")
# Output a completion message
print("The script is complete!")



# Clean up the environment
remove(list = ls())

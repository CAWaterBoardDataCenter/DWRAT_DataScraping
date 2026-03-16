# This script prepares the DAT file that is input into PRMS

# In the previous script, the downloaded meteorological dataset was formatted
# for integration into a PRMS input file

# It will now be merged into a long-running DAT file

# After that, predictions for the remainder of the water year 
# will also be appended (this is an optional but enabled-by-default step)

# Depending on the time of year, the type of predictions will differ:
#  (*) If 'endDate' is between October and February:
#      SPI-based predictions will be used
#  (*) If 'endDate' is between March and September AND this is the FIRST PRMS 
#      run in this period:
#      PRMS will be run with the SPI approach, the output will be 


# 



# Also, if there 




# This script has eight required input files:

# The station input files for each of the web scraping scripts are needed

# This time, in addition to the "STATION_ID" column, the script requires 
# columns that link these stations to specific columns in the PRMS DAT input file

# The required fields are:
#  (1) STATION_ID
#  (2) PRMS_PRECIP_NAME
#  (3) PRMS_TMIN_NAME
#  (4) PRMS_TMAX_NAME

# Every station should be linked to at least one column among the 
# 15 precipitation columns and 8 max/min temperature columns

# In addition to these files, the outputs of the web scraping scripts are all required:
#  (1) "WebData/PRISM_PRMS_Data_[startDate]_[endDate].csv"
#  (2) "WebData/NOAA_API_Data_[startDate]_[endDate].csv"
#  (3) "WebData/RAWS_HTTP_Data_[startDate]_[endDate].csv"
#  (4) "WebData/CIMIS_API_Data_[startDate]_[endDate].csv"


# These files will be combined into a single output file:
#  (1) "ProcessedData/Weather_Processed_Data_[startDate]_[endDate].csv"


#### Setup ####

# Clear the environment
remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")


#### Functions ####

mainProcedure <- function (allTempColumnsFromPRISM = TRUE) {
  
  cat("\n\n")
  cat("Starting 'RRS_006_Prepare_PRMS_Input.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  

  
  
  
  
  
  
  
  
  # Output a completion message
  cat(col_green("\n'RRS_006_Prepare_PRMS_Input.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
remove(list = ls())



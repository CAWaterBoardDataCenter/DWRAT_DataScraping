# Verify that all required PRISM weather data has been downloaded
# Then, reformat the data into a structure suitable for the SRP DAT file


# This script has two required input files:

# The first one is the station input file for PRISM

# This time, in addition to the "STATION_ID" column, the script requires 
# columns that link these stations to specific columns in the SRP DAT input file

# Thus, the required fields are:
#  (1) STATION_ID
#  (2) SRP_PRECIP_NAME
#  (3) SRP_TMIN_NAME
#  (4) SRP_TMAX_NAME

# Every SRP station should be linked to at least one column among the 
# 2 precipitation columns and 2 max/min temperature columns

# In addition to these files, the output of the PRISM web scraping script 
# is required:
#  (1) "W2_Russian_River/Intermediate/PRISM_SRP_Data_[startDate]_[endDate].csv"


# These files will be combined into a single output file:
#  (1) "W2_Russian_River/Output/SRP_Meteorological_[startDate]_[endDate].csv"


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")
source("W2_Russian_River/Scripts/HLP_014_Generate_Metorological_Dataset.R")


#### Functions ####

mainProcedure <- function (archiveFiles = TRUE) {
  
  cat("\n\n")
  cat("Starting 'RRW_012_Process_SRP_Weather_Data.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Use the functions in 'HLP_014_Generate_Metorological_Dataset.R' 
  # to complete this procedure
  
  # This function requires several different inputs
  # Input model information, the paths to meteorological input and output files,
  # and QA/QC filepaths 
  
  # Several meteorological files will be added to the "Output" folder
  merge_weather_data(startDate, endDate, "SRP", 
                     
                     prismInputPath = getFromControl_RR("PRISM_SRP_STATIONS_CSV") |>
                       sharepointPathCheck(isFolder = FALSE), 
                     prismOutputPath = paste0("W2_Russian_River/Intermediate/PRISM_SRP_Data_",
                                              startDate, "_", endDate, ".csv"), 
                     
                     allTempColumnsFromPRISM = TRUE, 
                     siPRISM = FALSE, 
                     applyFullQAQC = TRUE, 
                     archiveFiles = archiveFiles, 
                     
                     noaaInputPath = getFromControl_RR("NOAA_STATIONS_CSV") |>
                       sharepointPathCheck(isFolder = FALSE), 
                     noaaOutputPath = paste0("W2_Russian_River/Intermediate/NOAA_API_Data_",
                                             startDate, "_", endDate, ".csv"),
                     
                     rawsInputPath = getFromControl_RR("RAWS_STATIONS_CSV") |>
                       sharepointPathCheck(isFolder = FALSE), 
                     rawsOutputPath = paste0("W2_Russian_River/Intermediate/RAWS_HTTP_Data_",
                                             startDate, "_", endDate, ".csv"),
                     
                     cimisInputPath = getFromControl_RR("CIMIS_STATIONS_CSV") |>
                       sharepointPathCheck(isFolder = FALSE), 
                     cimisOutputPath = paste0("W2_Russian_River/Intermediate/CIMIS_API_Data_",
                                              startDate, "_", endDate, ".csv"),
                     
                     cdecInputPath = getFromControl_RR("CDEC_PRECIPITATION_STATIONS_CSV") |>
                       sharepointPathCheck(isFolder = FALSE), 
                     cdecOutputPath = paste0("W2_Russian_River/Intermediate/CDEC_API_",
                                             "Precip_Data_",
                                             startDate, "_", endDate, ".csv"),
                     
                     precipOutliersPath = getFromControl_RR("SRP_PRECIP_GAGE_OUTLIER_BOUNDS") |>
                       sharepointPathCheck(isFolder = FALSE), 
                     
                     precipCorrPath = getFromControl_RR("SRP_PRECIP_GAGE_CORRELATION_TABLE") |>
                       sharepointPathCheck(isFolder = FALSE))
  
  
  # Output a completion message
  cat(col_green("\n'RRW_012_Process_SRP_Weather_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

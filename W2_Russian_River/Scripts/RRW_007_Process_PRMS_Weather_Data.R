# Verify that all required weather data has been downloaded
# Then, reformat the data into a structure suitable for the PRMS DAT file


# This script has twelve required input files:

# The five station input files for each of the web scraping scripts are needed

# This time, in addition to the "STATION_ID" column, the script requires 
# columns that link these stations to specific columns in the PRMS DAT input file

# The required fields are:
#  (1) STATION_ID
#  (2) PRMS_PRECIP_NAME
#  (3) PRMS_TMIN_NAME
#  (4) PRMS_TMAX_NAME

# Every station should be linked to at least one column among the 
# 45 precipitation columns and 8 max/min temperature columns

# In addition to these files, the outputs of the web scraping scripts are all required:
#  (1) "W2_Russian_River/Intermediate/PRISM_PRMS_Data_[startDate]_[endDate].csv"
#  (2) "W2_Russian_River/Intermediate/NOAA_API_Data_[startDate]_[endDate].csv"
#  (3) "W2_Russian_River/Intermediate/RAWS_HTTP_Data_[startDate]_[endDate].csv"
#  (4) "W2_Russian_River/Intermediate/CIMIS_API_Data_[startDate]_[endDate].csv"
#  (5) "W2_Russian_River/Intermediate/CDEC_API_Precip_Data_[startDate]_[endDate].csv"


# The remaining two input files are related to QA/QC procedures 
# for the precipitation stations

# Both outlier thresholds and inter-gage correlations are required
# for these processes

# (The RRW "EX2" and "EX3" scripts contain documentation and procedures 
#  related to the origin of these files)


# The station data will be combined into a single output file:
#  (1) "W2_Russian_River/Output/PRMS_Meteorological_[startDate]_[endDate].csv"

# This file will contain the data after QA/QC and PRISM temperature substitution
# procedures have been applied


# Before that final result, two intermediate files will be saved as well

# Before any QA/QC procedures are applied, the combined station data will be saved as:
#  (1) "W2_Russian_River/Output/PRMS_Meteorological_No_QC_Intermediate_[startDate]_[endDate].csv"

# Then, after the quality flags provided by CIMIS and CDEC are applied, the combined
# file will be saved again as:
#  (1) "W2_Russian_River/Output/PRMS_Meteorological_QC_Intermediate_[startDate]_[endDate].csv"


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

mainProcedure <- function (allTempColumnsFromPRISM = TRUE, archiveFiles = TRUE) {
  
  cat("\n\n")
  cat("Starting 'RRW_007_Process_PRMS_Weather_Data.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Use the functions in 'HLP_014_Generate_Metorological_Dataset.R' 
  # to complete this procedure
  
  # This function requires several different inputs
  # Input model information, the paths to meteorological input and output files,
  # and QA/QC filepaths 
  
  # Several meteorological files will be added to the "Output" folder
  merge_weather_data(startDate, endDate, "PRMS", 
                     
                     prismInputPath = getFromControl_RR("PRISM_PRMS_STATIONS_CSV") |>
                       sharepointPathCheck(isFolder = FALSE), 
                     prismOutputPath = paste0("W2_Russian_River/Intermediate/PRISM_PRMS_Data_",
                                              startDate, "_", endDate, ".csv"), 
                     
                     allTempColumnsFromPRISM = allTempColumnsFromPRISM, 
                     siPRISM = TRUE,
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
                     
                     precipOutliersPath = getFromControl_RR("PRMS_PRECIP_GAGE_OUTLIER_BOUNDS") |>
                       sharepointPathCheck(isFolder = FALSE), 
                     
                     precipCorrPath = getFromControl_RR("PRMS_PRECIP_GAGE_CORRELATION_TABLE") |>
                       sharepointPathCheck(isFolder = FALSE))
  
  
  # Output a completion message
  cat(col_green("\n'RRW_007_Process_PRMS_Weather_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

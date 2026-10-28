# This script prepares the weather data DAT file that is input into RRIHM
# The control file and batch file in RRIHM are setup as well

# Earlier in the process, the downloaded meteorological dataset was formatted
# for integration into a PRMS input file

# It will now be merged into a long-running DAT file for RRIHM

# After that, predictions for the remainder of the water year will
# also be appended (this is an optional but enabled-by-default step)

# Depending on the time of year, the prediction method will differ:

#  (*) If 'endDate' is between October and February:
#      SPI-based predictions will be used

#  (*) If 'endDate' is between March and September:
#      (1) One of three linear regression models will be applied
#          to identify the most similar water year for the current
#          water year and the remainder of the water year 

# The script has several input files:
#  (1) "W2_Russian_River/Output/RRIHM_Meteorological_[startDate]_[endDate].csv"
#      The processed weather data

#  (2) A long-running DAT file (whose parent folder is input into 
#      "MAIN_PRMS_DAT_FOLDER" of the control file)

#  (3) From the RRIHM model files, the "gsflow_rr.control" file will be edited

#  (4) Similarly, the model's "run.bat" file will be updated


# A single output will be generated in all cases, and additional outputs will 
# be included whenever the "Similar WY" procedure is executed: 

#  (1) "DAT_RRIHM_[startDate]_[endDate].dat"
#      The final DAT file to use in the model run
#      (This file is also copied to the "RRIHM" folder 
#       as "RR_RRIHM_Input.dat")

#  (2) If the similar WY needs to be identified, a summary CSV from that 
#      procedure will be generated as well

# Technically, "gsflow_rr.control" and "run.bat" are output by 
# this script as well
# (Though, the copied "RRIHM" contents will be deleted at the end of the model
#  run procedure, so they will not stick around for long)


# To Do: The control file editing and batch file generation functions can be made
#        more generic (there's a lot of overlap with PRMS)


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function (predictWY = TRUE) {
  
  cat("\n\n")
  cat("Starting 'RRW_v2_016_Finalize_RRIHM_Weather_Input.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Import functions from the v1 PRMS and SRP scripts
  c("predictCurrentWY", "spiPrediction", "similarWYPrediction", "importLinModels",
    "similarWY_findWY", "similarWY_appendDAT", "validate_num_stations",
    "updateMetadata_DAT", "outputDAT", "updateControlFilePRMS",
    "updateBatchFilePRMS") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_009_Finalize_PRMS_Input.R", .))
  
    
  # Confirm that a proper directory exists for model input and output files
  # The actual SRP model files should have been successfully copied to
  # the "Output" folder too
  cat(paste0("[1/", if_else(predictWY, 5, 4),
             "]\tChecking directories...\n"))
  
  
  # Check for the directory that contains metadata and model input/output files
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Also confirm that the RRIHM model folder was copied to "Output"
  prmsPath <- validate_model_copy("RRIHM")
  
  
  cat("\tDone!\n\n")
  
  
  cat(paste0("[2/", if_else(predictWY, 5, 4),
             "]\tLoading meteorological data and long-running DAT file...\n"))
  
  
  # Read in two of the main input files
  # (The SRPHM Meteorological CSV and the primary DAT file)
  filePaths <- tibble("METEOROLOGICAL" = 
                        paste0("W2_Russian_River/Output/RRIHM_Meteorological_", startDate,
                               "_", endDate, ".csv"),
                      "MAIN_DAT" = get_model_historic_DAT_filepath("RRIHM"))
  
  
  # Read in the two files next (while also verifying that they exist)
  meteorDF <- filePaths$METEOROLOGICAL[1] |> 
    checkForPreviousOutput() |>
    getDelim(",")
  
  
  primaryDAT <- getFile(filePaths$MAIN_DAT[1], delim = ",")
  
  
  # Validate the primary DAT file next
  # (This function also adds a "DATE" column to 'primaryDAT')
  # (That column enables matching with 'meteorDF')
  primaryDAT <- validateInputDAT(primaryDAT, filePaths$MAIN_DAT[1], "RRIHM", 
                                 names(meteorDF)[names(meteorDF) != "DATE"],
                                 startDate, endDate, datType = "Main")
  
  
  # Output messages
  cat("\tDone!\n\n")
  
  
  cat(paste0("[3/", if_else(predictWY, 5, 4),
             "]\tMerging the two files together...\n"))
  
  
  # Merge 'primaryDAT' with 'meteorDF'
  # (Overlapping dates with 'startDate' are removed from 'primaryDAT')
  # (Also, "YEAR", "MONTH", "DAY", "HOUR", "MINUTE", and "SECOND" are 
  #  added to 'meteorDF')
  mergedDAT <- primaryDAT |> 
    filter(DATE < startDate) |>
    bind_rows(meteorDF |>
                mutate(YEAR = year(DATE), MONTH = month(DATE), DAY = day(DATE),
                       HOUR = 0, MINUTE = 0, SECOND = 0))
  
  
  cat("\tDone!\n\n")
  
  
  # After that, if 'predictWY' is TRUE, add predictions for 
  # the current water year
  if (predictWY) {
    
    cat(paste0("[4/5]\tAppending weather predictions for ",
               "the current water year...\n"))
    
    
    mergedDAT <- predictCurrentWY(mergedDAT,
                                  startDate, endDate, "RRIHM", 
                                  names(meteorDF)[names(meteorDF) != "DATE"],
                                  dirPath, filePaths$MAIN_DAT[1])
    
    
    cat("\tDone!\n\n")
    
    
    # Even if predictions will not be used, 
    # update metadata in the hydrology folder
  } else {
    
    updateMetadata_DAT(dirPath, datStartDate = min(mergedDAT$DATE),
                       modelEndDate = endDate, 
                       predictionMethod = NA_character_, filePaths$MAIN_DAT[1],
                       model = "RRIHM")
    
  }
  
  
  # Before writing 'mergedDAT' to a file, make sure that it contains 
  # the correct number of precipitation and temperature stations
  mergedDAT |>
    validate_num_stations(prmsPath, "RRIHM")
  
  
  cat(paste0("[", if_else(predictWY, "5/5", "4/4"),
             "]\tSaving output...\n"))
  
  
  # Finally, write 'mergedDAT' to a file
  # It will be stored in both the "RRIHM" folder and the model run 
  # hydrology folder 
  mergedDAT |>
    outputDAT(startDate, endDate, dirPath, prmsPath, predictWY, "RRIHM")
  
  
  # Archive the historic DAT file too
  copy_file_to_archive(filePaths$MAIN_DAT, dirPath, "RRIHM")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_016_Finalize_RRIHM_Weather_Input.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

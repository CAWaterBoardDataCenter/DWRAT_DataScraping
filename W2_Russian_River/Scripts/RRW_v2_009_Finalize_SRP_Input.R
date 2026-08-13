# This script prepares the DAT file that is input into SRP
# The control file and batch file in SRP are setup as well

# Earlier in the process, the downloaded meteorological dataset was formatted
# for integration into a SRP input file

# It will now be merged into a long-running DAT file

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
#  (1) "W2_Russian_River/Output/SRP_Meteorological_[startDate]_[endDate].csv"
#      The processed weather data

#  (2) A long-running DAT file (whose parent folder is input into 
#      "MAIN_SRP_DAT_FOLDER" of the control file)

#  (3) From the SRP model files, the "SRPHM_spinup.control" file will be edited

#  (4) Similarly, the model's "run_SRPHM_spinup.bat" file will be updated


# A single output will be generated in all cases, and additional outputs will 
# be included whenever the "Similar WY" procedure is executed: 

#  (1) "DAT_SRP_[startDate]_[endDate].dat"
#      The final DAT file to use in the model run
#      (This file is also copied to the "SRPHM" folder 
#       as "RR_SRP_Input.dat")

#  (2) If the similar WY needs to be identified, a summary CSV from that 
#      procedure will be generated as well

# Technically, "SRPHM_spinup.control" and "run_SRPHM_spinup.bat" are output by 
# this script as well
# (Though, the copied "SRP" contents will be deleted at the end of the model
#  run procedure, so they will not stick around for long)


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
  cat("Starting 'RRW_v2_009_Finalize_SRP_Input.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Import functions from the v1 PRMS and SRP scripts
  c("predictCurrentWY", "spiPrediction", "similarWYPrediction", "importLinModels",
    "similarWY_findWY", "similarWY_appendDAT") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_009_Finalize_PRMS_Input.R", .))
  
  c("updateMetadata_DAT", "finalizeDAT") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_014_Finalize_SRP_Input.R", .))
  
  
  # Confirm that a proper directory exists for model input and output files
  # The actual SRP model files should have been successfully copied to
  # the "Output" folder too
  cat(paste0("[1/", if_else(predictWY, 5, 4),
             "]\tChecking directories...\n"))
  
  
  # Check for the directory that contains metadata and model input/output files
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Also confirm that the SRP model folder was copied to "Output"
  srpPath <- validate_model_copy("SRPHM")
  
  
  cat("\tDone!\n\n")
  
  
  cat(paste0("[2/", if_else(predictWY, 5, 4),
             "]\tLoading meteorological data and long-running DAT file...\n"))
  
  
  # Read in two of the main input files
  # (The SRP Meteorological CSV and the primary DAT file)
  filePaths <- tibble("METEOROLOGICAL" = 
                        paste0("W2_Russian_River/Output/SRP_Meteorological_", startDate,
                               "_", endDate, ".csv"),
                      "MAIN_DAT" = getFromControl_RR("MAIN_SRP_DAT_FOLDER"))
  
  
  # "MAIN_DAT" contains a folder path right now
  # Extract the latest primary DAT file from there
  filePaths$MAIN_DAT[1] <- filePaths$MAIN_DAT[1] |>
    getLatestFile("^DAT_SRP_WY1948_to_WY[0-9]{4}\\.csv$",
                  "SRP Main DAT File")
  
  
  # Read in the two files next (while also verifying that they exist)
  meteorDF <- filePaths$METEOROLOGICAL[1] |> 
    checkForPreviousOutput() |>
    getDelim(",")
  
  
  primaryDAT <- getFile(filePaths$MAIN_DAT[1], delim = ",")
  
  
  # Validate the primary DAT file next
  # (This function also adds a "DATE" column to 'primaryDAT')
  # (That column enables matching with 'meteorDF')
  primaryDAT <- validateInputDAT(primaryDAT, filePaths$MAIN_DAT[1], "SRP", 
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
                                  startDate, endDate, "SRP", 
                                  names(meteorDF)[names(meteorDF) != "DATE"],
                                  dirPath, filePaths$MAIN_DAT[1])
    
    
    cat("\tDone!\n\n")
    
    
    # Even if predictions will not be used, 
    # update metadata in the hydrology folder
  } else {
    
    updateMetadata_DAT(dirPath, datStartDate = min(mergedDAT$DATE),
                       modelEndDate = endDate, 
                       predictionMethod = NA_character_, filePaths$MAIN_DAT[1])
    
  }
  
  
  cat(paste0("[", if_else(predictWY, "5/5", "4/4"),
             "]\tSaving output...\n"))
  
  
  # Finally, write 'mergedDAT' to a file
  # It will be stored in both the "SRPHM_update_ag" folder and the model run 
  # hydrology folder 
  mergedDAT |>
    outputDAT(startDate, endDate, dirPath, srpPath, predictWY)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_009_Finalize_SRP_Input.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



outputDAT <- function (mergedDAT, startDate, endDate, dirPath, srpPath, 
                       predictWY, quietly = FALSE) {
  
  # Write 'mergedDAT' to two folders:
  #  (1) In the hydrology directory, store the file under "SRP > Input"
  #  (2) In the copied SRP model files, store it under the "external_files" 
  #      folder within "SRPHM" 
  
  
  # The final filename of 'mergedDAT' will contain 'startDate' and 'endDate'
  datName <- paste0("DAT_SRP_", startDate, "_", endDate, ".dat")
  
  
  # 'datName' will appear in the hydrology folder only
  # In "SRPHM_update_ag", a generic name will be used instead
  genericName <- "RR_SRP_Input.dat"
  
  
  # Create a finalized version of 'mergedDAT'
  # This will be a vector of lines that will be written directly into a file
  finalDAT <- mergedDAT |>
    finalizeDAT()
  
  
  # Write 'finalDAT' to the hydrology folder first
  finalDAT |>
    writeOutput(paste0(dirPath, "/SRP/Input/", datName) |> 
                  normalizePath(mustWork = FALSE),
                writeFunction = "write_lines", quietly = quietly)
  
  
  # Write 'finalDAT' to the SRP model folder next
  # The name will be fixed as "RR_SRP_Input.dat" for ease of modeling automation
  finalDAT |>
    writeOutput(paste0(srpPath, "/external_files/", genericName) |> 
                  normalizePath(mustWork = FALSE),
                writeFunction = "write_lines", quietly = quietly)
  
  
  # Update the SRP control file next
  # (Its presence was already confirmed at the beginning of the script in 
  #  `validate_model_copy`)
  updateControlFileSRP(dirPath, srpPath, genericName, endDate, predictWY)
  
  
  # Update the SRP batch file
  updateBatchFileSRP(srpPath)
  
  
  # Finally, add metadata containing 'datName'
  updateMetadataCSV(dirPath,
                    list("SRP_FINAL_DAT_FILE_NAME" = datName))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



updateControlFileSRP <- function (dirPath, srpPath, datName, endDate, 
                                  predictWY) {
  
  # Update the fields in the "SRPHM_update.control" control file
  # This customizes the SRP model run
  
  # (Some metadata will be added at the end of the function too)
  
  
  # First, read in the file
  controlPath <- paste0(srpPath, "/", 
                        list_model_components("SRPHM")[["CONTROL"]]) |>
    normalizePath(mustWork = TRUE)
  
  
  srpControl <- controlPath |>
    getFile(fileType = "OTHER")
  
  
  # 'srpControl' is a vector of strings, with each element corresponding to a 
  # line of the control file
  # The parameters in these lines will be customized in preparation 
  # for the model run
  
  
  # The first parameter to update is the end date for the model run
  
  # This value will be based on 'predictWY'
  # If 'predictWY' is FALSE, use 'endDate'
  # Otherwise, use the end of the current water year
  if (predictWY) {
    
    modelEnd <- getModeledWY(endDate)[2]
    
  } else {
    
    modelEnd <- endDate
    
  }
  
  
  # Locate "end_time" in the control file
  targetLoc <- grep("^end_time$", srpControl)[1]
  
  
  # Three lines after "end_time", the next three lines are the components of 
  # the end date for the model run
  srpControl[targetLoc + 3] <- modelEnd |> year()
  srpControl[targetLoc + 4] <- modelEnd |> month() |> sprintf(fmt = "%.2d")
  srpControl[targetLoc + 5] <- modelEnd |> day() |> sprintf(fmt = "%.2d")
  
  
  # The next parameter to update is the name of the input DAT file
  # This will be a fixed name in all cases for ease of automation
  
  # This information is stored under the "data_file" parameter
  targetLoc <- grep("^data_file$", srpControl)[1]
  
  
  # Three lines after "data_file", the DAT filename is specified
  srpControl[targetLoc + 3] <- paste0("external_files\\", datName)
  
  
  # Write 'srpControl' back to a file (overwriting the previous version)
  writeOutput(srpControl, controlPath, quietly = TRUE)
  
  
  # Finally, save metadata about the model start date
  updateMetadataCSV(dirPath,
                    list("SRP_MODEL_START_DATE" = 
                           srpControl[grep("start_time", srpControl) + 3:5] |>
                           paste0(collapse = "-") |>
                           as.Date(format = "%Y-%m-%d")))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



updateBatchFileSRP <- function (srpPath) {
  
  # Update the "run_SRPHM_spinup.bat" file that initiates SRP
  
  
  # This file contains two commands:
  # cd [SRP FULL MODEL PATH]
  # [PATH TO GSFLOW.EXE] [PATH TO CONTROL FILE]
  
  
  batDir <- srpPath |>
    normalizePath(mustWork = TRUE)
  
  
  batchCommands <- c(paste0("cd ", shQuote(batDir)),
                     "bin\\gsflow.exe SRPHM_spinup.control")
  
  # The first command changes the working directory to the location of 
  # the SRP bat file (the root model directory in "SRPHM")
  # The second command then executes gsflow.exe using "SRPHM_spinup.control" 
  # (the latter is also located in the same directory as the bat file)
  
  
  # Write these commands to "run_SRPHM_spinup.bat"
  batchCommands |>
    writeOutput(paste0(srpPath, 
                       "/run_SRPHM_spinup.bat") |> 
                  normalizePath(mustWork = FALSE),
                quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

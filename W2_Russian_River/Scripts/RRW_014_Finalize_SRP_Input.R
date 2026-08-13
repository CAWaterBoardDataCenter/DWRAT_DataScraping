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

#  (3) From the SRP model files, the "SRPHM_update.control" file will be edited

#  (4) Similarly, the model's "Run_updated_Model.bat" file will be updated


# A single output will be generated in all cases, and additional outputs will 
# be included whenever the "Similar WY" procedure is executed: 

#  (1) "DAT_SRP_[startDate]_[endDate].dat"
#      The final DAT file to use in the model run
#      (This file is also copied to the "SRPHM_update_ag" folder 
#       as "RR_SRP_Input.dat")

#  (2) If the similar WY needs to be identified, a summary CSV from that 
#      procedure will be generated as well

# Technically, "SRPHM_update.control" and "Run_updated_Model.bat" are output by 
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
  cat("Starting 'RRW_014_Finalize_SRP_Input.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Import functions from the PRMS counterpart script
  c("predictCurrentWY", "spiPrediction", "similarWYPrediction", "importLinModels",
    "similarWY_findWY", "similarWY_appendDAT", "validate_num_stations") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_009_Finalize_PRMS_Input.R", .))
  
  
  # Confirm that a proper directory exists for model input and output files
  # The actual SRP model files should have been successfully copied to
  # the "Output" folder too
  cat(paste0("[1/", if_else(predictWY, 5, 4),
             "]\tChecking directories...\n"))
  
  
  # Check for the directory that contains metadata and model input/output files
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Also confirm that the SRP model folder was copied to "Output"
  srpPath <- validate_model_copy("SRP")
  
  
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
  
  
  
  # Before writing 'mergedDAT' to a file, make sure that it contains 
  # the correct number of precipitation and temperature stations
  mergedDAT |>
    validate_num_stations(srpPath, "SRP")
  
  
  cat(paste0("[", if_else(predictWY, "5/5", "4/4"),
             "]\tSaving output...\n"))
  
  
  # Finally, write 'mergedDAT' to a file
  # It will be stored in both the "SRPHM_update_ag" folder and the model run 
  # hydrology folder 
  mergedDAT |>
    outputDAT(startDate, endDate, dirPath, srpPath, predictWY)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_014_Finalize_SRP_Input.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



updateMetadata_DAT <- function (dirPath, datStartDate, modelEndDate, 
                                predictionMethod, pathMainDAT, 
                                pathPastPrecip = NA_character_, 
                                pathCurrentPrecip = NA_character_,
                                similarWY = NA_real_, 
                                linModel = list(m = NA_real_, b = NA_real_)) {
  
  # Update "metadata.csv" in the model run hydrology folder
  
  # Add information about the water year prediction method and the 
  # component DAT files
  
  
  updateMetadataCSV(dirPath,
                    newCols = list("SRP_MAIN_DAT_FILE" = pathMainDAT,
                                   "SRP_WY_PREDICTION_METHOD" = predictionMethod,
                                   "SRP_MODEL_DOMAIN_HISTORIC_PRECIP" = 
                                     pathPastPrecip,
                                   "SRP_MODEL_DOMAIN_CURRENT_WY_PRECIP" = 
                                     pathCurrentPrecip,
                                   "SRP_MOST_SIMILAR_WY" = similarWY,
                                   "SRP_REGRESSION_MODEL_SLOPE" = linModel$m,
                                   "SRP_REGRESSION_MODEL_INTERCEPT" = linModel$b,
                                   "SRP_DAT_START_DATE" = datStartDate, 
                                   "SRP_MODEL_END_DATE" = modelEndDate))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



outputDAT <- function (mergedDAT, startDate, endDate, dirPath, srpPath, 
                       predictWY, quietly = FALSE) {
  
  # Write 'mergedDAT' to two folders:
  #  (1) In the hydrology directory, store the file under "SRP > Input"
  #  (2) In the copied SRP model files, under "SRPHM_update_ag" 
  
  
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
    writeOutput(paste0(srpPath, "/", genericName) |> 
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



finalizeDAT <- function (mergedDAT) {
  
  # Reformat the DAT file into a structure suitable for writing it to a file
  
  # Right now, 'mergedDAT' is a tibble
  # However, it will be converted into a vector 
  # (with each element corresponding to a line in the eventual output file)
  
  # Thus, the columns must be bound together in a format that matches the
  # inconsistently fixed spacing of the SRP DAT files
  
  
  # Start with the following edits:
  
  #  (1) Remove the "DATE" column
  #  (2) Round every numeric value to four decimal places (exact)
  #  (3) Convert every column to character (needed for the next step)
  finalDAT <- mergedDAT |>
    select(-DATE) |>
    mutate(across(matches("^(PRE)|(TM)"), ~ round(., 4) |> 
                    sprintf(fmt = "%.4f"))) |>
    mutate(across(everything(), as.character))
  
  
  # The final format of 'finalDAT' will be a string with spaces separating
  # each of the column values
  
  # However, the number of spaces is inconsistent:
  
  #  (*) Between all of the datetime columns, there is only one space
  #  (*) Before and after "PRECIP1", there are five spaces
  #  (*) Between all subsequent columns, there are four spaces
  
  finalDAT <- finalDAT |>
    # Add a column to 'finalDAT' that merges the datetime columns
    # (with a single space of separation)
    unite(col = "DATETIME_MERGED",
          c("YEAR", "MONTH", "DAY", "HOUR", "MINUTE", "SECOND"), 
          sep = " ", remove = FALSE) |>
    # Add another column that combines "DATETIME_MERGED" with "PRECIP1"
    # (this time, there are five spaces of separation)
    unite(col = "DATETIME_PRECIP1_APPENDED",
          c("DATETIME_MERGED", matches("^PRECIP1(_REV[0-9]+)?$")),
          sep = str_dup(" ", 5), remove = FALSE) |>
    # Then, merge together the precipitation and temperature columns 
    # (ignoring "PRECIP1", which was already merged with the datetime values)
    # (There are four spaces of separation between these climate columns)
    unite(col = "OTHER_CLIMATE_COLS",
          matches("^(PRE)|(TM)") & !matches("^PRECIP1(_REV[0-9]+)?$"),
          sep = str_dup(" ", 4), remove = FALSE) |>
    # Finally, merge together "DATETIME_PRECIP1_APPENDED" and
    # "OTHER_CLIMATE_COLS"
    # Put five spaces of separation (so that "PRECIP1" has five spaces 
    # before and after its value)
    unite(col = "FINAL",
          c("DATETIME_PRECIP1_APPENDED", "OTHER_CLIMATE_COLS"),
          sep = str_dup(" ", 5), remove = TRUE)
  
  
  # Add a header to the DAT file next
  # It mainly describes the number of columns 
  headerDAT <- tibble(FINAL = c(paste0("generated in Excel/R : ",
                                       "1947-1980 USGS daily grid, ",
                                       "1981-present PRISM daily interp ",
                                       "station, Author: Pascual Benito ",
                                       "(pbenito@elmontgomery.com)"),
                                
                                paste0("precip ", names(mergedDAT) |> 
                                         str_subset("PRECIP") |> length()),
                                
                                paste0("tmax ", names(mergedDAT) |> 
                                         str_subset("TMAX") |> length()),
                                
                                paste0("tmin ", names(mergedDAT) |> 
                                         str_subset("TMIN") |> length()),
                                
                                c(str_dup("#", 19), names(mergedDAT)) |> 
                                  tolower() |> 
                                  str_subset("^date$", negate = TRUE) |>
                                  str_replace("second", "sec") |>
                                  str_replace("([a-z])([0-9])$", "\\10\\2") |>
                                  paste0(collapse = str_dup(" ", 10))))
  
  
  # Append 'headerDAT' to the beginning of 'finalDAT'
  finalDAT <- bind_rows(headerDAT,
                        finalDAT) |>
    select(FINAL)
  
  
  # Return the "FINAL" column in 'finalDAT' (as a vector)
  return(finalDAT$FINAL)
  
}



updateControlFileSRP <- function (dirPath, srpPath, datName, endDate, 
                                  predictWY) {
  
  # Update the fields in the "SRPHM_update.control" control file
  # This customizes the SRP model run
  
  # (Some metadata will be added at the end of the function too)
  
  
  # First, read in the file
  controlPath <- paste0(srpPath, "/",
                        list_model_components("SRP")[["CONTROL"]]) |>
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
  srpControl[targetLoc + 3] <- paste0(datName)
  
  
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
  
  # Update the "Run_updated_Model.bat" file that initiates SRP
  
  
  # This file contains two commands:
  # cd [SRP ROOT PATH]
  # call [PATH TO GSFLOW_AG.EXE] [PATH TO CONTROL FILE]
  
  
  batchCommands <- c(paste0("cd ", srpPath),
                     "call gsflow_ag.exe SRPHM_update.control")
  
  # The first command changes the working directory to the location of 
  # the SRP model files (the root directory of "SRPHM_update_ag")
  # The second command then executes gsflow_ag.exe using "SRPHM_update.control" 
  # (which is also located in the same directory)
  
  
  # Write these commands to "Run_updated_Model.bat"
  batchCommands |>
    writeOutput(paste0(srpPath, "/Run_updated_Model.bat") |> 
                  normalizePath(mustWork = FALSE),
                quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

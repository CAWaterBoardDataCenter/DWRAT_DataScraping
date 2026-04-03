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
#        (1) The "Most Similar Water Year" chosen for the SRP model run will 
#            be used here as well


# The script has several input files:
#  (1) "ProcessedData/SRP_Meteorological_[startDate]_[endDate].csv"
#      The processed weather data

#  (2) A long-running DAT file (whose filepath is input into "MAIN_SRP_DAT_FILE"
#      of the control file)

#  (3) A DAT file containing predictions for the current water year (its 
#      filepath should be given in "SRP_DAT_SPI_FILE" of the control file)




#  (4) From the SRP model files, the "prms_rr.control" file will be edited

#  (5) Similarly, the model's "run.bat" file will be updated as well


# A single output will be generated in all cases, and additional outputs will 
# be included whenever the "Similar WY" procedure is executed: 

#  (1) "ProcessedData/DAT_SRP_[startDate]_[endDate].dat"
#      The final DAT file to use in the model run
#      (This file is also copied to the "RR_SRP" folder as "RR_SRP_Input.dat")

#  (2) If the similar WY needs to be identified, components from that procedure
#      are generated as well

# Technically, "prms_rr.control" and "run.bat" are output by this script as well
# (Though, the copied "RR_SRP" contents will be deleted at the end of the model
#  run procedure, so they will not stick around for long)


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Supply_Validation_Functions.R")


#### Functions ####

mainProcedure <- function (predictWY = TRUE) {
  
  cat("\n\n")
  cat("Starting 'RRS_013_Finalize_SRP_Input.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that a proper directory exists for model input and output files
  # The actual SRP model files should have been successfully copied to
  # the "ProcessedData" folder too
  cat(paste0("[1/", if_else(predictWY, 5, 4),
             "]\tChecking directories...\n"))
  
  
  # Check for the directory that contains metadata and model input/output files
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Also confirm that the SRP model folder was copied to "ProcessedData"
  srpPath <- validateModelCopy_SRP()
  
  
  cat("\tDone!\n\n")
  
  
  cat(paste0("[2/", if_else(predictWY, 5, 4),
             "]\tLoading meteorological data and long-running DAT file...\n"))
  
  
  # Read in two of the main input files
  # (The SRP Meteorological CSV and the primary DAT file)
  filePaths <- c(paste0("ProcessedData/SRP_Meteorological_", startDate,
                        "_", endDate, ".csv"),
                 getFromSupplyControl_RR("MAIN_SRP_DAT_FILE"))
  
  
  # Read in the two files next (while also verifying that they exist)
  meteorDF <- filePaths[1] |> 
    checkForPreviousOutput() |> 
    getDelim(",")
  
  primaryDAT <- getFile(filePaths[2], ",")
  
  
  # Validate the primary DAT file next
  # (This function also adds a "DATE" column to 'primaryDAT')
  # (That column enables matching with 'meteorDF')
  primaryDAT <- validateInputDAT(primaryDAT, "MAIN_SRP_DAT_FILE", "SRP", 
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
    
    cat(paste0("[4/5]\tAppending forecasted predictions for ",
               "the current water year...\n"))
    
    
    mergedDAT <- predictCurrentWY(mergedDAT, startDate, endDate, 
                                  names(meteorDF)[names(meteorDF) != "DATE"],
                                  dirPath, srpPath, filePaths[2])
    
    
    cat("\tDone!\n\n")
    
    
    # Even if predictions will not be used, 
    # update metadata in the hydrology folder
  } else {
    
    updateMetadata_DAT(dirPath, NA_character_, filePaths[2], endDate)
    
  }
  
  
  cat(paste0("[", if_else(predictWY, "5/5", "4/4"),
             "]\tSaving output...\n"))
  
  
  # Finally, write 'mergedDAT' to a file
  # It will be stored in both the "RR_SRP" folder and the model run 
  # output folder 
  mergedDAT |>
    outputDAT(startDate, endDate, dirPath, srpPath, predictWY)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRS_013_Finalize_SRP_Input.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



predictCurrentWY <- function (mergedDAT, startDate, endDate, srpCols,
                              dirPath, srpPath, pathMainDAT) {
  
  # Based on 'endDate', apply different methods to select predictions 
  # to append to 'mergedDAT'
  
  
  # For October - February, use the SPI prediction method
  if (month(endDate) < 3 || month(endDate) > 9) {
    
    # Append SPI data to 'mergedDAT'
    finalDAT <- spiPrediction(mergedDAT, startDate, endDate, srpCols)
    
    
    # Update the metadata file next
    updateMetadata_DAT(dirPath, "SPI", pathMainDAT, getModeledWY(endDate)[2],
                       pathSPI = pathSPI)
    
    
    # If 'endDate' is within March - September, the most similar WY will be used
  } else {
    
    # But first confirm that 'endDate' is not the end of the current water year
    # If that is the case, no predictions are needed
    if (endDate == getModeledWY(endDate)[2]) {
      
      # No additional data is needed for the water year
      finalDAT <- mergedDAT
      
      
      # Update the metadata file after that
      updateMetadata_DAT(dirPath, "Not Required", pathMainDAT, 
                         getModeledWY(endDate)[2])
      
    } else {
      
      # Otherwise, perform all operations for the similar water year procedure
      # in a separate function 
      finalDAT <- similarWYPrediction(mergedDAT, startDate, endDate,
                                      dirPath, srpPath, pathMainDAT)
      
      # The metadata will be updated in that function too
      
    }
    
  }
  
  
  # Make sure the "Runoff" columns all contain "1" for every row
  # (The meteorological dataset does not have these columns, and that causes
  #  "NA" entries to appear)
  finalDAT <- finalDAT |>
    mutate(across(starts_with("RUNOFF"), ~replace_na(., 1)))
  
  
  # Perform a few checks on 'finalDAT'
  validateInputDAT(finalDAT, sourceField = NULL, "SRP", srpCols,
                   startDate, endDate, datType = "Final")
  
  
  # Then, return 'finalDAT'
  return(finalDAT)
  
}



spiPrediction <- function (mergedDAT, startDate, endDate, srpCols) {
  
  # Use the Standard Precipitation Index (SPI) to predict precipitation
  # and temperature for the rest of the water year
  
  
  # At this point in time, insufficient precipitation data is available
  # Predictions using the SPI will be appended to the DAT file
  pathSPI <- getFromSupplyControl_RR("SRP_DAT_SPI_FILE")
  
  
  spiDAT <- pathSPI |>
    getFile(",")
  
  
  # Validate the DAT file before continuing
  spiDAT <- validateInputDAT(spiDAT, "SRP_DAT_SPI_FILE", "SRP", srpCols, 
                             startDate, endDate, datType = "SPI")
  
  
  # Filter 'spiDAT' to dates after the last date in 'mergedDAT'
  # Then, append it to 'mergedDAT'
  finalDAT <- mergedDAT |>
    bind_rows(spiDAT |> filter(DATE > max(mergedDAT$DATE)))
  
  
  return(finalDAT)
  
}



updateMetadata_DAT <- function (dirPath, predictionMethod, pathMainDAT, 
                                modelEndDate, pathSPI = NA_character_, 
                                similarWY = NA_real_, linModel = NULL) {
  
  # Update "metadata.csv" in the model run hydrology folder
  
  # Add information about the water year prediction method and the 
  # component DAT files
  
  
  updateMetadataCSV(dirPath,
                    newCols = list("SRP_MAIN_DAT_FILE" = pathMainDAT,
                                   "SRP_SPI_DAT_FILE" = pathSPI,
                                   #"WY_PREDICTION_METHOD" = predictionMethod,
                                   #"MOST_SIMILAR_WY" = similarWY,
                                   #"REGRESSION_MODEL_SLOPE" = 
                                  #   if_else(is.null(linModel),
                                  #           NA_real_, linModel$m),
                                  # "REGRESSION_MODEL_INTERCEPT" = 
                                  #   if_else(is.null(linModel),
                                  #           NA_real_, linModel$b),
                                  # "MODEL_END_DATE" = modelEndDate
                                  ))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



similarWYPrediction <- function (mergedDAT, startDate, endDate, 
                                 dirPath, srpPath, pathMainDAT) {
  
  # Use 'mergedDAT' as-is without any predictions appended and 
  # prepare for a model run
  
  # Run SRP to get the "rr_budget.out2" file
  
  # Then, apply a regression model to identify the most similar water year
  # The hard-coded model coefficients are here:
  linModel <- list("FEB" = list(m = 1.06123659152113, b = 3.84494429543525),
                   "MAR" = list(m = 1.08706793979007, b = 0.833608488137486),
                   "APR" = list(m = 1.02259029456379, b = 0.627031577682994),
                   "MAY" = list(m = 1.00332226548437, b = 0.263608157053314),
                   "JUN" = list(m = 0.998668183546254, b = 0.148860933464808),
                   "JUL" = list(m = 0.998945430679489, b = 0.12814054299168),
                   "AUG" = list(m = 0.998332437810631, b = 0.118465777984958))
  
  
  # The model to use depends on the current month in 'endDate'
  # The chosen model will cover October to the previous month
  linModel <- linModel[[toupper(month.abb[month(endDate) - 1])]]
  
  
  # Perform the model run first
  
  # Prepare a DAT file that extends only up to 'endDate'
  outputDAT(mergedDAT, startDate, endDate, dirPath, srpPath, 
            predictWY = FALSE, quietly = TRUE)
  
  
  # Run SRP next
  similarWY_runSRP()
  
  
  # The "rr_budget.out2" file from this process is needed for the analysis
  outDF <- similarWY_processOut2(srpPath, dirPath)
  
  
  # Find the most similar water year for the current water year after that
  similarWY <- similarWY_findWY(endDate, outDF, dirPath, 
                                month(endDate) - 1, linModel)
  
  
  # Create a final DAT tibble after that using the similar water year
  finalDAT <- similarWY_appendDAT(mergedDAT, endDate, similarWY)
  
  
  # Finally, update the metadata file
  updateMetadata_DAT(dirPath, "WY", pathMainDAT, getModeledWY(endDate)[2], 
                     similarWY = similarWY, linModel = linModel)
  
  
  # Return 'finalDAT'
  return(finalDAT)
  
}



outputDAT <- function (mergedDAT, startDate, endDate, dirPath, srpPath, 
                       predictWY, quietly = FALSE) {
  
  # Write 'mergedDAT' to two folders:
  #  (1) In the hydrology directory, store the file under "SRP > Input"
  #  (2) In the copied SRP model files, 
  #      under "SRP > input > climate_scenarios" 
  
  
  # The final filename of 'mergedDAT' will contain 'startDate', 'endDate', and
  # the name of the user running this script
  datName <- paste0("DAT_SRP_", Sys.info()[["user"]], "_", startDate, 
                    "_", endDate, ".dat")
  
  
  # 'datName' will appear in the hydrology folder only
  # In "SRPHM_update_ag", a generic name will be used instead
  genericName <- "RR_SRP_Input.dat"
  
  
  # Create a finalized version of 'mergedDAT':
  #  (1) Remove the "DATE" column
  #  (2) Round every numeric value to four decimal places (at most)
  #  (3) Convert every column to character (needed for the next step)
  finalDAT <- mergedDAT |>
    select(-DATE) |>
    mutate(across(where(is.numeric), ~ round(., 4))) |>
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
          c("DATETIME_MERGED", "PRECIP1"),
          sep = str_dup(" ", 5), remove = FALSE) |>
    # Then, merge together the precipitation and temperature columns 
    # (ignoring "PRECIP1", which was already merged with the datetime values)
    # (There are four spaces of separation between these climate columns)
    unite(col = "OTHER_CLIMATE_COLS",
          matches("^(PRE)|(TM)") & !matches("^PRECIP1$"),
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
  headerDAT <- tibble(FINAL = c(paste0("Originally generated in Excel : ",
                                      "1947-1980 USGS daily grid, 1981-2018 ",
                                      "PRISM daily interp station, Author: ",
                                      "Pascual Benito ",
                                      "(pbenito@elmontgomery.com)"),
                               paste0("precip ", names(mergedDAT) |> 
                                        str_subset("PRECIP") |> length()),
                               paste0("tmax ", names(mergedDAT) |> 
                                        str_subset("TMAX") |> length()),
                               paste0("tmin ", names(mergedDAT) |> 
                                        str_subset("TMIN") |> length()),
                               c("###################", names(mergedDAT)) |> 
                                 tolower() |> str_replace("second", "sec") |>
                                 str_replace("([a-z])([0-9])$", "\\10\\2") |>
                                 paste0(collapse = str_dup(" ", 10))))
  
  
  finalDAT <- bind_rows(headerDAT,
                        finalDAT) |>
    select(FINAL)
  
  
  # Write 'finalDAT' to the hydrology folder first
  finalDAT$FINAL |>
    writeOutput(paste0(dirPath, "/SRP/Input/", datName) |> 
                  normalizePath(mustWork = FALSE),
                writeFunction = "write_lines", quietly = quietly)
  
  
  # Write 'finalDAT' to the SRP model folder next
  # The name will be fixed as "RR_SRP_Input.dat" 
  # for ease of modeling automation
  finalDAT$FINAL |>
    writeOutput(paste0(srpPath, "/", genericName) |> 
                  normalizePath(mustWork = FALSE),
                writeFunction = "write_lines", quietly = quietly)
  
  
  # Update the SRP control file next
  # (Its presence was already confirmed at the beginning of the script in 
  #  `validateModelCopy_SRP`)
  updateControlFileSRP(srpPath, genericName, endDate, predictWY)
  
  
  # Finally, update the SRP batch file
  updateBatchFileSRP(srpPath)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



updateControlFileSRP <- function (srpPath, datName, endDate, predictWY) {
  
  # Update the fields in the "prms_rr" control file
  # This customizes the SRP model run
  
  
  # First, read in the file
  controlPath <- paste0(srpPath, "/SRPHM_update.control") |>
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
  writeOutput(srpControl, controlPath, "write_lines", quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



updateBatchFileSRP <- function (srpPath) {
  
  # Update the "run.bat" file that initiates SRP
  
  
  # This file contains two commands:
  # cd [PATH TO "SRPHM_update_ag" FOLDER]
  # call [PATH TO "gsflow_ag.exe"] [PATH TO CONTROL FILE]
  
  
  batchCommands <- c(paste0("cd ", srpPath),
                     "call gsflow_ag.exe SRPHM_update.control")
  
  # The first command changes the working directory to the SRP model folder
  # The second command then executes gsflow_ag.exe using "SRPHM_update.control" 
  # (which is also located in the root directory)
  
  
  # Write these commands to "Run_updated_Model.bat"
  batchCommands |>
    writeOutput(paste0(srpPath, "/Run_updated_Model.bat") |> 
                  normalizePath(mustWork = FALSE),
                "write_lines", quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



similarWY_runSRP <- function () {
  
  # To find the most similar water year, a SRP run is required
  
  
  # Start by notifying the user about this run
  message(paste0("To find the most similar water year, a preliminary SRP ",
                 "model run will be performed."))
  
  
  # This script will run the "Run_SRP" script next
  prmsScript <- "Scripts/RRS_009_Run_SRP.R"
  
  
  # But before doing so, edit the next script to disable the 
  # environment-clearing commands
  scriptCode <- getFile(prmsScript, fileType = "OTHER")
  
  
  # Locate the two lines of code that clear the environment
  clearIndices <- grep("^[#]*\\s*remove\\(list = ls\\(\\)\\)\\s*$", scriptCode)
  
  
  # Add a "#" to the beginning of those lines to comment them out
  scriptCode[clearIndices] <- scriptCode[clearIndices] |>
    str_replace("^(.+)$", "#\\1")
  
  
  # Save the updates to the script
  writeOutput(scriptCode, prmsScript, "write_lines", quietly = TRUE)
  
  
  # Next, run the SRP execution script
  source(prmsScript)
  
  
  # Once that script is complete, revert the environment-clearing code to its
  # original state (i.e., uncomment those lines)
  scriptCode[clearIndices] <- scriptCode[clearIndices] |>
    str_remove("^[#]+")
  
  
  # Save the reverted version
  writeOutput(scriptCode, prmsScript, "write_lines", quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



similarWY_processOut2 <- function (srpPath, dirPath) {
  
  # SRP was successfully run for the watershed
  
  # The next step is to process its "rr_budget.out2" file
  
  
  # Before starting that, copy the "rr_budget.out2" file from the SRP "output"
  # folder and save it into the hydrology folder
  out2Path <- paste0(srpPath, "/SRP/output/rr_budget.out2") |>
    normalizePath(mustWork = TRUE)
  
  
  newOutPath <- paste0(dirPath, "/SRP/Input/SimilarWY_NoPredict_rr_budget.out2")
  
  
  copyRes <- file.copy(from = out2Path, to = newOutPath, 
                       overwrite = TRUE)
  
  
  # Verify that the file copied successfully
  # If not, output an error message
  if (!copyRes || !file.exists(newOutPath)) {
    
    stop(paste0("Could Not Copy Out2 File\n\n",
                "The script attempted to copy the \"rr_budget.out2\" ",
                "file (\"", out2Path, "\") to the new output directory.\n\n",
                "However, the processed failed for an unknown reason ",
                "(possibly a permission issue). Please investigate.\n\n",
                "The intended new file was: \"", newOutPath, "\"") |>
           errWrap())
    
  }
  
  
  # Read in the .out2 file next
  outDF <- read_out2(out2Path)
  
  
  # Confirm that "Year", "mo", "day", and "ppt (in)" are columns in 'outDF'
  # (The names of the elements in this vector are the planned column names)
  # (The elements themselves are the current expected column names)
  expectedCols <- c("YEAR" = "Year",
                    "MONTH" = "mo",
                    "DAY" = "day",
                    "PRECIP" = "ppt (in)")
  
  
  if (anyFalse(expectedCols %in% names(outDF))) {
    
    missingColumns <- which(!(expectedCols %in% names(outDF)))
    
    
    stop(paste0("Missing Columns in Out2 File\n\n",
                "Several of the expected columns in SRP's \"rr_budget.out2\" ",
                "output file could not be found (",
                expectedCols[missingColumns] |> vec2QuotedStr(),
                "). Please investigate the file and update the script if ",
                "needed.\n\n",
                "(This error occurred for \"", out2Path, "\")") |>
           errWrap())
    
  }
  
  
  # Rename these columns in 'outDF' and return only those columns
  return(outDF |>
           select(all_of(expectedCols)))
  
}



similarWY_findWY <- function (endDate, outDF, dirPath, endMonth, linModel) {
  
  # In WY2024, SDA staff developed a calibrated and validated linear regression
  # model that linked Oct - Feb Precipitation to Total WY Precipitation
  
  # In WY2026, similar models were developed for Oct - Mar, Oct - Apr, Oct - May,
  # Oct - Jun, Oct - July, and Oct - Aug using data for WY2025
  
  # These models will now be applied here to find the most similar water year
  # for the current water year
  
  # Given the partial precipitation, the total precipitation for the water year
  # will be estimated
  
  # Then, this predicted total will be compared to the total precipitation
  # in previous water years
  
  # The "most similar water year" will have a total precipitation closest to 
  # the current water year's predicted total
  
  
  # First, make sure that 'endMonth' is a valid value
  # It should correspond to a month between February and August
  if (!(endMonth %in% 2:8)) {
    
    stop(paste0("Script Error - Invalid 'endMonth'\n\n",
                "The \"Similar Water Year\" method uses data from October to ",
                "a month between February and August (inclusive) to predict ",
                "the total precipitation for the current water year. As a ",
                "result, the input \"endMonth\" should have a value between 2 ",
                "and 8 (inclusive). However, \"", endMonth, "\" was provided ",
                "to `similarWY_findWY` instead. Please revise the script.") |>
           errWrap())
    
  }
  
  
  # Get the current water year 
  currentWY <- getModeledWY(endDate)[2] |> year()
  
  
  # Adjust the formatting of 'outDF'
  # Add water year and date columns
  outDF <- outDF |>
    mutate(WY = if_else(MONTH < 10, YEAR, YEAR + 1),
           DATE = paste0(YEAR, "-", MONTH, "-", DAY) |>
             as.Date(format = "%Y-%m-%d")) |>
    filter(!is.na(PRECIP)) |>
    arrange(DATE)
  
  
  # Calculate the partial precipitation for the current water year
  # It will be Oct - Feb/Mar/Apr/May/Jun/Jul/Aug
  partialPrecip <- outDF |>
    filter(WY == currentWY) |>
    filter(MONTH > 9 | MONTH <= endMonth) |>
    select(PRECIP) |>
    sum()
  
  
  # Apply the linear regression model to get the predicted total this year
  predictedPrecip <- linModel$m * partialPrecip + linModel$b
  
  
  # Before finding the most similar water year in 'outDF', some additional
  # adjustments are necessary first
  
  # Only keep water years in 'outDF' that have a complete set of data
  # (Ignore the current water year too)
  
  # To determine which water years are missing data, two approaches will be used
  
  # First, a count of days for every water year will be established
  # Any year with less than 365 days of data is incomplete
  # These water years should be removed from 'outDF'
  countDF <- outDF |>
    group_by(WY) |>
    summarize(COUNT = n()) |>
    filter(COUNT < 365)
  
  
  # Water years with incomplete data will be removed from 'outDF'
  # (At this step, the current water year will be excluded too)
  outDF <- outDF |>
    filter(!(WY %in% countDF$WY | WY == currentWY))
  
  
  # The second method to determine which water years are missing data will rely
  # on a separate tibble of expected dates
  # (This extra check is needed because some years should have 365 days 
  #  and others should have 366. It could be cleaner to just verify that water  
  #  years have 365 days by default and require 366 if YEAR %% 4 == 0, but 
  #  this method can also weed out instances where a date is missing and another 
  #  date is duplicated in the same water year--it is unlikely to happen though)
  dateDF <- tibble(DATE = seq(from = min(outDF$DATE), 
                              to = max(outDF$DATE), 
                              by = "days")) |>
    mutate(WY = if_else(month(DATE) < 10, year(DATE), year(DATE) + 1))
  
  
  # Figure out which dates are missing in 'outDF' from 'dateDF'
  missingDates <- which(!(dateDF$DATE %in% outDF$DATE))
  
  
  # If missing dates are found, remove their water years from 'outDF'
  if (length(missingDates) > 0) {
    
    # Identify the water years that correspond to the missing dates
    incompleteWYs <- dateDF$WY[missingDates] |> unique()
    
    
    # Remove those water years from 'outDF'
    outDF <- outDF |>
      filter(!(WY %in% incompleteWYs))
    
  }
  
  
  # Now that all incomplete water years have been excluded, calculate
  # the partial precipitation and total water year precipitation for
  # every water year in 'outDF'
  precipDF <- outDF |>
    group_by(WY) |>
    summarize(!! paste0("OCT_TO_", toupper(month.abb[endMonth]), 
                        "_PARTIAL_PRECIP") := 
                sum(PRECIP[MONTH > 9 | MONTH <= endMonth]),
              TOTAL_WY_PRECIP = sum(PRECIP), 
              .groups = "drop")
  
  
  # Using 'predictedPrecip', calculate the absolute difference between 
  # every value of "TOTAL_WY_PRECIP" and this predicted total
  precipDF <- precipDF |>
    mutate(ERROR = abs(TOTAL_WY_PRECIP - predictedPrecip))
  
  
  # Identify which water year has the smallest absolute difference
  # (If multiple water years have the same minimum error, the most recent water
  #  year will be chosen. Its data quality will be higher and more accurate.)
  similarWY <- precipDF$WY[which.min(precipDF$ERROR)] |> max()
  
  
  # Before returning 'similarWY', this information will be output into a 
  # spreadsheet in the hydrology model input/output folder
  
  # Append additional information to 'precipDF':
  #  (1): The current water year's information will be appended at the end
  #       (It has a partial precipitation value like the other columns, 
  #        but its total water year is a prediction, so that will be listed
  #        in a separate column)
  #  (2): Identify the years that have the minimum error
  #  (3): Identify the year labeled as "most similar"
  
  # After that, rearrange the columns in 'precipDF' and make sure it is sorted
  precipDF <- precipDF |>
    bind_rows(tibble(WY = currentWY,
                     !! paste0("OCT_TO_", toupper(month.abb[endMonth]), 
                               "_PARTIAL_PRECIP") := partialPrecip,
                     PREDICTED_TOTAL_WY_PRECIP = predictedPrecip)) |>
    mutate(MINIMUM_ABS_DIFFERENCE_ERROR = !is.na(ERROR) &
             ERROR == min(ERROR, na.rm = TRUE),
           MOST_SIMILAR_WY = (WY == similarWY)) |>
    select(WY, paste0("OCT_TO_", toupper(month.abb[endMonth]), 
                      "_PARTIAL_PRECIP"), TOTAL_WY_PRECIP,
           PREDICTED_TOTAL_WY_PRECIP, ERROR, MINIMUM_ABS_DIFFERENCE_ERROR,
           MOST_SIMILAR_WY) |>
    arrange(WY)
  
  
  # Write 'precipDF' as a CSV file to 'dirPath' next
  precipDF |>
    writeOutput(paste0(dirPath, "/SRP/Input/SimilarWY_Analysis.csv") |>
                  normalizePath(mustWork = FALSE),
                "write_csv")
  
  
  # Finally, return 'similarWY'
  return(similarWY)
  
}



similarWY_appendDAT <- function (mergedDAT, endDate, similarWY) {
  
  # Copy the data in 'mergedDAT' from a previous water year and 
  # use it as predictions for the rest of the current water year
  
  
  # Get the current water year's bounds
  currentWY <- getModeledWY(endDate)
  
  
  # Extract data from 'mergedDAT' for the water year identified in 'similarWY'
  wyDAT <- mergedDAT |>
    filter(DATE >= paste0(similarWY - 1, "-10-01") &
             DATE <= paste0(similarWY, "-09-30"))
  
  
  # Adjust the "YEAR" and "DATE" columns to be for the current water year
  wyDAT <- wyDAT |>
    mutate(YEAR = if_else(MONTH < 10, year(currentWY[2]), year(currentWY[1]))) |>
    mutate(DATE = paste0(YEAR, "-", MONTH, "-", DAY) |> 
             as.Date(format = "%Y-%m-%d"))
  
  
  # Filter 'wyDAT' to after 'endDate'
  wyDAT <- wyDAT |>
    filter(DATE > endDate)
  
  
  # Append 'wyDAT' to 'mergedDAT' and return it
  return(mergedDAT |>
           bind_rows(wyDAT))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

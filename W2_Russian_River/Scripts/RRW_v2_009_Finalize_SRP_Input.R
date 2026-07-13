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
source("W2_Russian_River/Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function (predictWY = TRUE) {
  
  cat("\n\n")
  cat("Starting 'RRW_v2_009_Finalize_SRP_Input.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that a proper directory exists for model input and output files
  # The actual SRP model files should have been successfully copied to
  # the "Output" folder too
  cat(paste0("[1/", if_else(predictWY, 5, 4),
             "]\tChecking directories...\n"))
  
  
  # Check for the directory that contains metadata and model input/output files
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Also confirm that the SRP model folder was copied to "Output"
  srpPath <- validateModelCopy_SRP_2024()
  
  
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
                                  startDate, endDate, 
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



predictCurrentWY <- function (mergedDAT, startDate, endDate, srpCols,
                              dirPath, pathMainDAT) {
  
  # Based on 'endDate', apply different methods to select predictions 
  # to append to 'mergedDAT'
  
  
  # Before doing anything else, check if 'endDate' is equal to the 
  # end of the current water year
  # (No predictions are required in this edge case)
  if (endDate == getModeledWY(endDate)[2]) {
    
    # The final DAT file will be an unchanged copy of 'mergedDAT'
    finalDAT <- mergedDAT
    
    
    # But still update the metadata file after that
    updateMetadata_DAT(dirPath, datStartDate = min(finalDAT$DATE), 
                       modelEndDate = endDate, 
                       "Not Required", pathMainDAT)
    
  } else {
    
    # Otherwise, a prediction method is required
    
    # One of two methods will be used:
    
    # (*) SPI-based prediction
    # (*) "Most Similar WY" prediction
    
    # In both cases, historic precipitation data is required for
    # the SRP model domain
    
    # Get the path to that file
    pastPrecipPath <- getFromControl_RR("PRISM_SRP_HISTORIC_PRECIP_FOLDER") |>
      getLatestFile(paste0("^RR_Workflow_PRISM_SRP_Avg_Historic_Precip_",
                           "CY1981_to_WY[0-9]{4}\\.csv$"),
                    "SRP Historic Precip File")
    
    
    # Read in the file and validate it
    pastPrecip <- pastPrecipPath |>
      getFile()
    
    pastPrecip |>
      validateHistoricPrecipFile(pastPrecipPath,
                                 getModeledWY(endDate)[1])
    
    
    # For October - February, use the SPI prediction method
    if (month(endDate) < 3 || month(endDate) > 9) {
      
      # Use 'pastPrecip' to calculate the Standard Precipitation Index
      # Then, choose months with the driest conditions and use them 
      # as predictions for the remaining months of the current water year
      finalDAT <- spiPrediction(mergedDAT, pastPrecip, 
                                startDate, endDate, srpCols)
      
      
      # Update the metadata file next
      updateMetadata_DAT(dirPath, datStartDate = min(finalDAT$DATE),
                         modelEndDate = getModeledWY(endDate)[2],
                         predictionMethod = "SPI", 
                         pathMainDAT = pathMainDAT, 
                         pathPastPrecip = pastPrecipPath)
      
      # If 'endDate' is within March - September, the most similar WY will be used
    } else {
      
      
      # In a separate function, the most similar water year will be identified
      # using a linear regression model and data downloaded from PRISM in a 
      # previous script
      finalDAT <- similarWYPrediction(mergedDAT, pastPrecip,
                                      endDate, dirPath, pathMainDAT,
                                      pastPrecipPath)
      
      # The metadata will be updated in that function too
      
    }
    
    
    # For both the SPI and Similar Water Year methods, archive 'pastPrecipPath'
    copyFile(pastPrecipPath, 
             paste0(dirPath, "/SRP/Input/",
                    pastPrecipPath |> str_remove("^.+[/\\\\]")) |>
               normalizePath(mustWork = FALSE), 
             quietly = TRUE)
    
  }
  
  
  # Perform a few checks on 'finalDAT'
  validateInputDAT(finalDAT, sourcePath = NULL, "SRP", srpCols,
                   startDate, endDate, datType = "Final")
  
  
  # Then, return 'finalDAT'
  return(finalDAT)
  
}



spiPrediction <- function (mergedDAT, pastPrecip, startDate, endDate, srpCols) {
  
  # Use the 12-month Standard Precipitation Index (SPI) to predict precipitation
  # and temperature for the rest of the water year
  
  # At this point in time, insufficient precipitation data is available
  
  # The "worst case scenario" from past months will be used to fill in the 
  # missing data gaps
  
  
  # Get the current water year bounds
  wyBounds <- getModeledWY(endDate)
  
  
  # The next step is to summarize 'pastPrecip' on a monthly timescale
  
  # Before doing that, certain filters must be applied
  # Ignore records from the current water year onwards
  pastPrecip <- pastPrecip |>
    filter(Date < wyBounds[1])
  
  
  # Make sure the last day in 'pastPrecip' is September 30th
  if (month(max(pastPrecip$Date)) != 9 || day(max(pastPrecip$Date)) != 30) {
    
    # If not, find the latest instance of September 30th and filter to that bound
    
    # Due to prior validation checks, 'pastPrecip' should be a continuous dataset
    # If there is no September 30th in the latest year in 'pastPrecip',  
    # there will definitely be one in the prior year
    
    # Use whichever one is available
    
    if (paste0(year(max(pastPrecip$Date)), "-09-30") %in% pastPrecip$Date) {
      
      pastPrecip <- pastPrecip |>
        filter(Date <= paste0(year(max(pastPrecip$Date)), "-09-30"))
      
    } else {
      
      pastPrecip <- pastPrecip |>
        filter(Date <= paste0(year(max(pastPrecip$Date)) - 1, "-09-30"))
      
    }
    
  }
  
  
  # After that, summarize 'pastPrecip' into a monthly dataset
  monthDF <- pastPrecip |>
    mutate(YEAR = year(Date), MONTH = month(Date)) |>
    group_by(YEAR, MONTH) |>
    summarize(PRECIP = sum(`ppt (mm)`), .groups = "drop") |>
    arrange(YEAR, MONTH)
  
  
  # Add dummy entries for the last three months of the year in 'monthDF'
  # (This data will round out 'monthDF' into 12 months of data per year)
  # (Also, "YEAR_MONTH" is an extra column that will be useful later)
  dummyDF <- tibble(YEAR = max(monthDF$YEAR),
                    MONTH = 10:12,
                    PRECIP = 0) |>
    mutate(YEAR_MONTH = paste0(YEAR, "-", MONTH))
  
  
  monthDF <- monthDF |>
    bind_rows(dummyDF)
  
  
  # Calculate the twelve-month scale SPI for this dataset
  spiRes <- spi(monthDF$PRECIP, scale = 12, verbose = TRUE)
  
  
  # Add 'spiRes' to a new column in 'monthDF'
  monthDF <- monthDF |>
    mutate(SPI = spiRes[["fitted"]])
  
  
  # For the next step, exclude certain entries from 'monthDF': 
  #  (*) Year-month pairs with a "NA" value for "SPI"
  #  (*) Year-month pairs present in 'dummyDF'
  #  (*) Year-month pairs that are not present in 'mergedDF'
  
  # Define a "YEAR-MONTH" column to help with these edits
  monthDF <- monthDF |>
    filter(!is.na(SPI)) |>
    mutate(YEAR_MONTH = paste0(YEAR, "-", MONTH)) |>
    filter(!(YEAR_MONTH %in% dummyDF$YEAR_MONTH)) |>
    filter(YEAR_MONTH %in% paste0(mergedDAT$YEAR, "-", mergedDAT$MONTH))
  
  
  # For each month, identify the years with the lowest "SPI" value
  monthDF <- monthDF |>
    group_by(MONTH) |>
    filter(SPI == min(SPI)) |>
    filter(YEAR == max(YEAR)) |>
    arrange(MONTH) |>
    ungroup()
  
  # The filter for max "YEAR" is just in case there is a tie for  
  # any of the months
  # The more recent year will be chosen among the options 
  
  
  # After that, extract entries from 'mergedDAT' for this SPI dataset
  # Get the relevant entries with matching "YEAR-MONTH" pairs
  # Then, alter "YEAR" to match the current water year
  spiDAT <- mergedDAT |>
    mutate(YEAR_MONTH = paste0(YEAR, "-", MONTH)) |>
    filter(YEAR_MONTH %in% monthDF$YEAR_MONTH) |>
    select(-YEAR_MONTH) |>
    mutate(YEAR = if_else(MONTH < 10, year(wyBounds[2]), year(wyBounds[1])))
  
  
  # One final check is for leap years
  
  # If February 29th does not exist in the current water year, 
  # ensure that it is not present in 'spiDAT'
  if (is.na(paste0(year(wyBounds[2]), "-02-29") |>
            as.Date(format = "%Y-%m-%d"))) {
    
    # Remove any entries for February 29th
    spiDAT <- spiDAT |>
      filter(!(MONTH == 2 & DAY == 29))
    
    # However, if February 29th is present in the modeled water year,
    # double-check that an entry is present for that date
  } else {
    
    # If no row is present, duplicate the row for February 28th
    # and use it for February 29th as well
    if (nrow(spiDAT |> filter(MONTH == 2 & DAY == 29)) == 0) {
      
      # Bind the altered entry for February 28th to 'spiDAT'
      spiDAT <- spiDAT |>
        bind_rows(spiDAT[spiDAT$MONTH == 2 & spiDAT$DAY == 28, ] |>
                    mutate(DAY = 29))
      
    }
    
  }
  
  
  # Sort 'spiDAT' and make sure the "DATE" column is accurate
  spiDAT <- spiDAT |>
    arrange(YEAR, MONTH, DAY) |>
    mutate(DATE = paste0(YEAR, "-", MONTH, "-", DAY) |>
             as.Date(format = "%Y-%m-%d"))
  
  
  # Validate the DAT file before continuing
  spiDAT <- validateInputDAT(spiDAT, NA_character_, "SRP", srpCols, 
                             startDate, endDate, datType = "SPI")
  
  
  # Filter 'spiDAT' to dates after the last date in 'mergedDAT'
  # Then, append it to 'mergedDAT'
  finalDAT <- mergedDAT |>
    bind_rows(spiDAT |> filter(DATE > max(mergedDAT$DATE)))
  
  
  return(finalDAT)
  
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



similarWYPrediction <- function (mergedDAT, pastPrecip, endDate, 
                                 dirPath, pathMainDAT, pathPastPrecip) {
  
  # Use data downloaded from PRISM for the SRP model bounds
  # in a linear regression model to identify which past water year 
  # is most similar to the current water year's conditions
  
  # That similar water year's data will be used for the remainder of the 
  # current water year
  
  
  # There are three different models that can be applied
  # (The selection is based on 'endDate')
  #  (*) "October - February" will be used in March
  #  (*) "October - March" will be used in April
  #  (*) "October - April" will be used in May - September
  
  
  # The hard-coded model coefficients are here:
  linModel <- list("FEB" = list(m = 1.18889417583887, b = 127.497807635486),
                   "MAR" = list(m = 1.09263349165030, b = 28.1349370455440),
                   "APR" = list(m = 1.01611885039427, b = 33.7042053778413))
  
  
  # The model to use depends on the current month in 'endDate'
  # Get the index of the previous month
  selectedMonth <- month(endDate) - 1
  
  
  if (selectedMonth == 2) {
    
    linModel <- linModel[["FEB"]]
    
  } else if (selectedMonth == 3) {
    
    linModel <- linModel[["MAR"]]
    
  } else {
    
    # For May through September, use the "October - April" model
    linModel <- linModel[["APR"]]
    
    
    # Make sure 'selectedMonth' is set to 4 as well (corresponding to April)
    selectedMonth <- 4
    
  }
  
  
  # PRISM data that was previously downloaded for SRP is also required
  # Locate that file, confirm its existence, and validate the data
  prismPath <- paste0("W2_Russian_River/Intermediate/PRISM_SRP_Domain_Data_",
                      getModeledWY(endDate)[1], "_", endDate, ".csv") |>
    checkForPreviousOutput()
  
  
  currentPrecip <- prismPath |>
    getPRISM()
  
  
  # The validation function expects both precipitation and temperature,
  # so include dummy columns for "TMIN" and "TMAX" when checking the data
  currentPrecip |>
    mutate(`tmin (degrees C)` = 0, `tmax (degrees C)` = 0) |>
    validateWebData(dataSource = "PRISM", 
                    inputPath = prismPath,
                    stationVec = currentPrecip$Name |> unique(),
                    siPRISM = TRUE)
  
  
  # Convert the data in 'currentPrecip' into an average daily precipiation 
  # value for the entire model domain
  currentPrecip <- currentPrecip |>
    group_by(Date) |>
    summarize(`ppt (mm)` = mean(`ppt (mm)`), .groups = "drop")
  
  
  # After that, adjust 'pastPrecip'
  # It should only contain data for dates present in 'mergedDAT'
  # (The analysis will fail if a year with no data in 'mergedDAT' is selected)
  pastPrecip <- pastPrecip |>
    filter(Date >= min(mergedDAT$DATE) & Date <= max(mergedDAT$DATE))
  
  
  # Use 'pastPrecip' and 'currentPrecip' in conjunction with the linear model
  # Identify the most similar water year for the current water year
  similarWY <- similarWY_findWY(endDate, pastPrecip, currentPrecip, 
                                dirPath, selectedMonth, linModel)
  
  
  # Create a final DAT tibble after that using the similar water year
  finalDAT <- similarWY_appendDAT(mergedDAT, endDate, similarWY)
  
  
  # After that, update the metadata file
  updateMetadata_DAT(dirPath, datStartDate = min(finalDAT$DATE),
                     modelEndDate = getModeledWY(endDate)[2],
                     predictionMethod = "WY", pathMainDAT = pathMainDAT,
                     pathPastPrecip = pathPastPrecip, 
                     pathCurrentPrecip = prismPath,
                     similarWY = similarWY, linModel = linModel)
  
  
  # 'currentPrecip' should be archived in the hydrology folder as well
  # (This was already done in "RRW_v2_007_Setup_Output_Directory.R")
  
  
  # Return 'finalDAT'
  return(finalDAT)
  
}



similarWY_findWY <- function (endDate, pastPrecip, currentPrecip, 
                              dirPath, endMonth, linModel) {
  
  # In March 2026, SDA staff developed three calibrated and validated linear 
  # regression models that linked partial precipitation to total water year 
  # precipitation
  
  # These models cover "Oct - Feb", "Oct - Mar", and "Oct - Apr"
  
  # These models will now be applied here to find the most similar water year
  # for the current water year
  
  # Given the partial precipitation, the total precipitation for the water year
  # will be estimated
  
  # Then, this predicted total will be compared to the total precipitation
  # in previous water years
  
  # The "most similar water year" will have a total precipitation closest to 
  # the current water year's predicted total
  
  
  # First, make sure that 'endMonth' is a valid value
  # It should correspond to a month between February and April
  if (!(endMonth %in% 2:4)) {
    
    paste0("Script Error - Invalid 'endMonth'\n\n",
           "The \"Similar Water Year\" method uses data from October to ",
           "February/March/April to predict the total precipitation ",
           "for the current water year. As a result, the input ",
           "\"endMonth\" should have a value between 2 ",
           "and 4 (inclusive). However, \"", endMonth, "\" was provided ",
           "to `similarWY_findWY` instead. Please revise the script.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Get the current water year 
  currentWY <- getModeledWY(endDate)[2] |> year()
  
  
  # Adjust the formatting of 'pastPrecip'
  # Add water year and date columns
  # (Additional edits will occur later)
  pastPrecip <- pastPrecip |>
    mutate(YEAR = year(Date), MONTH = month(Date)) |>
    mutate(WY = if_else(MONTH < 10, YEAR, YEAR + 1)) |>
    filter(!is.na(`ppt (mm)`)) |>
    arrange(Date)
  
  
  # Calculate the partial precipitation for the current water year
  # It will be Oct - Feb/Mar/Apr
  partialPrecip <- currentPrecip |>
    mutate(YEAR = year(Date), MONTH = month(Date)) |>
    mutate(WY = if_else(MONTH < 10, YEAR, YEAR + 1)) |>
    filter(WY == currentWY) |>
    filter(MONTH > 9 | MONTH <= endMonth) |>
    select(`ppt (mm)`) |>
    sum()
  
  
  # Apply the linear regression model to get the predicted total this year
  predictedPrecip <- linModel$m * partialPrecip + linModel$b
  
  
  # Before finding the most similar water year in 'pastPrecip', some additional
  # adjustments are necessary first
  
  # Only keep water years in 'pastPrecip' that have a complete set of data
  # (Ignore the current water year too)
  
  # To determine which water years are missing data, two approaches will be used
  
  # First, a count of days for every water year will be established
  # Any year with less than 365 days of data is incomplete
  # These water years should be removed from 'pastPrecip'
  countDF <- pastPrecip |>
    group_by(WY) |>
    summarize(COUNT = n()) |>
    filter(COUNT < 365)
  
  
  # Water years with incomplete data will be removed from 'pastPrecip'
  # (At this step, the current water year will be excluded too)
  pastPrecip <- pastPrecip |>
    filter(!(WY %in% countDF$WY | WY == currentWY))
  
  
  # The second method to determine which water years are missing data will rely
  # on a separate tibble of expected dates
  # (This extra check is needed because some years should have 365 days 
  #  and others should have 366. It could be cleaner to just verify that water  
  #  years have 365 days by default and require 366 if YEAR %% 4 == 0, but 
  #  this method can also weed out instances where a date is missing and another 
  #  date is duplicated in the same water year--it is unlikely to happen though)
  dateDF <- tibble(DATE = seq(from = min(pastPrecip$Date), 
                              to = max(pastPrecip$Date), 
                              by = "days")) |>
    mutate(WY = if_else(month(DATE) < 10, year(DATE), year(DATE) + 1))
  
  
  # Figure out which dates are missing in 'outDF' from 'dateDF'
  missingDates <- which(!(dateDF$DATE %in% pastPrecip$Date))
  
  
  # If missing dates are found, remove their water years from 'pastPrecip'
  if (length(missingDates) > 0) {
    
    # Identify the water years that correspond to the missing dates
    incompleteWYs <- dateDF$WY[missingDates] |> unique()
    
    
    # Remove those water years from 'outDF'
    pastPrecip <- pastPrecip |>
      filter(!(WY %in% incompleteWYs))
    
  }
  
  
  # Now that all incomplete water years have been excluded, calculate
  # the partial precipitation and total water year precipitation for
  # every water year in 'outDF'
  precipDF <- pastPrecip |>
    group_by(WY) |>
    summarize(!! paste0("OCT_TO_", toupper(month.abb[endMonth]), 
                        "_PARTIAL_PRECIP") := 
                sum(`ppt (mm)`[MONTH > 9 | MONTH <= endMonth]),
              TOTAL_WY_PRECIP = sum(`ppt (mm)`), 
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
                  normalizePath(mustWork = FALSE))
  
  
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
    mutate(YEAR = if_else(MONTH < 10, 
                          year(currentWY[2]), year(currentWY[1]))) |>
    mutate(DATE = paste0(YEAR, "-", MONTH, "-", DAY) |> 
             as.Date(format = "%Y-%m-%d"))
  
  
  # One final check is for leap years
  
  # If February 29th does not exist in the current water year, 
  # ensure that it is not present in 'wyDAT'
  if (is.na(paste0(year(currentWY[2]), "-02-29") |>
            as.Date(format = "%Y-%m-%d"))) {
    
    # Remove any entries for February 29th
    wyDAT <- wyDAT |>
      filter(!(MONTH == 2 & DAY == 29))
    
    # However, if February 29th is present in the modeled water year,
    # double-check that an entry is present for that date
  } else {
    
    # If no row is present, duplicate the row for February 28th
    # and use it for February 29th as well
    if (nrow(wyDAT |> filter(MONTH == 2 & DAY == 29)) == 0) {
      
      # Bind the altered entry for February 28th to 'wyDAT'
      wyDAT <- wyDAT |>
        bind_rows(wyDAT[wyDAT$MONTH == 2 & wyDAT$DAY == 28, ] |>
                    mutate(DAY = 29))
      
    }
    
  }
  
  
  # The above procedure is kinda extraneous since the "Most Similar WY"
  # method is only used for data substitutions from March onwards 
  
  # However, let's keep it in case upstream modifications to the procedure
  # change that restriction
  
  
  # Filter 'wyDAT' to after 'endDate'
  wyDAT <- wyDAT |>
    filter(DATE > endDate)
  
  
  # Append 'wyDAT' to 'mergedDAT' and return it
  return(mergedDAT |>
           bind_rows(wyDAT))
  
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
  
  
  # Create a finalized version of 'mergedDAT':
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
                                  tolower() |> str_replace("second", "sec") |>
                                  str_replace("([a-z])([0-9])$", "\\10\\2") |>
                                  paste0(collapse = str_dup(" ", 10))))
  
  
  # Append 'headerDAT' to the beginning of 'finalDAT'
  finalDAT <- bind_rows(headerDAT,
                        finalDAT) |>
    select(FINAL)
  
  
  # Write 'finalDAT' to the hydrology folder first
  finalDAT$FINAL |>
    writeOutput(paste0(dirPath, "/SRP/Input/", datName) |> 
                  normalizePath(mustWork = FALSE),
                writeFunction = "write_lines", quietly = quietly)
  
  
  # Write 'finalDAT' to the SRP model folder next
  # The name will be fixed as "RR_SRP_Input.dat" for ease of modeling automation
  finalDAT$FINAL |>
    writeOutput(paste0(srpPath, "/external_files/", genericName) |> 
                  normalizePath(mustWork = FALSE),
                writeFunction = "write_lines", quietly = quietly)
  
  
  # Update the SRP control file next
  # (Its presence was already confirmed at the beginning of the script in 
  #  `validateModelCopy_SRP`)
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
  controlPath <- paste0(srpPath, 
                        "/model1/SRPHM_post_spinup_WY2021/SRPHM_spinup.control") |>
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
  srpControl[targetLoc + 3] <- paste0("..\\..\\external_files\\", datName)
  
  
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
  
  
  batDir <- paste0(srpPath, "/model1/SRPHM_post_spinup_WY2021/") |>
    normalizePath(mustWork = TRUE)
  
  
  batchCommands <- c(paste0("cd ", shQuote(batDir)),
                     "bin\\gsflow.exe SRPHM_spinup.control")
  
  # The first command changes the working directory to the location of 
  # the SRP bat file (the "SRPHM_post_spinup_WY2021" directory in "SRPHM")
  # The second command then executes gsflow.exe using "SRPHM_spinup.control" 
  # (the latter is also located in the same directory as the bat file)
  
  
  # Write these commands to "run_SRPHM_spinup.bat"
  batchCommands |>
    writeOutput(paste0(srpPath, 
                       "/model1/SRPHM_post_spinup_WY2021/run_SRPHM_spinup.bat") |> 
                  normalizePath(mustWork = FALSE),
                quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

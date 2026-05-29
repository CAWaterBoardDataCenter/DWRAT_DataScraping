# Verify that all required weather data has been downloaded
# Then, reformat the data into a structure suitable for the PRMS DAT file


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
#  (1) "ProcessedData/PRMS_Meteorological_[startDate]_[endDate].csv"


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function (allTempColumnsFromPRISM = TRUE) {
  
  cat("\n\n")
  cat("Starting 'RRW_005_Process_PRMS_Weather_Data.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Start with a vector containing every single required input file
  inputFiles <- tibble("PRISM_INPUT" = getFromControl_RR("PRISM_PRMS_STATIONS_CSV") |>
                         sharepointPathCheck(isFolder = FALSE),
                       
                       "NOAA_INPUT" = getFromControl_RR("NOAA_STATIONS_CSV") |>
                         sharepointPathCheck(isFolder = FALSE), 
                       
                       "RAWS_INPUT" = getFromControl_RR("RAWS_STATIONS_CSV") |>
                         sharepointPathCheck(isFolder = FALSE), 
                       
                       "CIMIS_INPUT" = getFromControl_RR("CIMIS_STATIONS_CSV") |>
                         sharepointPathCheck(isFolder = FALSE),
                       
                       "PRECIP_OUTLIER_BOUNDS" = getFromControl_RR("PRMS_PRECIP_GAGE_OUTLIER_BOUNDS") |>
                         sharepointPathCheck(isFolder = FALSE),
                       
                       "PRECIP_GAGE_CORRELATION" = getFromControl_RR("PRMS_PRECIP_GAGE_CORRELATION_TABLE") |>
                         sharepointPathCheck(isFolder = FALSE), 
                       
                       "PRISM_OUTPUT" = paste0("WebData/PRISM_PRMS_Data_",
                                               startDate, "_", endDate, ".csv"),
                       "NOAA_OUTPUT" = paste0("WebData/NOAA_API_Data_",
                                              startDate, "_", endDate, ".csv"),
                       "RAWS_OUTPUT" = paste0("WebData/RAWS_HTTP_Data_",
                                              startDate, "_", endDate, ".csv"),
                       "CIMIS_OUTPUT" = paste0("WebData/CIMIS_API_Data_",
                                               startDate, "_", endDate, ".csv"))
  
  
  # Check if any required input files are missing
  if (anyFalse(map_lgl(inputFiles, file.exists))) {
    
    # Output the names of the missing files before sending a message
    missingFiles <- inputFiles[!map_lgl(inputFiles, file.exists)]
    
    
    cat("\n\n")
    cat("Missing File(s):\n")
    print(missingFiles)
    cat("\n\n")
    
    
    # Output the error message too
    stop(paste0("Missing Required Input File", 
                if_else(length(missingFiles) > 1, "s", ""), "\n\n",
                "This script requires that the PRISM, NOAA, RAWS, and CIMIS ",
                "web scraping scripts are run for the chosen date range (",
                startDate, " to ", endDate, "). However, ", length(missingFiles),
                " file", if_else(length(missingFiles) > 1, "s are", " is"), 
                " missing. Please prepare any required input files and then run ",
                "the corresponding script", 
                if_else(length(missingFiles) > 1, "s", ""),
                " first.\n\n",
                "Also, please make sure to provide files with the outlier ",
                "bounds (in mm) and gage correlations for precipitation data.") |>
           errWrap())
    
  }
  
  
  # Read in the files next
  prismInput <- inputFiles$PRISM_INPUT[1] |> getFile() |> unique()
  noaaInput <- inputFiles$NOAA_INPUT[1] |> getFile() |> unique()
  rawsInput <- inputFiles$RAWS_INPUT[1] |> getFile() |> unique()
  cimisInput <- inputFiles$CIMIS_INPUT[1] |> getFile() |> unique()
  
  prismDF <- getPRISM(inputFiles$PRISM_OUTPUT[1])
  noaaDF <- getDelim(inputFiles$NOAA_OUTPUT[1], delim = ",")
  rawsDF <- getDelim(inputFiles$RAWS_OUTPUT[1], delim = ",")
  cimisDF <- getDelim(inputFiles$CIMIS_OUTPUT[1], delim = ",")
  
  outlierDF <- getFile(inputFiles$PRECIP_OUTLIER_BOUNDS[1])
  corrDF <- getFile(inputFiles$PRECIP_GAGE_CORRELATION[1])
  
  
  # Validate all variables next
  cat("[1/2]\tChecking all input files...\n")
  
  
  # Ensure that all eight primary files have the expected formatting
  validateInputs(prismInput, noaaInput, rawsInput, cimisInput,
                 prismDF, noaaDF, rawsDF, cimisDF, inputFiles)
  
  
  # Check 'outlierDF' next too
  validateOutlierFile(outlierDF, inputFiles$PRECIP_OUTLIER_BOUNDS[1])
  
  
  # Finally, check 'corrDF'
  validateCorrFile(corrDF, inputFiles$PRECIP_GAGE_CORRELATION[1])
  
  
  cat("\tDone!\n\n")
  
  
  # After all validation requirements have been cleared, prepare a single
  # meteorological dataset (combining data from NOAA, RAWS, and CIMIS)
  cat("[2/2]\tPreparing final meteorological dataset...\n")
  
  
  meteorDF <- combineMeteorologicalDatasets(noaaInput, rawsInput, cimisInput,
                                            noaaDF, rawsDF, cimisDF,
                                            startDate, endDate)
  
  
  # For archival purposes, save 'meteorDF' without any data substitution
  # or outlier modifications
  meteorDF |>
    writeOutput(paste0("ProcessedData/PRMS_No-QAQC_Meteorological_", 
                       startDate, "_", endDate, ".csv"),
                quietly = TRUE)
  
  
  # After that, check for and remove outliers from the dataset
  # Then, fill in empty entries using other gages' data or PRISM values
  meteorDF <- datQAQC(meteorDF, outlierDF, corrDF, 
                      prismDF, prismInput, allTempColumnsFromPRISM,
                      fullQAQC = TRUE)
  
  
  # Missing entries in this dataset will be substituted with PRISM data
  # (And if 'allTempColumnsFromPRISM' is set to TRUE, all temperature data will 
  #  come from PRISM)
  #meteorDF <- prismSub(meteorDF, prismDF, prismInput, allTempColumnsFromPRISM)
  
  
  cat("\tDone!\n\n")
  
  
  # Once this step is complete, write 'meteorDF' to a file
  outFile <- paste0("ProcessedData/PRMS_Meteorological_", startDate, "_",
                    endDate, ".csv")
  
  
  meteorDF |>
    writeOutput(outFile)
  
  
  # Output a completion message
  cat(col_green("\n'RRW_005_Process_PRMS_Weather_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateInputs <- function (prismInput, noaaInput, rawsInput, cimisInput,
                            prismDF, noaaDF, rawsDF, cimisDF, inputFiles) {
  
  # Verify that all eight tibbles are formatted as expected
  
  
  # The number of expected PRMS precipitation columns is hard-coded as 15
  # Similarly, the number of expected minimum/maximum temperature columns is 8
  numPrecip <- 15
  numTemp <- 8
  
  
  # First, check the four "INPUT" tibbles
  validateStationInputs(prismInput, inputFiles$PRISM_INPUT[1], "PRMS", numPrecip, numTemp)
  validateStationInputs(noaaInput, inputFiles$NOAA_INPUT[1], "PRMS", numPrecip, numTemp)
  validateStationInputs(rawsInput, inputFiles$RAWS_INPUT[1], "PRMS", numPrecip, numTemp)
  validateStationInputs(cimisInput, inputFiles$CIMIS_INPUT[1], "PRMS", numPrecip, numTemp)
  
  
  # Validate the four weather output tibbles next
  
  # Each website returns data in a slightly different format
  # But the general expectations are similar in all cases
  validateWebData(prismDF, "PRISM", inputFiles$PRISM_OUTPUT[1], prismInput$STATION_ID, siPRISM = TRUE)
  validateWebData(noaaDF, "NOAA", inputFiles$NOAA_OUTPUT[1], noaaInput$STATION_ID)
  validateWebData(rawsDF, "RAWS", inputFiles$RAWS_OUTPUT[1], rawsInput$STATION_ID)
  validateWebData(cimisDF, "CIMIS", inputFiles$CIMIS_OUTPUT[1], cimisInput$STATION_ID)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateOutlierFile <- function (outlierDF, sourcePath, numPrecip = 15) {
  
  # Inspect 'outlierDF' and ensure that all PRMS precipitation gages have
  # outlier bounds for every month
  
  
  # Every month should have an outlier limit column
  expectedCols <- c("GAGE",
                    paste0(month.abb, "_OUTLIER_LIMIT_MM") |> toupper())
  
  
  # Check for missing columns
  outlierDF |>
    checkMissingCol(expectedCols, sourcePath, 
                    infoStr = paste0("file containg upper outlier bounds ",
                                     "(in mm) for PRMS precipitation gages"))
  
  
  # After that, confirm that one row is present in 'outlierDF' 
  # for every PRMS precipitation column
  if (nrow(outlierDF) != numPrecip ||
      anyFalse(paste0("PRECIP", 1:numPrecip) %in% outlierDF[["GAGE"]])) {
    
    paste0("Incompatible Number of Rows\n\n",
           "The file containing outlier bounds for each PRMS precipitation ",
           "gage is expected to have exactly one row for each of the ", 
           numPrecip, " precipitation stations. The \"GAGE\" column should only ",
           "have \"PRECIP1\" through \"PRECIP", numPrecip, "\" as its values. ",
           "However, this was not the case. Please investigate the file for ",
           "issues.\n\n",
           "(This error occurred for \"", sourcePath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, confirm that every "OUTLIER_LIMIT" column is numeric
  # These values should be either NA or a positive number
  if (outlierDF[toupper(paste0(month.abb, "_OUTLIER_LIMIT_MM"))] |>
      map_lgl(is.numeric) |> anyFalse() ||
      any(!is.na(outlierDF[toupper(paste0(month.abb, "_OUTLIER_LIMIT_MM"))]) & 
          outlierDF[toupper(paste0(month.abb, "_OUTLIER_LIMIT_MM"))] < 0)) {
    
    paste0("\"OUTLIER_LIMIT\" Column Issue\n\n",
           "The file containing outlier bounds for each PRMS precipitation ",
           "gage is expected to have an \"OUTLIER_LIMIT\" column for each of ",
           "the twelve months. The values for each gage can be either \"NA\" or ",
           "a positive number. However, this was not the case. Please ",
           "investigate the file for issues.\n\n",
           "(This error occurred for \"", sourcePath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



validateCorrFile <- function (corrDF, sourcePath, numPrecip = 15) {
  
  # Check the values in 'corrDF' 
  # Ensure that all PRMS precipitation gages have model values 
  # between themselves and with their respective PRISM datasets too
  
  
  # Check for five key columns
  expectedCols <- c("PREDICTOR", "RESPONSE", "SLOPE", "INTERCEPT", "R_SQUARED")
  
  
  # Check for missing columns
  corrDF |>
    checkMissingCol(expectedCols, sourcePath, 
                    infoStr = paste0("file containg linear regression model ",
                                     "parameters for PRMS precipitation gages"))
  
  
  # After that, confirm that no missing values are present in 'corrDF'
  if (anyNA(corrDF)) {
    
    # Print to the console the location of these NA values
    cat("\n\n")
    cat("Missing Element(s):\n")
    print(which(is.na(corrDF), arr.ind = TRUE))
    cat("\n\n")
    
    
    # Then output an error message
    paste0("Missing Values Detected\n\n",
           "The file containing linear regression models for PRMS precipitation ",
           "gages should not have any \"NA\" values in any of its columns. ",
           "However, at least one missing value was detected (see above). ",
           "Please investigate the file for issues.\n\n",
           "(This error occurred for \"", sourcePath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Iterate through every precipitation column ("PRECIP1" to 'numPrecip')
  # Make sure it appears in a model with all other precipitation columns
  # (There should be one with "PRISM" too)
  precipCols <- paste0("PRECIP", 1:numPrecip)
  
  
  for (i in 1:length(precipCols)) {
    
    # Take a subset of 'corrDF'
    # Get all models that involve this iteration's value from 'precipCols'
    subsetDF <- corrDF |>
      filter(PREDICTOR == precipCols[i] | RESPONSE == precipCols[i])
    
    
    # Extract all values in the "PREDICTOR" and "RESPONSE" columns
    colNames <- c(subsetDF$PREDICTOR, subsetDF$RESPONSE)
    
    
    # Confirm that every value in 'precipCols' appears within 'colNames'
    # (That means that every PRMS precipitation column was modeled against
    #  this iteration's specific precipitation column)
    
    # In addition, "PRISM" should also appear in 'colNames'
    # (This corresponds to the precipitation gage being modeled against 
    #  its PRISM counterpart)
    
    if (anyFalse(precipCols %in% colNames) || !("PRISM" %in% colNames)) {
      
      cat("\n\n")
      cat(paste0("Missing Model(s) for ", precipCols[i], ":\n"))
      print(!(c(precipCols, "PRISM") %in% colNames))
      cat("\n\n")
      
      
      paste0("Missing Models for PRECIP", i, " (And Maybe More)\n\n",
             "The file containing linear regression models for PRMS precipitation ",
             "gages should have models between every gage. Each of the ", 
             numPrecip, " gages should have a model between it and the other ", 
             numPrecip - 1, " gages. In addition, there should be a model with ",
             "the gage's PRISM counterpart. However, at least one gage does not ",
             "have a complete set of models (one gage is shown above--there may ",
             "be more). Please investigate the file for issues.\n\n",
             "(This error occurred for \"", sourcePath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
  }
  
  
  # Next, confirm that "SLOPE", "INTERCEPT", and "R_SQUARED" are all numeric values
  if (corrDF |> select(SLOPE, INTERCEPT, R_SQUARED) |> 
      map_lgl(is.numeric) |> anyFalse()) {
    
    cat("\n\n")
    cat("Non-Numeric Column(s):\n")
    print(corrDF |> select(SLOPE, INTERCEPT, R_SQUARED) |> 
            map_lgl(~ !is.numeric(.)) |> which(useNames = TRUE) |> names())
    cat("\n\n")
    
    
    paste0("Incorrect Column Type\n\n",
           "The file containing linear regression models for PRMS precipitation ",
           "gages should have numeric columns corresponding to the model ",
           "slope and intercept, as well as its R^2 value. However, one or more ",
           "of these columns could not be parsed as numeric. Please investigate ",
           "the file for issues.\n\n",
           "(This error occurred for \"", sourcePath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Make sure "R_SQUARED" contains values between 0 and 1 (inclusive)
  if (any(corrDF$R_SQUARED > 1 | corrDF$R_SQUARED < 0)) {
    
    paste0("R^2 Column Error\n\n",
           "The file containing linear regression models for PRMS precipitation ",
           "gages should have a column that shows each model's R^2 metric. ",
           "This column is expected to have values that range between 0 and 1 ",
           "(inclusive). However, that was not the case. Please investigate ",
           "the file for issues.\n\n",
           "(This error occurred for \"", sourcePath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



combineMeteorologicalDatasets <- function (noaaInput, rawsInput, cimisInput,
                                           noaaDF, rawsDF, cimisDF,
                                           startDate, endDate) {
  
  # Format the data for easier integration into the PRMS DAT file
  # For each station, the relevant PRMS column names are listed in the input files
  
  
  # Start with building a skeleton for the final dataset
  meteorDF <- tibble(DATE = seq(from = startDate, to = endDate, by = "days"))
  
  
  # Add columns for precipitation, minimum temperature, and maximum temperature
  
  
  # To help specify these column names (and get their ordering right),
  # make a data frame for the column names
  prmsColumnNames <- c(noaaInput$PRMS_PRECIP_NAME, rawsInput$PRMS_PRECIP_NAME, 
                       cimisInput$PRMS_PRECIP_NAME,
                       noaaInput$PRMS_TMIN_NAME, rawsInput$PRMS_TMIN_NAME, 
                       cimisInput$PRMS_TMIN_NAME,
                       noaaInput$PRMS_TMAX_NAME, rawsInput$PRMS_TMAX_NAME, 
                       cimisInput$PRMS_TMAX_NAME) |>
    unique() |> sort() |>
    matrix(ncol = 1) |> data.frame() |> set_names("COLUMN") |>
    filter(!is.na(COLUMN)) |>
    mutate(TYPE = str_remove(COLUMN, "[0-9]+$"),
           NUMBER = str_extract(COLUMN, "[0-9]+$") |> as.numeric()) |>
    arrange(TYPE, NUMBER)
  
  # The above code pools together all PRMS-related field names into a vector, 
  # then a matrix, and finally a data frame
  # The column is arbitrarily titled "COLUMN" 
  # Then, two variables are created based on the type of PRMS variable and the 
  # value of the column name's number
  # Finally, the data frame is sorted based on the column type 
  # (PRECIP > TMAX > TMIN) and the column number
  
  # Note: With the default value of "NA" for the argument "na.last" in sort(), 
  # the NA entries are removed automatically
  # But just for redundancy, a filter to remove "NA" is also applied to the data frame
  
  
  # Add these columns to 'meteorDF'
  meteorDF[prmsColumnNames$COLUMN] <- NA_real_
  
  
  
  # In another function, reformat 'noaaDF', 'rawsDF', and 'cimisDF'
  noaaProcessed <- noaaDF |>
    reformatClimateData(noaaInput, "NOAA")
  
  rawsProcessed <- rawsDF |>
    reformatClimateData(rawsInput, "RAWS")
  
  cimisProcessed <- cimisDF |>
    reformatClimateData(cimisInput, "CIMIS")
  
  
  # Bind these processed data frames to 'meteorDF'
  # (Other than "DATE", the columns in the processed tibbles should replace 
  #  the corresponding ones in 'meteorDF')
  meteorDF <- meteorDF |>
    # Aside from "DATE", remove all PRMS fields from 'meteorDF' that 
    # appear in the processed tibble
    select(-all_of(names(noaaProcessed)[names(noaaProcessed) != "DATE"])) |>
    # Then, join the processed tibble to 'meteorDF'
    left_join(noaaProcessed, by = "DATE", relationship = "one-to-one") |>
    # Repeat with RAWS
    select(-all_of(names(rawsProcessed)[names(rawsProcessed) != "DATE"])) |>
    left_join(rawsProcessed, by = "DATE", relationship = "one-to-one") |>
    # Repeat with CIMIS
    select(-all_of(names(cimisProcessed)[names(cimisProcessed) != "DATE"])) |>
    left_join(cimisProcessed, by = "DATE", relationship = "one-to-one")
  
  
  # Return the revised 'meteorDF'
  return(meteorDF |>
           select(DATE, all_of(prmsColumnNames$COLUMN)))
  
}



reformatClimateData <- function (climateDF, climateInput, dataSource) {
  
  # The 'climateDF' data frames need to be widened 
  # (so that each station's data is in its own separate column)
  
  # The "PRMS" column names in 'climateInput' will then be used to switch 
  # from the station IDs to the PRMS field names
  fieldNameVec <- validateWebData_expectedColumnNames(dataSource, siPRISM = TRUE)
  
  
  # Start by renaming the columns in 'climateDF' to be consistent 
  # Then, pivot the dataset into a wider format (where each station has 
  # three of its own columns--one for each PRMS field)
  widerDF <- climateDF |>
    select(all_of(fieldNameVec)) |>
    pivot_wider(names_from = STATION_ID,
                values_from = c(PRECIP, TMIN, TMAX),
                names_sep = "_")
  
  
  # After that, prepare the PRMS-equivalent names using 'climateInput'
  # Appending the station IDs to "PRECIP"/"TMAX"/"TMIN" gives the 
  # column names that appear in 'widerDF'
  # The values in "PRMS_PRECIP_NAME", "PRMS_TMAX_NAME", and "PRMS_TMIN_NAME" 
  # are the intended replacements for these column names
  equivalentNames <- climateInput |>
    mutate(NAME_1 = paste0("PRECIP_", STATION_ID),
           NAME_2 = paste0("TMAX_", STATION_ID),
           NAME_3 = paste0("TMIN_", STATION_ID)) |>
    select(NAME_1, NAME_2, NAME_3, 
           PRMS_PRECIP_NAME, PRMS_TMAX_NAME, PRMS_TMIN_NAME)
  
  
  # Create a vector from 'equivalentNames' that can be used with rename()
  renameVec <- c(equivalentNames$NAME_1, equivalentNames$NAME_2,
                 equivalentNames$NAME_3) |>
    set_names(c(equivalentNames$PRMS_PRECIP_NAME, equivalentNames$PRMS_TMAX_NAME, 
                equivalentNames$PRMS_TMIN_NAME))
  
  
  # Not every station will be used for precipitation and max/min temperature
  # In those cases, the names will be "NA"
  # Remove them from 'renameVec'
  renameVec <- renameVec[!is.na(names(renameVec)) & renameVec != "NA"]
  
  
  # After that, apply 'renameVec' to 'widerDF'
  # Then, keep DATE and the renamed variables only
  processedDF <- widerDF |>
    rename(any_of(renameVec)) |>
    select(DATE, any_of(names(renameVec)))
  
  
  # Return 'processedDF'
  return(processedDF)
  
}



datQAQC <- function (meteorDF, outlierDF, corrDF, 
                     prismDF, prismInput, allTempSub,
                     fullQAQC = TRUE) {
  
  
  # If 'fullQAQC' is TRUE, the outlier procedure and correlation-based 
  # replacement algorithm will be applied
  if (fullQAQC) {
    
    # Identify outliers in the dataset and make them 'NA'
    meteorDF <- removeOutliers(meteorDF, outlierDF)
    
    
    # Then, try to use gage data and/or modified PRISM data to replace missing values
    meteorDF <- gageSub(meteorDF, corrDF, prismDF, prismInput)
    
  }
  
  # 'fullQAQC' should be set to FALSE when the older SDA workflow for 
  # PRMS and SRP is desired
  
  # (In that case, only PRISM-based substitution occurs)
  
  
  # Regardless of the value of 'fullQAQC', 
  # replace any remaining missing values with direct PRISM values
  # (Also, if 'allTempSub' is TRUE, all temperature values will come from PRISM)
  meteorDF <- prismSub(meteorDF, prismDF, prismInput, allTempSub)
  
  
  # Return 'meteorDF'
  return(meteorDF)
  
}



removeOutliers <- function (meteorDF, outlierDF) {
  
  # Given upper-limit bounds for each PRMS precipitation gage, 
  # remove outliers from their datasets
  
  
  # Get a vector of precipitation columns that appear in 'meteorDF'
  precipNames <- names(meteorDF) |>
    str_subset("^PRECIP[0-9]+$")
  
  
  # Iterate through each of the precipitation gages in 'meteorDF'
  for (i in 1:length(precipNames)) {
    
    # Note: This procedure will not be applied to several gages
    if (precipNames[i] %in% c("PRECIP1", "PRECIP4", "PRECIP7",
                              "PRECIP6", "PRECIP12")) {
      
      # Gages 1, 4, and 7 are outside the watershed and correlate poorly with
      # all other gages
      
      # Gages 6 and 12 come from CIMIS, but they are raw and unsuitable for 
      # this process
      
      next
    }
    
    
    # Extract a subset of 'meteorDF' that contains "DATE" and the corresponding
    # precipitation column
    # (Add a "MONTH" column too)
    subsetDF <- meteorDF |>
      select(DATE, all_of(precipNames[i])) |>
      mutate(MONTH = month(DATE))
    
    
    # To make the process simpler, rename the gage in 'subsetDF' to "PRECIP"
    subsetDF <- subsetDF |>
      rename(PRECIP = all_of(outlierDF$GAGE[i]))
    
    
    # Then, set all negative precipitation values in 'subsetDF' to NA
    # (This covers the lower bound for outliers)
    # (Also, all -999 values become NA) <-- If you do a before and after 
    #                                       comparison for this function, 
    #                                       don't forget about this!
    subsetDF$PRECIP[subsetDF$PRECIP < 0] <- NA_real_
    
    
    # Next, iterate in a nested loop through every month
    for (j in 1:12) {
      
      # Extract the corresponding upper bound for this gage and month
      outLimit <- outlierDF[outlierDF$GAGE == precipNames[i], ] |>
        select(contains("OUTLIER_LIMIT")) |>
        select(starts_with(month.abb[j], ignore.case = TRUE)) |>
        unlist(use.names = FALSE)
      
      
      # If 'outLimit' is NA, skip to the next iteration
      if (is.na(outLimit)) {
        next
      }
      
      
      # Otherwise, find all precipitation values in 'subsetDF' that:
      #   (*) Match this month
      #   (*) Are not NA
      #   (*) Contain a value greater than 'outLimit'
      
      # Make those values NA
      subsetDF[subsetDF$MONTH == j &
                 !is.na(subsetDF$PRECIP) &
                 subsetDF$PRECIP > outLimit, ]$PRECIP <- NA_real_
      
    } # End of 'j' loop through months
    
    
    # At the end of the iteration, update the precipitation values in 'meteorDF'
    # with the modified values that appear in 'subsetDF'
    
    # Just in case, we want to make sure the values in 'subsetDF' are assigned
    # to the correct date
    
    # We also want to maintain the same ordering in 'meteorDF'
    
    # This will all be done in one stream of code to make it a little easier
    meteorDF <- meteorDF |>
      select(-all_of(precipNames[i])) |>
      full_join(subsetDF |> select(DATE, PRECIP) |>
                  rename(!! precipNames[i] := PRECIP),
                by = "DATE", relationship = "one-to-one") |>
      select(all_of(names(meteorDF)))
    
    #   (*) We remove the iteration's precipitation column from 'meteorDF'
    #   (*) Then, we extract only "DATE" and "PRECIP" from 'subsetDF' and change
    #       "PRECIP" back to its original name
    #   (*) After that, we join 'meteorDF' and 'subsetDF' together using "DATE"
    #   (*) Finally, we can preserve the original column ordering in 'meteorDF' 
    #       using its name vector and `select`
    #
    # Note: This only works because 'meteorDF' still has all of its columns
    #       at the time that the final `select` is occurring
    #
    #       IF YOU SPLIT THIS CODE INTO SEPARATE STEPS, IT WILL NOT WORK
    
  }
  
  
  # Return 'meteorDF' afterwards
  return(meteorDF)
  
}



gageSub <- function (meteorDF, corrDF, prismDF, prismInput) {
  
  # Fill in missing data gaps in 'meteorDF'
  
  # Rely on regression models that link gages to other gages in the watershed
  
  # Models that fill in missing data based on PRISM may also be used
  
  
  # The exact procedure for each precipitation gage is this:
  
  #  (1) Check if the gage has a strong model (R^2 > 0.90) with its PRISM data
  #
  #      If yes, apply the corresponding linear model to fill in gaps
  #
  #  (2) Sort each gage in terms of best correlation to worst
  #
  #      Take the three* best available gages and average their values 
  #      (The average precipitation values fill in the missing gaps)
  #
  #      If fewer than three gages have data available for that period, 
  #      still use the average if at least two gages have a value
  #      Otherwise, leave the entry as "missing"
  
  
  # First, create a copy of 'meteorDF'
  # Its missing entries will be adjusted and filled in
  adjDF <- meteorDF
  
  # The original data in 'meteorDF' will be preserved in this procedure
  # because gages' data can be used in other gages' model equations
  
  
  # Reformat PRISM data so that it can be used in the substitution process too
  prismProcessed <- reformatClimateData(prismDF, prismInput, "PRISM") |>
    select(all_of(names(meteorDF)))
  
  
  # Get a list of precipitation columns in 'adjDF'
  precipNames <- names(adjDF) |>
    str_subset("^PRECIP[0-9]+$")
  
  
  # Note: This procedure will not be applied to several gages
  excludedGages <- c("PRECIP1", "PRECIP4", "PRECIP7",
                     "PRECIP6", "PRECIP12")
  
  
  # Next, iterate through each of the precipitation columns in 'adjDF'
  for (i in 1:length(precipNames)) {
    
    # Skip the iterations for excluded gages
    if (precipNames[i] %in% excludedGages) {
      
      # Gages 1, 4, and 7 are outside the watershed and correlate poorly with
      # other precipitation gages
      
      # Gages 6 and 12 come from CIMIS, but they are raw and unsuitable for 
      # this process
      
      next
    }
    
    
    # Otherwise, check for missing values in this column of 'adjDF' 
    missingDates <- adjDF |>
      filter(is.na(get(precipNames[i])) | get(precipNames[i]) < 0) |>
      select(DATE)
    
    
    # If 'missingDates' is empty, skip to the next precipitation gage
    if (nrow(missingDates) == 0) {
      next
    }
    
    
    # If there is missing data, extract a subset of 'corrDF' with models 
    # related to the current iteration's gage
    # (Still ignore excluded gages)
    gageModels <- corrDF |>
      filter(PREDICTOR == precipNames[i] | RESPONSE == precipNames[i]) |>
      filter(!(PREDICTOR %in% excludedGages) & !(RESPONSE %in% excludedGages))
    
    
    # REMEDIATION OPTION #1: PRISM REGRESSION MODEL
    
    
    # First, check if this gage has a strong correlation with PRISM data
    # If so, that will be the source of data (but through a regression model)
    if (gageModels |> filter(PREDICTOR == "PRISM" | RESPONSE == "PRISM") |>
        filter(R_SQUARED > 0.90) |> nrow() > 0) {
      
      # Extract the model that correlates gage data and PRISM data
      prismModel <- gageModels |> 
        filter(PREDICTOR == "PRISM" | RESPONSE == "PRISM")
      
      
      # Get a modified version of the simulated gage data in 'prismProcessed'
      prismSubset <- prismProcessed |>
        select(DATE, all_of(precipNames[i]))
      
      
      # Apply the regression model to the PRISM data to convert it
      # into suitable gage data
      if (prismModel$PREDICTOR[1] == "PRISM") {
        
        # If PRISM data is the predictor (x), apply the model as:
        # gage = m * prism + b
        
        prismSubset[[precipNames[i]]] <- 
          prismSubset[[precipNames[i]]] * prismModel$SLOPE[1] + prismModel$INTERCEPT[1]
        
      } else {
        
        # Otherwise, if PRISM is the response variable (y), apply the model as:
        # gage = (prism - b) / m
        prismSubset[[precipNames[i]]] <- 
          (prismSubset[[precipNames[i]]] - prismModel$INTERCEPT[1]) / prismModel$SLOPE[1]
        
      }
      
      
      # Locate missing dates for this gage in 'adjDF'
      missingDates <- adjDF |>
        filter(is.na(get(precipNames[i])) | get(precipNames[i]) < 0) |>
        select(DATE)
      
      
      # Filter 'prismSubset' to the same dates
      prismSubset <- prismSubset |>
        filter(DATE %in% missingDates$DATE)
      
      
      # The safer (but slower) approach would be to fill in these missing values
      # in a for loop
      if (nrow(prismSubset) > 0) {
        
        # Iterate through every missing date in 'adjDF' 
        # that appears within 'prismSubset'
        for (j in 1:nrow(prismSubset)) {
          
          # Locate the index in 'adjDF' that contains this date
          matchIndex <- which(adjDF$DATE == prismSubset$DATE[j])
          
          
          # Replace that date's missing entry with gage-equivalent data
          adjDF[matchIndex, precipNames[i]] <- prismSubset[j, precipNames[i]]
          
        }
        
      }
      
      
      # Skip to the next gage once this procedure is complete
      next
      
    }
    
    
    # REMEDIATION OPTION #2: AVERAGE PRECIPITATION OF OTHER GAGES
    
    
    # An alternative option for filling in missing gage data is using the average
    # precipitation values from the two or three best correlated gages
    
    
    # Get the most similar non-PRISM gages for this iteration's gage
    # Sort the table from greatest to least R^2 and keep only the gage names
    similarGages <- gageModels |>
      arrange(desc(R_SQUARED)) |>
      select(PREDICTOR, RESPONSE) 
    
    # Note: In one column of each row, 'precipNames[i]' will appear
    #       The other column will have the name of a different gage
    
    
    # Flatten the two-column table of gage names into a single vector
    # Remove the iteration's gage name and "PRISM" from this result
    similarGages <- similarGages |>
      t() |> as.vector()|> unique() |>
      base::setdiff(precipNames[i]) |>
      base::setdiff("PRISM")
    
    # NOTE
    # Why do we transpose before unlisting the tibble? 
    
    # We do not know if 'precipNames[i]' appears in "PREDICTOR" or "RESPONSE"
    # in each row of 'similarGages'
    
    # However, we want both gages in a row to appear in the vector BEFORE
    # the two gages that appear in the next row 
    # (the next row is a different model with a worse R^2 value)
    
    # But `unlist` extracts values by COLUMN first instead of by ROW first
    
    # For example, in this table:
    
    # PREDICTOR     RESPONSE        R^2 (this column is not in 'similarGages' 
    # B             C               0.9  but the sorting is still in effect)
    # A             B               0.7
    # B             D               0.5
    
    # We want the final result to look like: "C", "A", "D"
    
    # However, if we apply `unlist` to the table, it unlists *by column* first
    
    # We get an intermediate result of "B", "A", "B", "C", "B", "D"
    
    # And the final result looks like: "A", "C", "D"
    
    
    # If we apply `t` first and switch the rows/columns, `unlist` will give
    # the desired result
    
    
    # Once we have a list of similar gages, iterate through the missing dates
    for (j in 1:nrow(missingDates)) {
      
      # Extract the values for these gages in 'meteorDF'
      similarVals <- meteorDF |>
        filter(DATE == missingDates$DATE[j]) |>
        select(all_of(similarGages)) |>
        unlist(use.names = FALSE)
      
      
      # Remove NA and negative values from 'similarVals'
      similarVals <- similarVals[!is.na(similarVals) & similarVals >= 0]
      
      
      # If 'similarVals' contains one or fewer entries, leave this gage's value 
      # as empty 
      # (The fallback PRISM substitution method will be applied later)
      if (length(similarVals) < 2) {
        next
      }
      
      
      # Otherwise, if 'similarVals' contains at least 3 values, 
      # take the average of the three most similar gages' values
      
      # If there are only 2 available values, use their average instead
      if (length(similarVals) > 2) {
        
        avgVal <- mean(similarVals[1:3])
        
      } else {
        
        avgVal <- mean(similarVals[1:2])
        
      }
      
      
      # Store 'avgVal' in 'adjDF' for this gage's missing entry
      adjDF[adjDF$DATE == missingDates$DATE[j], precipNames[i]] <- avgVal
      
    } # End of 'j' loop through missing dates for a gage in 'adjDF'
    
  } # End of 'i' loop through precipitation gages
  
  
  # Return 'adjDF' after this procedure is completed
  return(adjDF)
  
}



prismSub <- function (meteorDF, prismDF, prismInput, allTempSub) {
  
  # Wherever missing values are present in 'meteorDF', use PRISM data as a substitute
  
  # And if 'allTempSub' is TRUE, all temperature data will come from PRISM
  
  
  # Start by reformatting 'prismDF'
  prismProcessed <- reformatClimateData(prismDF, prismInput, "PRISM")
  
  
  # Make sure the columns in 'prismProcessed' match the ordering in 'meteorDF'
  prismProcessed <- prismProcessed |>
    select(all_of(names(meteorDF)))
  
  
  # Verify that 'meteorDF' and 'prismProcessed' have the same shape as well
  if (nrow(meteorDF) != nrow(prismProcessed)) {
    
    stop(paste0("Issue in `prismSub()`\n\n", 
                "There is a mismatch between the PRISM data and the other meteorlogical ",
                "data sources. Despite having the same date range, the number of days with ", 
                "available data does not match.\n\n",
                "This might be a script issue, but it more likely could be an issue ",
                "with the data itself. The files may be corrupted. ",
                "Please investigate.") |>
           errWrap())
    
  } else if (ncol(meteorDF) != ncol(meteorDF)) {
    
    stop(paste0("Issue in `prismSub()`\n\n", 
                "There is a mismatch between the PRISM data and the other ",
                "meteorlogical data sources. There should be PRISM data that ",
                "corresponds to each meteorological station. ", 
                "PRISM data should be available for every temperature and ",
                "precipitation column used in the PRMS model.\n\n", 
                "This is likely an issue with the input data. For example, the ",
                "downloaded files may be corrupt. Please investigate.") |>
           errWrap())
    
  }
  
  
  # Next, substitute PRISM data in 'meteorDF'
  
  
  # To make the process easier, replace instances of -999 in 'meteorDF' with "NA"
  meteorDF[meteorDF == -999] <- NA_real_
  
  
  # Then, iterate through every column
  for (j in 1:ncol(meteorDF)) {
    
    # Skip the column if it lacks "NA" values
    if (!anyNA(meteorDF[, j])) {
      next
    }
    
    
    # Wherever "NA" appears in 'meteorDF', use corresponding PRISM data
    missingRows <- is.na(meteorDF[, j])
    
    meteorDF[missingRows, j] <- prismProcessed[missingRows, j]
    
  }
  
  
  # After that, if 'allTempSub' is TRUE, replace all TMIN and TMAX columns with PRISM data
  if (allTempSub) {
    
    # Notify the user of this option being applied
    message("Replacing all temperature data with values from PRISM!")
    
    
    # Get a subset of 'prismProcessed' that just contains temperature columns (and "DATE")
    tempSub <- prismProcessed |>
      select(DATE, contains("TMAX"), contains("TMIN"))
    
    
    # Remove all temperature columns from 'meteorDF'
    # Then, append the PRISM temperature columns
    meteorDF <- meteorDF |>
      select(-all_of(names(tempSub)[names(tempSub) != "DATE"])) |>  # Don't remove "DATE" by accident!
      left_join(tempSub, by = "DATE", relationship = "one-to-one")
    
  }
  
  
  # As a final check, make sure there are no "NA" values left in 'meteorDF'
  if (anyNA(meteorDF)) {
    
    stop(paste0("Issue in `prismSub()`\n\n", 
                "By the end of this function, there should be no missing values ",
                "left in 'meteorDF'. However, \"NA\" ", 
                if_else(sum(is.na(meteorDF)) > 1, "values were ", "was "),
                "detected by the script.\n\n",
                "This could be a script issue, or a problem with the data. ",
                "Please investigate.") |>
           errWrap())
    
  }
  
  
  # Return 'meteorDF'
  return(meteorDF)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

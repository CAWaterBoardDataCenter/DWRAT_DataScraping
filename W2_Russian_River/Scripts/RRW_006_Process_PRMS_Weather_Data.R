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


#### Functions ####

mainProcedure <- function (allTempColumnsFromPRISM = TRUE) {
  
  cat("\n\n")
  cat("Starting 'RRW_006_Process_PRMS_Weather_Data.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Start with a vector containing every single required input file
  inputFiles <- tibble("PRISM_INPUT" = getFromControl_RR("PRISM_PRMS_STATIONS_CSV") |>
                         sharepointPathCheck(isFolder = FALSE),
                       
                       "NOAA_INPUT" = getFromControl_RR("NOAA_STATIONS_CSV") |>
                         sharepointPathCheck(isFolder = FALSE), 
                       
                       "RAWS_INPUT" = getFromControl_RR("RAWS_STATIONS_CSV") |>
                         sharepointPathCheck(isFolder = FALSE), 
                       
                       "CIMIS_INPUT" = getFromControl_RR("CIMIS_STATIONS_CSV") |>
                         sharepointPathCheck(isFolder = FALSE),
                       
                       "CDEC_INPUT" = getFromControl_RR("CDEC_PRECIPITATION_STATIONS_CSV") |>
                         sharepointPathCheck(isFolder = FALSE),
                       
                       "PRECIP_OUTLIER_BOUNDS" = getFromControl_RR("PRMS_PRECIP_GAGE_OUTLIER_BOUNDS") |>
                         sharepointPathCheck(isFolder = FALSE),
                       
                       "PRECIP_GAGE_CORRELATION" = getFromControl_RR("PRMS_PRECIP_GAGE_CORRELATION_TABLE") |>
                         sharepointPathCheck(isFolder = FALSE), 
                       
                       "PRISM_OUTPUT" = paste0("W2_Russian_River/Intermediate/PRISM_PRMS_Data_",
                                               startDate, "_", endDate, ".csv"),
                       
                       "NOAA_OUTPUT" = paste0("W2_Russian_River/Intermediate/NOAA_API_Data_",
                                              startDate, "_", endDate, ".csv"),
                       
                       "RAWS_OUTPUT" = paste0("W2_Russian_River/Intermediate/RAWS_HTTP_Data_",
                                              startDate, "_", endDate, ".csv"),
                       
                       "CIMIS_OUTPUT" = paste0("W2_Russian_River/Intermediate/CIMIS_API_Data_",
                                               startDate, "_", endDate, ".csv"),
                       
                       "CDEC_OUTPUT" = paste0("W2_Russian_River/Intermediate/CDEC_API_",
                                              "Precip_Data_",
                                               startDate, "_", endDate, ".csv"))
  
  
  # Check if any required input files are missing
  if (!all(map_lgl(inputFiles, file.exists))) {
    
    # Output the names of the missing files before sending a message
    missingFiles <- inputFiles[!map_lgl(inputFiles, file.exists)]
    
    
    cat("\n\n")
    cat("Missing File(s):\n")
    print(missingFiles)
    cat("\n\n")
    
    
    # Output the error message too
    stop(paste0("Missing Required Input File", 
                if_else(length(missingFiles) > 1, "s", ""), "\n\n",
                "This script requires that the PRISM, NOAA, RAWS, CIMIS, and CDEC ",
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
  cdecInput <- inputFiles$CDEC_INPUT[1] |> getFile() |> unique()
  
  prismDF <- getPRISM(inputFiles$PRISM_OUTPUT[1])
  noaaDF <- getDelim(inputFiles$NOAA_OUTPUT[1], delim = ",")
  rawsDF <- getDelim(inputFiles$RAWS_OUTPUT[1], delim = ",")
  cimisDF <- getDelim(inputFiles$CIMIS_OUTPUT[1], delim = ",")
  cdecDF <- getDelim(inputFiles$CDEC_OUTPUT[1], delim = ",")
  
  outlierDF <- getFile(inputFiles$PRECIP_OUTLIER_BOUNDS[1])
  corrDF <- getFile(inputFiles$PRECIP_GAGE_CORRELATION[1])
  
  
  # Validate all variables next
  cat("[1/2]\tChecking all input files...\n")
  
  
  # Ensure that all eight primary files have the expected formatting
  validateInputs(prismInput, noaaInput, rawsInput, cimisInput, cdecInput,
                 prismDF, noaaDF, rawsDF, cimisDF, cdecDF, inputFiles)
  
  
  # Check 'outlierDF' next too
  validateOutlierFile(outlierDF, inputFiles$PRECIP_OUTLIER_BOUNDS[1],
                      prismInput$PRMS_PRECIP_NAME |> na.omit())
  
  
  # Finally, check 'corrDF'
  validateCorrFile(corrDF, inputFiles$PRECIP_GAGE_CORRELATION[1],
                   prismInput$PRMS_PRECIP_NAME |> na.omit())
  
  
  cat("\tDone!\n\n")
  
  
  # After all validation requirements have been cleared, prepare a single
  # meteorological dataset (combining data from NOAA, RAWS, and CIMIS)
  cat("[2/2]\tPreparing final meteorological dataset...\n")
  
  
  meteorDF <- combineMeteorologicalDatasets(noaaInput, rawsInput, cimisInput,
                                            cdecInput,
                                            noaaDF, rawsDF, cimisDF, cdecDF,
                                            startDate, endDate)
  
  
  # For archival purposes, save 'meteorDF' without any data substitution
  # or outlier modifications
  meteorDF |>
    writeOutput(paste0("W2_Russian_River/Output/PRMS_Meteorological_No_QC_", 
                       startDate, "_", endDate, ".csv"),
                quietly = TRUE)
  
  
  # After that, check for and remove outliers from the dataset
  # Then, fill in empty entries using other gages' data or PRISM values
  meteorDF <- datQAQC(meteorDF, outlierDF, corrDF, 
                      cimisInput, cimisDF, inputFiles$CIMIS_OUTPUT,
                      cdecInput, cdecDF, inputFiles$CDEC_OUTPUT, 
                      prismDF, prismInput, allTempColumnsFromPRISM,
                      startDate, endDate, 
                      fullQAQC = TRUE)
  
  
  # Missing entries in this dataset will be substituted with gage and PRISM data
  # (And if 'allTempColumnsFromPRISM' is set to TRUE, all temperature data will 
  #  come from PRISM)
  
  
  # Now that QA/QC procedures have been completed, 
  # remove supplemental and extra gages from 'meteorDF'
  meteorDF <- meteorDF |>
    select(-starts_with("SUP_"), -starts_with("EX_"))
  
  
  cat("\tDone!\n\n")
  
  
  # Once this step is complete, write 'meteorDF' to a file
  outFile <- paste0("W2_Russian_River/Output/PRMS_Meteorological_", startDate, "_",
                    endDate, ".csv")
  
  
  meteorDF |>
    writeOutput(outFile)
  
  
  # Output a completion message
  cat(col_green("\n'RRW_006_Process_PRMS_Weather_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateInputs <- function (prismInput, noaaInput, rawsInput, cimisInput, cdecInput,
                            prismDF, noaaDF, rawsDF, cimisDF, cdecDF, inputFiles,
                            numPrecip = 45, numTemp = 8) {
  
  # Verify that all ten tibbles are formatted as expected
  
  # The number of expected PRMS precipitation columns is hard-coded as 45
  # Similarly, the number of expected minimum/maximum temperature columns is 8

  
  # First, check the five "INPUT" tibbles
  validateStationInputs(prismInput, inputFiles$PRISM_INPUT[1], "PRMS", numPrecip, numTemp)
  validateStationInputs(noaaInput, inputFiles$NOAA_INPUT[1], "PRMS", numPrecip, numTemp)
  validateStationInputs(rawsInput, inputFiles$RAWS_INPUT[1], "PRMS", numPrecip, numTemp)
  validateStationInputs(cimisInput, inputFiles$CIMIS_INPUT[1], "PRMS", numPrecip, numTemp)
  validateStationInputs(cimisInput, inputFiles$CDEC_INPUT[1], "PRMS", numPrecip, numTemp)
  
  
  # Validate the five weather output tibbles next
  
  # Each website returns data in a slightly different format
  # But the general expectations are similar in all cases
  validateWebData(prismDF, "PRISM", inputFiles$PRISM_OUTPUT[1], prismInput$STATION_ID, siPRISM = TRUE)
  validateWebData(noaaDF, "NOAA", inputFiles$NOAA_OUTPUT[1], noaaInput$STATION_ID)
  validateWebData(rawsDF, "RAWS", inputFiles$RAWS_OUTPUT[1], rawsInput$STATION_ID)
  validateWebData(cimisDF, "CIMIS", inputFiles$CIMIS_OUTPUT[1], cimisInput$STATION_ID)
  validateWebData(cdecDF, "CDEC", inputFiles$CDEC_OUTPUT[1], cdecInput$STATION_ID)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateOutlierFile <- function (outlierDF, sourcePath, stationNames) {
  
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
  if (nrow(outlierDF) != length(stationNames) ||
      !all(stationNames %in% outlierDF[["GAGE"]])) {
    
    paste0("Incompatible Number of Rows\n\n",
           "The file containing outlier bounds for each PRMS precipitation ",
           "gage is expected to have exactly one row for each of the ", 
           length(stationNames), " precipitation stations. The \"GAGE\" column ",
           "should only have \"PRECIP#\" as its values (with optional revision, ",
           "supplemental, and extra labels (e.g., \"_REV1\", \"SUP_\", or ",
           "\"EX_\"). However, this was not the case. Please investigate the ",
           "file for issues.\n\n",
           "(This error occurred for \"", sourcePath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, confirm that every "OUTLIER_LIMIT" column is numeric
  # These values should be either NA or a positive number
  if (outlierDF[toupper(paste0(month.abb, "_OUTLIER_LIMIT_MM"))] |>
      map_lgl(is.numeric) |> notAll() ||
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



validateCorrFile <- function (corrDF, sourcePath, stationNames) {
  
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
  
  
  # After that, confirm that no missing values are present in the "PREDICTOR" 
  # and "RESPONSE" columns of 'corrDF'
  if (corrDF |> select(PREDICTOR, RESPONSE) |> anyNA()) {
    
    # Print to the console the location of these NA values
    cat("\n\n")
    cat("Missing Element(s):\n")
    print(which(is.na(corrDF[c("PREDICTOR", "RESPONSE")]), arr.ind = TRUE)[, 1] |>
            unique())
    cat("\n\n")
    
    
    # Then output an error message
    paste0("Missing Values Detected\n\n",
           "The file containing linear regression models for PRMS precipitation ",
           "gages should not have any \"NA\" values in its \"PREDICTOR\" and ",
           "\"RESPONSE\" columns. However, at least one missing value was ",
           "detected (see above). Please investigate the file for issues.\n\n",
           "(This error occurred for \"", sourcePath, "\")") |>
      errWrap() |>
      stop()
    
  # A similar but slightly different check confirms that "SLOPE" and "INTERCEPT"
  # are never NA when "R_SQUARED" has a value
  } else if (corrDF |> filter(!is.na(R_SQUARED)) |> select(SLOPE, INTERCEPT) |> anyNA()) {
    
    # Print to the console the location of these NA values
    cat("\n\n")
    cat("Missing Element(s):\n")
    print(which(!is.na(corrDF[["R_SQUARED"]]) & 
                  is.na(corrDF[c("SLOPE", "INTERCEPT")]), arr.ind = TRUE)[, 1])
    cat("\n\n")
    
    
    # Then output an error message
    paste0("Missing Values Detected\n\n",
           "The file containing linear regression models for PRMS precipitation ",
           "gages should not have any \"NA\" values in its \"SLOPE\" and ",
           "\"INTERCEPT\" columns when \"R_SQUARED\" has a value. However, at ",
           "least one missing value was detected (see above). Please ",
           "investigate the file for issues.\n\n",
           "(This error occurred for \"", sourcePath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Iterate through every precipitation column (contained in 'stationNames')
  # Make sure it appears in a model with all other precipitation columns
  # (There should be one with "PRISM" too)
  for (i in 1:length(stationNames)) {
    
    # Take a subset of 'corrDF'
    # Get all models that involve this iteration's value from 'stationNames'
    subsetDF <- corrDF |>
      filter(PREDICTOR == stationNames[i] | RESPONSE == stationNames[i])
    
    
    # Extract all values in the "PREDICTOR" and "RESPONSE" columns
    colNames <- c(subsetDF$PREDICTOR, subsetDF$RESPONSE)
    
    
    # Confirm that every value in 'stationNames' appears within 'colNames'
    # (That means that every PRMS precipitation column was modeled against
    #  this iteration's specific precipitation column)
    
    # In addition, "PRISM" should also appear in 'colNames'
    # (This corresponds to the precipitation gage being modeled against 
    #  its PRISM counterpart)
    
    if (!all(stationNames %in% colNames) || !("PRISM" %in% colNames)) {
      
      cat("\n\n")
      cat(paste0("Missing Model(s) for ", stationNames[i], ":\n"))
      print(!(c(stationNames, "PRISM") %in% colNames))
      cat("\n\n")
      
      
      paste0("Missing Models for ", stationNames[i], " (And Maybe More)\n\n",
             "The file containing linear regression models for PRMS precipitation ",
             "gages should have models between every gage. Each of the ", 
             length(stationNames), " gages should have a model between it and ",
             "the other ", length(stationNames) - 1, " gages. In addition, ",
             "there should be a model with the gage's PRISM counterpart. ",
             "However, at least one gage does not have a complete set of ",
             "models (one gage is shown above--there may be more). Please ",
             "investigate the file for issues.\n\n",
             "(This error occurred for \"", sourcePath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
  }
  
  
  # Next, confirm that "SLOPE", "INTERCEPT", and "R_SQUARED" are all numeric values
  if (corrDF |> select(SLOPE, INTERCEPT, R_SQUARED) |> 
      map_lgl(is.numeric) |> notAll()) {
    
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
  if (corrDF |> filter(!is.na(R_SQUARED) & (R_SQUARED < 0 | R_SQUARED > 1)) |>
      nrow() > 0) {
    
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
  
  
  # Make sure every precipitation gage has at least one non-NA model entry 
  if (!all(stationNames %in% 
           unlist(corrDF |> filter(!is.na(R_SQUARED)) |> 
                  select(PREDICTOR, RESPONSE), use.names = FALSE))) {
    
    # Every gage should appear at least once among the predictor and response 
    # variables for models that are valid and have both coefficients and an
    # R^2 value
    
    paste0("No Models Found\n\n",
           "At least one gage in the correlation CSV file lacks linear regression ",
           "models with any other gage (even PRISM). This is a sign of an error ",
           "in the correlation file. Please investigate it for issues.\n\n",
           "(This error occurred for \"", sourcePath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



combineMeteorologicalDatasets <- function (noaaInput, rawsInput, cimisInput,
                                           cdecInput, 
                                           noaaDF, rawsDF, cimisDF, cdecDF,
                                           startDate, endDate) {
  
  # Format the data for easier integration into the PRMS DAT file
  # For each station, the relevant PRMS column names are listed in the input files
  
  
  # Start with building a skeleton for the final dataset
  meteorDF <- tibble(DATE = seq(from = startDate, to = endDate, by = "days"))
  
  
  # Add columns for precipitation, minimum temperature, and maximum temperature
  
  
  # To help specify these column names (and get their ordering right),
  # make a data frame for the column names
  prmsColumnNames <- c(noaaInput$PRMS_PRECIP_NAME, rawsInput$PRMS_PRECIP_NAME, 
                       cimisInput$PRMS_PRECIP_NAME, cdecInput$PRMS_PRECIP_NAME,
                       
                       noaaInput$PRMS_TMIN_NAME, rawsInput$PRMS_TMIN_NAME, 
                       cimisInput$PRMS_TMIN_NAME, cdecInput$PRMS_TMIN_NAME,
                       
                       noaaInput$PRMS_TMAX_NAME, rawsInput$PRMS_TMAX_NAME, 
                       cimisInput$PRMS_TMAX_NAME, cdecInput$PRMS_TMAX_NAME) |>
    unique() |> sort() |>
    matrix(ncol = 1) |> data.frame() |> set_names("COLUMN") |>
    filter(!is.na(COLUMN)) |>
    mutate(TYPE = str_remove(COLUMN, "[0-9]+(_.+)?$"),
           NUMBER = str_extract(COLUMN, "[0-9]+(?=(_|$))") |> as.numeric()) |>
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
  
  cdecProcessed <- cdecDF |>
    reformatClimateData(cdecInput, "CDEC")
  
  
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
    left_join(cimisProcessed, by = "DATE", relationship = "one-to-one") |>
    # Repeat with CDEC
    select(-all_of(names(cdecProcessed)[names(cdecProcessed) != "DATE"])) |>
    left_join(cdecProcessed, by = "DATE", relationship = "one-to-one")
  
  
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

  
  # Before performing that step, check if the input data is from NOAA or CDEC
  # These datasets have US customary units rather than metric units
  if (dataSource == "NOAA") {
    
    # Convert precipitation data from units of inches into millimeters
    # in * 25.4 mm / in
    
    # Convert the temperature data from Fahrenheit into Celsius as well
    # (deg-F - 32) * 5/9 = deg-C
    
    climateDF <- climateDF |>
      mutate(PRCP = PRCP * 25.4,
             TMAX = 5/9 * (TMAX - 32),
             TMIN = 5/9 * (TMIN - 32))
    
  } else if (dataSource == "CDEC") {
    
    # Convert units of inches into millimeters
    # Also, add dummy columns for "TMIN" and "TMAX" 
    # (only precipitation data is downloaded from CDEC)
    climateDF <- climateDF |>
      mutate(VALUE = VALUE * 25.4,
             NA_1 = NA,
             NA_2 = NA)
    
  }
  
  
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
                     cimisInput, cimisDF, cimisPath,
                     cdecInput, cdecDF, cdecPath, 
                     prismDF, prismInput, allTempSub, startDate, endDate, 
                     fullQAQC = TRUE) {
  
  # Perform different QA/QC routines to flag and replace suspicious precipitation data
  
  # If 'allTempSub' is TRUE, all temperature data will come from PRISM
  
  
  # Start by addressing CIMIS and CDEC data
  
  # Data from other stations is already modified with their own QA/QC procedures
  
  # CIMIS and CDEC have quality-control flags, but they are not applied by default
  # Use functions to perform those edits now
  meteorDF <- meteorDF |>
    applyFlags_CIMIS(cimisInput, cimisDF, cimisPath) |>
    applyFlags_CDEC(cdecInput, cdecDF, cdecPath)
  
  
  # Save 'meteorDF' to an intermediate file
  # CIMIS flags are the only QA/QC applied at this point, and this 
  meteorDF |>
    writeOutput(paste0("W2_Russian_River/Output/PRMS_Meteorological_QC_Intermediate_",
                       startDate, "_", endDate, ".csv"))
  
  
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



applyFlags_CIMIS <- function (meteorDF, cimisInput, cimisDF, cimisPath) {
  
  # The raw CIMIS data in 'cimisDF' has QA/QC flags as columns in the data
  # https://cimis.water.ca.gov/Content/PDF/CurrentFlags2.pdf
  
  # No corrections have been made, but these flags are present so that
  # users can make corrections at their discretion
  
  # This function will remove records that have certain precipitation flags
  
  
  # First, modify the column names in 'cimisDF' to be easier to work with
  # 'fieldNameVec' can help convert the raw headers in 'cimisDF' 
  fieldNameVec <- validateWebData_expectedColumnNames("CIMIS", siPRISM = TRUE)
  
  
  # Rename some columns in 'cimisDF'
  cimisDF <- cimisDF |>
    rename(all_of(fieldNameVec))
  
  
  # For convenience, filter 'cimisInput' as well
  # Keep only rows with values in "PRMS_PRECIP_NAME"
  # (These stations' precipitation values will be used in the model)
  cimisInput <- cimisInput |>
    filter(!is.na(PRMS_PRECIP_NAME))
  
  
  # Next, locate "PRECIP_QC" in 'cimisDF'
  # If it is not present, output an error message
  cimisDF |>
    checkMissingCol(colNames = "PRECIP_QC", 
                    msg = 
                      paste0("Missing CIMIS QC Column\n\n",
                             "The raw data downloaded from CIMIS should have ",
                             "contained a column labeled \"PRECIP_QC\". ",
                             "However, it could not be found. Please ",
                             "investigate the file.\n\n",
                             "(This error occurred for \"", cimisPath, "\""))
  
  
  # Next, identify precipitation records in 'cimisDF' with problematic flags
  
  # Data with these flags will be removed:
  #   (*) H - Severe issues in the underlying hourly precipitation data
  #           (e.g., missing or extreme values).
  #   (*) I - Meaningless data
  #   (*) R - Extreme data (e.g., value > 12 inches)
  #   (*) S - Sensor issues or very extreme data (e.g., value > 14 inches)
  
  
  # Record "DATE" and "STATION_ID" for entries that have suspect precipitation data
  flaggedDF <- cimisDF |>
    filter(STATION_ID %in% cimisInput$STATION_ID) |>
    filter(PRECIP_QC %in% c("H", "I", "R", "S")) |>
    select(DATE, STATION_ID)
  
  
  # Use 'cimisInput' to append PRMS precipitation information to 'flaggedDF'
  # "PRMS_PRECIP_NAME" will identify the actual PRMS column names (as it appears
  # in 'meteorDF') that correspond to CIMIS precipitation data
  flaggedDF <- flaggedDF |>
    left_join(cimisInput |> select(STATION_ID, PRMS_PRECIP_NAME),
              by = "STATION_ID", relationship = "many-to-one")
  
  
  # Iterate through each of the stations in 'cimisInput'
  for (i in 1:nrow(cimisInput)) {
    
    # Get the flagged dates associated with this specific CIMIS station
    errDates <- flaggedDF$DATE[flaggedDF$STATION_ID == cimisInput$STATION_ID[i]]
    
    
    # If 'errDates' is empty, skip to the next precipitation station
    if (length(errDates) == 0) {
      next
    }
    
    
    # Otherwise, update entries in 'meteorDF' for this CIMIS station
    # Flagged dates should have their precipitation values set to NA
    meteorDF[[cimisInput$PRMS_PRECIP_NAME[i]]][meteorDF$DATE %in% errDates] <- NA_real_
    
  }
  
  
  # Return 'meteorDF' afterwards
  return(meteorDF)
  
}



applyFlags_CDEC <- function (meteorDF, cdecInput, cdecDF, cdecPath) {
  
  # The raw CDEC data in 'cdecDF' has QA/QC flags as columns in the data
  # https://cdec.water.ca.gov/reportapp/javareports?name=FlagList
  
  # No corrections have been made, but these flags are present so that
  # users can make corrections at their discretion
  
  # This function will remove records that have certain precipitation flags
  
  
  # First, modify the column names in 'cdecDF' to be easier to work with
  # 'fieldNameVec' can help convert the raw headers in 'cdecDF' 
  fieldNameVec <- validateWebData_expectedColumnNames("CDEC", siPRISM = TRUE)
  
  
  # Rename some columns in 'cdecDF'
  cdecDF <- cdecDF |>
    rename(any_of(fieldNameVec))
  
  
  # For convenience, filter 'cdecInput' as well
  # Keep only rows with values in "PRMS_PRECIP_NAME"
  # (These stations' precipitation values will be used in the model)
  cdecInput <- cdecInput |>
    filter(!is.na(PRMS_PRECIP_NAME))
  
  
  # Next, locate "DATA_FLAG" in 'cdecDF'
  # If it is not present, output an error message
  cdecDF |>
    checkMissingCol(colNames = "DATA_FLAG", 
                    msg = 
                      paste0("Missing CDEC QC Column\n\n",
                             "The raw data downloaded from CDEC should have ",
                             "contained a column labeled \"DATA_FLAG\". ",
                             "However, it could not be found. Please ",
                             "investigate the file.\n\n",
                             "(This error occurred for \"", cdecPath, "\""))
  
  
  # Next, identify precipitation records in 'cdecDF' with problematic flags
  
  # Data with these flags will be removed:
  #   (*) A Precipitation accumulation
  #   (*) N Error in data
  #   (*) v Out of Valid Range
  
  
  # Record "DATE" and "STATION_ID" for entries that have suspect precipitation data
  flaggedDF <- cdecDF |>
    filter(STATION_ID %in% cdecInput$STATION_ID) |>
    filter(DATA_FLAG %in% c("A", "N", "v")) |>
    select(DATE, STATION_ID)
  
  
  # Use 'cdecInput' to append PRMS precipitation information to 'flaggedDF'
  # "PRMS_PRECIP_NAME" will identify the actual PRMS column names (as it appears
  # in 'meteorDF') that correspond to CDEC precipitation data
  flaggedDF <- flaggedDF |>
    left_join(cdecInput |> select(STATION_ID, PRMS_PRECIP_NAME),
              by = "STATION_ID", relationship = "many-to-one")
  
  
  # Iterate through each of the stations in 'cdecInput'
  for (i in 1:nrow(cdecInput)) {
    
    # Get the flagged dates associated with this specific CDEC station
    errDates <- flaggedDF$DATE[flaggedDF$STATION_ID == cdecInput$STATION_ID[i]]
    
    
    # If 'errDates' is empty, skip to the next precipitation station
    if (length(errDates) == 0) {
      next
    }
    
    
    # Otherwise, update entries in 'meteorDF' for this CIMIS station
    # Flagged dates should have their precipitation values set to NA
    meteorDF[[cdecInput$PRMS_PRECIP_NAME[i]]][meteorDF$DATE %in% errDates] <- NA_real_
    
  }
  
  
  # Return 'meteorDF' afterwards
  return(meteorDF)
  
}



removeOutliers <- function (meteorDF, outlierDF) {
  
  # Given upper-limit bounds for each PRMS precipitation gage, 
  # remove outliers from their datasets
  
  
  # Get a vector of precipitation columns that appear in 'meteorDF'
  precipNames <- names(meteorDF) |>
    str_subset("PRECIP[0-9]+")
  
  
  # Iterate through each of the precipitation gages in 'meteorDF'
  for (i in 1:length(precipNames)) {
    
    # Extract a subset of 'meteorDF' that contains "DATE" and the corresponding
    # precipitation column
    # (Add a "MONTH" column too)
    subsetDF <- meteorDF |>
      select(DATE, all_of(precipNames[i])) |>
      mutate(MONTH = month(DATE))
    
    
    # To make the process simpler, rename the gage in 'subsetDF' to "PRECIP"
    subsetDF <- subsetDF |>
      rename(PRECIP = all_of(precipNames[i]))
    
    
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
  
  
  # Make sure 'meteorDF' is sorted by date before proceeding
  meteorDF <- meteorDF |>
    arrange(DATE)
  
  
  # The exact procedure for each precipitation gage is this:
  
  #  (1) Find the gage with the highest correlation with this iteration's gage
  #
  #  (2) Wherever the chosen gage has available data, apply the model to fill in
  #      gaps within this iteration's gage data
  #
  #  (3) If there are still missing values in the gage dataset, find the next
  #      best correlating gage and repeat Step 2
  
  
  # First, create a copy of 'meteorDF'
  # Its missing entries will be adjusted and filled in
  adjDF <- meteorDF
  
  # The original data in 'meteorDF' will be preserved in this procedure
  # because gages' data can be used in other gages' model equations
  
  
  # Reformat PRISM data so that it can be used in the substitution process too
  prismProcessed <- reformatClimateData(prismDF, prismInput, "PRISM") |>
    select(all_of(names(meteorDF)))
  
  
  # Get a list of precipitation columns in 'adjDF'
  # (Exclude supplemental and extra gages from this)
  precipNames <- names(adjDF) |>
    str_subset("^PRECIP[0-9]+")
  
  
  # Next, iterate through each of the precipitation columns in 'adjDF'
  for (i in 1:length(precipNames)) {
    
    # Extract model information from 'corrDF' related to this iteration's gage
    gageCorr <- corrDF |>
      filter(PREDICTOR == precipNames[i] | RESPONSE == precipNames[i]) |>
      filter(!is.na(R_SQUARED)) |> 
      mutate(OTHER_GAGE = if_else(PREDICTOR == precipNames[i], RESPONSE, PREDICTOR)) |>
      arrange(desc(R_SQUARED))
    
    # The above code looks for records in 'corrDF' where the iteration's gage
    # appears as either the predictor variable (x) or the response variable (y)
    #
    # Then, it removes entries that lack a model and R^2 value
    #
    # After that, a column is added that identifies the name of the gage used
    # by the model that is NOT this iteration's gage
    #
    # Finally, the result is sorted so that the largest R^2 value is present
    # in the first row of 'gageCorr'
    
    
    # Define a counter that tracks which well-correlated gage will be used
    nthBest <- 0
    
    
    # Check for missing values in this column of 'adjDF'
    # Continuously loop until this issue is resolved
    while (anyNA(adjDF[[precipNames[i]]]) && nthBest < nrow(gageCorr)) {
      
      # Increment 'nthBest' (to use the next best gage)
      nthBest <- nthBest + 1
      
      
      # Get the dates where 'adjDF' is missing data for this gage
      missingDates <- adjDF$DATE[is.na(adjDF[[precipNames[i]]])]
      
      
      # Extract the nth best gage (as stated by 'nthBest')
      chosenModel <- gageCorr[nthBest, ]
      
      
      # If "OTHER_GAGE" is "PRISM", extract data from 'prismDF'
      # Otherwise, retrieve data from 'meteorDF'
      if (chosenModel$OTHER_GAGE[1] == "PRISM") {
        
        # In 'prismProcessed', a gage's PRISM counterpart uses the exact same 
        # column name
        modelDF <- prismProcessed |>
          select(DATE, all_of(precipNames[i])) |>
          rename(OTHER_GAGE = precipNames[i])
        
      } else {
        
        modelDF <- meteorDF |>
          select(DATE, all_of(chosenModel$OTHER_GAGE)) |>
          rename(OTHER_GAGE = chosenModel$OTHER_GAGE)
        
      }
      
      # (To make things tidier, the chosen gage's column is renamed to "OTHER_GAGE")
      
      
      # Filter 'modelDF' to days in 'missingDates'
      modelDF <- modelDF |>
        filter(DATE %in% missingDates)
      
      
      # If 'modelDF' is empty, skip to the next best gage
      if (nrow(modelDF) == 0) {
        next
      }
      
      
      # Otherwise, calculate a prediction for this iteration's gage
      #
      # If "OTHER_GAGE" is the x-variable, the gage's data can be calculated 
      # using the formula: RES = x * SLOPE + INTERCEPT
      #
      # However, if "OTHER_GAGE" is the y-variable, the formula must be modified:
      # RES = (y - INTERCEPT) / SLOPE
      if (chosenModel$PREDICTOR == chosenModel$OTHER_GAGE) {
        
        # RES = m * OTHER_GAGE + b
        modelDF <- modelDF |>
          mutate(RES = chosenModel$SLOPE * OTHER_GAGE + chosenModel$INTERCEPT)
        
      } else {
        
        # RES = (OTHER_GAGE - b) / m
        modelDF <- modelDF |>
          mutate(RES = (OTHER_GAGE - chosenModel$INTERCEPT) / chosenModel$SLOPE)
        
      }
      
      
      # Replace negative precipitation values in "RES" with 0
      modelDF$RES[!is.na(modelDF$RES) & modelDF$RES < 0] <- 0
      
      
      # In addition, wherever "OTHER_GAGE" is 0, set "RES" to 0 too
      # (The model may instead output a very small precipitation value for "RES"
      #  because of the y-intercept; however, in these cases, 0 would be preferable)
      modelDF$RES[!is.na(modelDF$RES) & modelDF$OTHER_GAGE == 0] <- 0
      
      
      # Replace the missing entries in 'adjDF' for this iteration's precipitation
      # gage using the predicted result from the nth best correlating gage 
      adjDF[[precipNames[i]]][adjDF$DATE %in% missingDates] <- modelDF$RES
      
      
      # It is possible that "OTHER_GAGE" is also missing data for dates
      # within 'missingDates'
      # 
      # In that case, the missing values for this iteration's gage would be 
      # replaced with NA (therefore remaining unchanged)
      # 
      # As a result, the next best gage would be considered
      #
      # (At worst, this process only continues until PRISM is the next best "gage" 
      #  because PRISM will not have any missing values)
      
    } # End of 'while' loop for missing gage data
    
    
    # If the precipitation gage still has missing values, the procedure failed
    # (The loop would've ended due to 'nthBest' reaching the end of 'gageCorr')
    if (anyNA(adjDF[[precipNames[i]]])) {
      
      paste0("The procedure failed to replace all missing values for ",
             precipNames[i], ". If a model with PRISM is present in the gage ",
             "correlations sheet, this should not happen. Please investigate.") |>
        errWrap() |>
        stop()
      
    }
    
    
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
    
    
    # Skip supplemental and extra gages too
    if (grepl("^((SUP)|(EX))_", names(meteorDF)[j])) {
      next
    }
    
    
    # If 'allTempSub' is TRUE, skip temperature columns too
    # (All temperature data will be replaced with PRISM values anyways)
    if (allTempSub && grepl("^((TMIN)|(TMAX))", names(meteorDF)[j])) {
      next
    }
    
    
    cat("\n\n")
    message(paste0("Direct PRISM substitution for missing values in ",
                   names(meteorDF)[j], "!"))
    cat("\n\n")
    
    
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
  # (ignoring supplemental and extra gages)
  if (anyNA(meteorDF |> select(-starts_with("SUP_"), -starts_with("EX_")))) {
    
    stop(paste0("Issue in `prismSub()`\n\n", 
                "By the end of this function, there should be no missing values ",
                "left in 'meteorDF'. However, \"NA\" ", 
                if_else(sum(is.na(meteorDF |> 
                                    select(-starts_with("SUP_"), 
                                           -starts_with("EX_")))) > 1, 
                            "values were ", "was "),
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

# This script contains functions related to generating meteorological datasets

# (These outputs are eventually incorporated into long-running DAT files for 
#  modeling with PRMS and/or SRP)

# The primary function of this script is `merge_weather_data`


#### Setup ####


# Import shared functions 
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

merge_weather_data <- function (startDate, endDate, model, 
                                prismInputPath, prismOutputPath, 
                                allTempColumnsFromPRISM = TRUE, 
                                siPRISM = model %notin% c("SRP"),
                                applyFullQAQC = TRUE, 
                                archiveFiles = TRUE, 
                                noaaInputPath = NULL, noaaOutputPath = NULL,
                                rawsInputPath = NULL, rawsOutputPath = NULL,
                                cimisInputPath = NULL, cimisOutputPath = NULL,
                                cdecInputPath = NULL, cdecOutputPath = NULL,
                                precipOutliersPath = NULL, precipCorrPath = NULL) {
  
  # This function takes file paths related to various meteorological data sources
  # (PRISM, NOAA, RAWS, CIMIS, and CDEC)
  
  # A precursor to the PRMS and SRP DAT files is developed from these datasets
  
  # The data sources' station input files contain information on which weather 
  # columns in the eventual DAT files their data correspond to
  
  # That information helps to build a meteorological CSV file
  
  # However, one additional step in this function is performing QA/QC on the 
  # weather data
  
  # CIMIS and CDEC have data quality flags in their weather data that must be applied
  
  # And, in general, for all non-PRISM stations, outliers must be removed, 
  # and missing data must be filled in
  
  
  # Start with data validation
  cat("\n\n")
  cat("[1/2]\tChecking input files...\n")
  
  
  # If the full QAQC procedure will be applied, 
  # outlier bounds and gage correlation files are required
  error_if(applyFullQAQC &&
             any_null(precipOutliersPath, precipCorrPath),
           paste0("Missing Required Files\n\n",
                  "If data from NOAA, RAWS, CIMIS, or CDEC will be incorporated ",
                  "into the dataset, files containing outlier bounds and gage ",
                  "correlations must be provided."))
  
  
  # If either 'noaaInputPath' or 'noaaOutputPath' is not NULL, the other 
  # shouldn't be NULL too (and vice versa)
  check_null_input(noaaInputPath, noaaOutputPath, "NOAA")
  
  
  # This is true for the RAWS, CIMIS, and CDEC variables too
  check_null_input(rawsInputPath, rawsOutputPath, "RAWS")
  check_null_input(cimisInputPath, cimisOutputPath, "CIMIS")
  check_null_input(cdecInputPath, cdecOutputPath, "CDEC")
  
  
  # After that, confirm that all files exist
  c(prismInputPath, prismOutputPath, noaaInputPath, noaaOutputPath, 
    rawsInputPath, rawsOutputPath, cimisInputPath, cimisOutputPath, 
    cdecInputPath, cdecOutputPath, precipOutliersPath, precipCorrPath) |>
    check_if_missing_file()
  
  # NOTE
  # In R, NULL values cannot be present in vectors
  #
  # As a result, if any of these inputs are NULL, they are automatically 
  # excluded from the vector that is sent to the validation function
  
  
  # Read in the files next (if they are not NULL)
  prismInput <- prismInputPath |> read_not_null_files() |> unique()
  noaaInput <- noaaInputPath |> read_not_null_files() |> unique()
  rawsInput <- rawsInputPath |> read_not_null_files() |> unique()
  cimisInput <- cimisInputPath |> read_not_null_files() |> unique()
  cdecInput <- cdecInputPath |> read_not_null_files() |> unique()
  
  prismDF <- prismOutputPath |> getPRISM()
  noaaDF <- noaaOutputPath |> read_not_null_files(delim = ",")
  rawsDF <- rawsOutputPath |> read_not_null_files(delim = ",")
  cimisDF <- cimisOutputPath |> read_not_null_files(delim = ",")
  cdecDF <- cdecOutputPath |> read_not_null_files(delim = ",")
  
  outlierDF <- precipOutliersPath |> read_not_null_files()
  corrDF <- precipCorrPath |> read_not_null_files()
  
  
  # Before proceeding, define 'numPrecip' and 'numTemp' based on 'prismInput'
  # The total number of expected precipitation and temperature stations 
  # will be based on its three model fields ("[MODEL]_PRECIP_NAME", 
  # "[MODEL]_TMIN_NAME", and "[MODEL]_TMAX_NAME")
  numPrecip <- get_num_stations(model, prismInput, prismInputPath, "PRECIP")
  
  numTemp <- get_num_stations(model, prismInput, prismInputPath, "TEMP")
  
  
  # Validate all of the weather inputs next
  validate_inputs(prismInputPath, prismOutputPath, prismInput, prismDF,
                  noaaInputPath = noaaInputPath, noaaOutputPath = noaaOutputPath, 
                  noaaInput = noaaInput, noaaDF = noaaDF, 
                  rawsInputPath = rawsInputPath, rawsOutputPath = rawsOutputPath, 
                  rawsInput = rawsInput, rawsDF = rawsDF, 
                  cimisInputPath = cimisInputPath, cimisOutputPath = cimisOutputPath, 
                  cimisInput = cimisInput, cimisDF = cimisDF, 
                  cdecInputPath = cdecInputPath, cdecOutputPath = cdecOutputPath, 
                  cdecInput = cdecInput, cdecDF = cdecDF,
                  numPrecip = numPrecip, numTemp = numTemp, 
                  siPRISM = siPRISM, model = model)
  
  
  # Check 'outlierDF' next too
  validate_outlier_file(outlierDF, precipOutliersPath,
                        prismInput[[paste0(model, "_PRECIP_NAME")]] |> na.omit(),
                        model)
  
  
  # Finally, check 'corrDF'
  validate_corr_file(corrDF, precipCorrPath,
                     prismInput[[paste0(model, "_PRECIP_NAME")]] |> na.omit(),
                     model)
  
  
  cat("\tDone!\n\n")
  
  
  # After all validation requirements have been cleared, prepare a single
  # meteorological dataset (combining data from NOAA, RAWS, CIMIS, and CDEC
  # if they have values)
  cat("[2/2]\tPreparing meteorological dataset...\n")
  
  
  meteorDF <- merge_datasets(startDate, endDate, model,
                             noaaInput, rawsInput, cimisInput, cdecInput,
                             noaaDF, rawsDF, cimisDF, cdecDF)
  
  # Note: 'meteorDF' will be NULL if all four data sources will NOT be used
  
  
  # Process PRISM data separately into a mirror image of 'meteorDF'
  # (i.e., same columns, same number of rows, just potentially different values)
  prismProcessed <- reformat_climate_data(prismDF, prismInput, "PRISM", 
                                          model, startDate, endDate, siPRISM)
  
  
  # The next steps depend on whether the DAT will be PRISM-only or not
  
  # If 'meteorDF' is not NULL, it contains data from other sources
  # In that case, QA/QC procedures, must be applied
  
  # Define variables to hold the planned filenames for pre-QC and intermediate-QC
  # versions of 'meteorDF' as it goes through QC procedures
  # (This will be important if archiving is performed)
  noQCPath <- NULL
  intermediatePath <- NULL
  
  
  # Check next if 'meteorDF' uses data from non-PRISM sources
  if (!is.null(meteorDF)) {
    
    # Update 'noQCPath' and 'intermediateQCPath' with proper filenames
    noQCPath <- paste0("W2_Russian_River/Output/", model, "_Meteorological_No_QC_", 
                       startDate, "_", endDate, ".csv")
    
    
    intermediatePath <- paste0("W2_Russian_River/Output/", model, 
                               "_Meteorological_QC_Intermediate_",
                               startDate, "_", endDate, ".csv")
    
    
    # Apply QA/QC procedures to 'meteorDF'
    meteorDF <- meteorDF |>
      apply_dat_qaqc(outlierDF, corrDF, 
                     cimisInput, cimisDF, cimisOutputPath,
                     cdecInput, cdecDF, cdecOutputPath, 
                     prismProcessed, allTempColumnsFromPRISM, 
                     startDate, endDate, model, 
                     noQCPath, intermediatePath, 
                     fullQAQC = applyFullQAQC)
    
    # Otherwise, if only PRISM data will appear in the QA/QC procedure, 
    # simply define 'meteorDF' to equal 'prismProcessed'
  } else {
    
    meteorDF <- prismProcessed
    
  }
  
  
  # Now that QA/QC procedures have been completed, 
  # remove supplemental and extra gages from 'meteorDF' (if present)
  meteorDF <- meteorDF |>
    select(-starts_with("SUP_"), -starts_with("EX_"))
  
  
  # Write 'meteorDF' to a file after that
  outFile <- paste0("W2_Russian_River/Output/", model, "_Meteorological_", 
                    startDate, "_", endDate, ".csv")
  
  
  meteorDF |>
    writeOutput(outFile)
  
  
  # As a final step, check if 'archiveFiles' is TRUE
  # That means that the weather files should be saved to the model archive folder
  if (archiveFiles == TRUE) {
    
    # Save copies of the key climate files and add metadata too 
    archive_climate_files(startDate, endDate, model, 
                          noQCPath, intermediatePath, meteorPath = outFile, 
                          prismInputPath, prismOutputPath, 
                          noaaInputPath, noaaOutputPath,
                          rawsInputPath, rawsOutputPath,
                          cimisInputPath, cimisOutputPath,
                          cdecInputPath, cdecOutputPat,
                          precipOutliersPath, precipCorrPath)
    
  }
  
  
  
  cat("\tDone!\n\n")
  
  
  # End the procedure by returning 'outFile'
  return(outFile)
  
}



check_null_input <- function (inputPath, outputPath, source) {
  
  # Most function inputs for `merge_weather_data` come in pairs by data source
  # (i.e., each pair has a station input and weather output path variable)
  
  # They should be consistently specified
  # If one in a pair is NULL, the other should be NULL too
  # If one in a pair is not NULL, the other should not be NULL too
  
  
  # Output an error if the pairs are not consistent in being NULL
  error_if(!is_consistent_null(inputPath, outputPath),
           paste0("Inconsistent Input Values\n\n",
                  "If either a station input file or a weather output file for ",
                  source, " is provided, the other file must be specified as ",
                  "well. Either both parameters should be NULL, or both should ",
                  "have a value."))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



read_not_null_files <- function (path, delim = NULL) {
  
  # Try to read in a filepath stored in 'path'
  
  # However, 'path' may be NULL instead
  # If that is the case, return NULL
  
  # In addition, if 'delim' is specified in this function, 
  # use `getDelim` instead of `getFile` to read in the file
  
  
  # First check if 'path' is actually NULL
  if (is.null(path)) {
    return(NULL)
  }
  
  
  # Next, check if 'delim' is NULL
  if (is.null(delim)) {
    
    # If 'delim' is NULL, use `getFile`
    return(getFile(path))
    
    # Otherwise, use `getDelim`
  } else {
    
    return(getDelim(path, delim = delim))
    
  }
  
}



get_num_stations <- function (model, prismInput, prismInputPath, fieldType) {
  
  # Each PRISM file should have columns that identify which stations and parameters
  # are used by the model 
  
  # They have names like "[MODEL]_PRECIP_NAME" and "[MODEL]_TMIN_NAME"
  
  # A weather station row that has a value in these fields will be used in the 
  # modeling procedure (for precipitation and/or air temperature)
  
  # Identify the unique number of non-NA values for each of these fields
  
  # Note 1: The PRISM file is used for this check because every weather station
  #         from any source is required to have PRISM-equivalent data downloaded 
  #         to support QAQC efforts 
  
  # Note 2: "[MODEL]_TMIN_NAME" and "[MODEL]_TMAX_NAME" should have 
  #         the same number of values
  
  
  # Start by confirming that the model columns exist in the PRISM station file 
  fieldNames <- paste0(model, c("_PRECIP_NAME", "_TMIN_NAME", "_TMAX_NAME")) |>
    set_names(c("PRECIP", "TMIN", "TMAX"))
  
  
  checkMissingCol(prismInput, fieldNames,
                  prismInputPath, infoStr = paste0("PRISM station CSV file for ", model))
  
  
  # After that, if 'fieldType' is "PRECIP", return the number of unique non-NA
  # entries in its field
  if (fieldType == "PRECIP") {
    
    return(prismInput[[fieldNames[names(fieldNames) == "PRECIP"]]] |>
             na.omit() |>
             unique() |>
             str_subset("^PRECIP") |>   # Ignore supplemental and extra gages
             length())
    
  # A similar procedure will be performed for temperature
  # However, the number of min and max temperature stations used by the model should match 
  } else if (fieldType == "TEMP") {
    
    # Get the number of unique temperature stations
    minVal <- prismInput[[fieldNames[names(fieldNames) == "TMIN"]]] |>
      na.omit() |>
      unique() |>
      length()
    
    
    maxVal <- prismInput[[fieldNames[names(fieldNames) == "TMAX"]]] |>
      na.omit() |>
      unique() |>
      length()
    
    
    error_if(minVal != maxVal,
             paste0("Mismatch in PRISM station CSV\n\n",
                    "The PRISM file containing all weather stations for ", 
                    model, " has an issue. The number of specified \"TMIN\" ",
                    "stations does not match the number of \"TMAX\" stations.\n\n",
                    "Please revise the file (\"", prismInputPath, "\")"))
    
    
    # Return 'minVal' if there are no issues
    return(minVal)
    
  } else {
    
    stop_script("Unknown 'fieldType' value received! Please revise the script.")
    
  }
  
}



validate_inputs <- function (prismInputPath, prismOutputPath, prismInput, prismDF, 
                             noaaInputPath = NULL, noaaOutputPath = NULL, 
                             noaaInput = NULL, noaaDF = NULL, 
                             rawsInputPath = NULL, rawsOutputPath = NULL,
                             rawsInput = NULL, rawsDF = NULL,
                             cimisInputPath = NULL, cimisOutputPath = NULL,
                             cimisInput = NULL, cimisDF = NULL,
                             cdecInputPath = NULL, cdecOutputPath = NULL, 
                             cdecInput = NULL, cdecDF = NULL,
                             numPrecip = 45, numTemp = 8, 
                             siPRISM = TRUE, model = "PRMS") {
  
  # Verify that all weather tibbles are formatted as expected
  
  # 'numPrecip' and 'numTemp' control the number of expected columns for
  # precipitation and minimum/maximum temperature
  
  
  # First, check the five station input tibbles (if they are not NULL)
  validateStationInputs(prismInput, prismInputPath, model, numPrecip, numTemp)
  
  if (!is.null(noaaInput)) {
    validateStationInputs(noaaInput, noaaInputPath, model, numPrecip, numTemp)
  }
  
  if (!is.null(rawsInput)) {
    validateStationInputs(rawsInput, rawsInputPath, model, numPrecip, numTemp)
  }
  
  if (!is.null(cimisInput)) {
    validateStationInputs(cimisInput, cimisInputPath, model, numPrecip, numTemp)
  }
  
  if (!is.null(cdecInput)) {
    validateStationInputs(cdecInput, cdecInputPath, model, numPrecip, numTemp)
  }
  
  
  # Validate the weather output tibbles next
  
  # Each website returns data in a slightly different format
  # But the general expectations are similar in all cases
  validateWebData(prismDF, "PRISM", prismOutputPath, prismInput$STATION_ID, 
                  siPRISM = siPRISM)
  
  if (!is.null(noaaDF)) {
    validateWebData(noaaDF, "NOAA", noaaOutputPath, noaaInput$STATION_ID)
  }
  
  if (!is.null(rawsDF)) {
    validateWebData(rawsDF, "RAWS", rawsOutputPath, rawsInput$STATION_ID)
  }
  
  if (!is.null(cimisDF)) {
    validateWebData(cimisDF, "CIMIS", cimisOutputPath, cimisInput$STATION_ID)
  }
  
  if (!is.null(cdecDF)) {
    validateWebData(cdecDF, "CDEC", cdecOutputPath, cdecInput$STATION_ID)
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validate_outlier_file <- function (outlierDF, sourcePath, stationNames, 
                                   model = "PRMS") {
  
  # Inspect 'outlierDF' and ensure that all precipitation gages have
  # outlier bounds for every month
  
  
  # Though, if 'outlierDF' is NULL, return nothing
  if (is.null(outlierDF)) {
    return(invisible(NULL))
  }
  
  
  # Every month should have an outlier limit column
  expectedCols <- c("GAGE",
                    paste0(month.abb, "_OUTLIER_LIMIT_MM") |> toupper())
  
  
  # Check for missing columns
  outlierDF |>
    checkMissingCol(expectedCols, sourcePath, 
                    infoStr = paste0("file containg upper outlier bounds ",
                                     "(in mm) for ", model, " precipitation gages"))
  
  
  # After that, confirm that one row is present in 'outlierDF' 
  # for every model precipitation column
  if (nrow(outlierDF) != length(stationNames) ||
      !all(stationNames %in% outlierDF[["GAGE"]])) {
    
    paste0("Incompatible Number of Rows\n\n",
           "The file containing outlier bounds for each ", model, " precipitation ",
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



validate_corr_file <- function (corrDF, sourcePath, stationNames, model = "PRMS") {
  
  # Check the values in 'corrDF' 
  # Ensure that all model precipitation gages have model values 
  # between themselves and with their respective PRISM datasets too
  
  
  # Though, if 'corrDF' is NULL, return nothing
  if (is.null(corrDF)) {
    return(invisible(NULL))
  }
  
  
  # Check for five key columns
  expectedCols <- c("PREDICTOR", "RESPONSE", "SLOPE", "INTERCEPT", "R_SQUARED")
  
  
  # Check for missing columns
  corrDF |>
    checkMissingCol(expectedCols, sourcePath, 
                    infoStr = paste0("file containg linear regression model ",
                                     "parameters for ", model, " precipitation gages"))
  
  
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
           "The file containing linear regression models for ", model, " precipitation ",
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
           "The file containing linear regression models for ", model, " precipitation ",
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
             "The file containing linear regression models for ", model, " precipitation ",
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
           "The file containing linear regression models for ", model, " precipitation ",
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
           "The file containing linear regression models for ", model, " precipitation ",
           "gages should have a column that shows each model's R^2 metric. ",
           "This column is expected to have values that range between 0 and 1 ",
           "(inclusive). However, that was not the case. Please investigate ",
           "the file for issues.\n\n",
           "(This error occurred for \"", sourcePath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Make sure every primary precipitation gage has at least one non-NA model entry 
  # (It is fine if "EX" or "SUP" gages do not have any models)
  if (!all(str_subset(stationNames, "^PRECIP") %in% 
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



merge_datasets <- function (startDate, endDate, 
                            model = "PRMS",
                            noaaInput = NULL, rawsInput = NULL, 
                            cimisInput = NULL, cdecInput = NULL, 
                            noaaDF = NULL, rawsDF = NULL, 
                            cimisDF = NULL, cdecDF = NULL) {
  
  # Format the data for easier integration into the DAT file
  # For each station, the relevant column names are listed in their input files
  
  
  # First check if all climate data sources are NULL
  # In that case, just return NULL instead
  if (all_null(noaaDF, rawsDF, cimisDF, cdecDF)) {
    return(NULL)
  }
  
  
  # After that, start with building a skeleton for the final dataset
  meteorDF <- tibble(DATE = seq(from = startDate, to = endDate, by = "days"))
  
  
  # Add columns for precipitation, minimum temperature, and maximum temperature
  
  
  # To help specify these column names (and get their ordering right),
  # make a data frame for the planned model column names
  prmsColumnNames <- c(noaaInput[[paste0(model, "_PRECIP_NAME")]], rawsInput[[paste0(model, "_PRECIP_NAME")]], 
                       cimisInput[[paste0(model, "_PRECIP_NAME")]], cdecInput[[paste0(model, "_PRECIP_NAME")]],
                       
                       noaaInput[[paste0(model, "_TMIN_NAME")]], rawsInput[[paste0(model, "_TMIN_NAME")]], 
                       cimisInput[[paste0(model, "_TMIN_NAME")]], cdecInput[[paste0(model, "_TMIN_NAME")]],
                       
                       noaaInput[[paste0(model, "_TMAX_NAME")]], rawsInput[[paste0(model, "_TMAX_NAME")]], 
                       cimisInput[[paste0(model, "_TMAX_NAME")]], cdecInput[[paste0(model, "_TMAX_NAME")]]) |>
    unique() |> sort(na.last = NA) |>
    matrix(ncol = 1) |> data.frame() |> set_names("COLUMN") |>
    filter(!is.na(COLUMN)) |>
    mutate(TYPE = str_remove(COLUMN, "[0-9]+(_.+)?$"),
           NUMBER = str_extract(COLUMN, "[0-9]+(?=(_|$))") |> as.numeric()) |>
    arrange(TYPE, NUMBER)
  
  # The above code pools together all PRMS-related field names into a vector, 
  # then a matrix, and finally a data frame
  # The column is arbitrarily titled "COLUMN" 
  # Then, two variables are created based on the type of model variable and the 
  # value of the column name's number
  # Finally, the data frame is sorted based on the column type 
  # (PRECIP > TMAX > TMIN) and the column number
  
  # Note: With the default value of "NA" for the argument "na.last" in sort(), 
  # the NA entries are removed automatically
  # But just for redundancy, a filter to remove "NA" is also applied to the data frame
  
  # Note 2: If any of these input variables are NULL, they add NULL to the vector,
  # which gets removed automatically from 'prmsColumnNames'
  
  
  # Add these columns to 'meteorDF'
  meteorDF[prmsColumnNames$COLUMN] <- NA_real_
  
  
  # In another function, reformat the climate tibble(s) 
  # Then, append them to 'meteorDF', replacing the placeholder columns in 'meteorDF'
  # with the actual downloaded data
  meteorDF <- meteorDF |>
    process_climate_data(noaaDF, noaaInput, "NOAA", startDate, endDate, model) |>
    process_climate_data(rawsDF, rawsInput, "RAWS", startDate, endDate, model) |>
    process_climate_data(cimisDF, cimisInput, "CIMIS", startDate, endDate, model) |>
    process_climate_data(cdecDF, cdecInput, "CDEC", startDate, endDate, model)
  
  
  # Return the revised 'meteorDF'
  return(meteorDF |>
           select(DATE, all_of(prmsColumnNames$COLUMN)))
  
}



process_climate_data <- function (meteorDF, climateDF, climateInput, dataSource,
                                  startDate, endDate, model, 
                                  siPRISM = TRUE) {
  
  # Replace placeholder columns in 'meteorDF'
  
  # Each climate data source supplies certain columns to the model DAT file
  
  # Process the downloaded data in 'climateDF' and integrate it into 'meteorDF'
  
  
  # Though, if 'climateDF' is NULL, make no changes to 'meteorDF' and just return it
  # (A model might not rely on all potential data sources)
  if (is.null(climateDF)) {
    return(meteorDF)
  }
  
  
  # Otherwise, process the data in 'climateDF'
  climateProcessed <- climateDF |>
    reformat_climate_data(climateInput, dataSource, model, startDate, endDate, siPRISM)
  
  
  # Bind the processed tibble to 'meteorDF'
  # (Other than "DATE", the columns in the processed tibble should replace 
  #  the corresponding placeholders in 'meteorDF')
  meteorDF <- meteorDF |>
    # Aside from "DATE", remove all model fields from 'meteorDF' that 
    # appear in the processed tibble
    select(-all_of(names(climateProcessed)[names(climateProcessed) != "DATE"])) |>
    # Then, join the processed tibble to 'meteorDF'
    left_join(climateProcessed, by = "DATE", relationship = "one-to-one")
  
  
  # Return 'meteorDF'
  return(meteorDF)  
  
}



reformat_climate_data <- function (climateDF, climateInput, dataSource, 
                                   model, startDate, endDate, siPRISM) {
  
  # The 'climateDF' data frames need to be widened 
  # (so that each station's data is in its own separate column)
  
  
  # The model column names in 'climateInput' will then be used to sort 
  # stations' data into those fields
  fieldNameVec <- validateWebData_expectedColumnNames(dataSource, siPRISM = siPRISM)
  
  
  # Before performing that step, there are a few edits that are required
  
  # First, for CDEC, only precipitation data is downloaded from that source
  # Dummy columns are needed for maximum and minimum temperature
  if (dataSource == "CDEC") {
    
    climateDF <- climateDF |>
      mutate(!! fieldNameVec[names(fieldNameVec) == "TMIN"] := NA,
             !! fieldNameVec[names(fieldNameVec) == "TMAX"] := NA)
    
  }
  
  
  # Next, check the units of the climate data
  # For PRMS and similar models, SI units are required
  # For SRP and similar models, US Customary units are required
  
  # This is hard-coded into the scripts, but:
  #   (*) PRISM (PRMS) is downloaded in SI units
  #   (*) PRISM (SRP) is downloaded in US units
  #   (*) NOAA is downloaded in US units
  #   (*) RAWS is downloaded in SI units
  #   (*) CIMIS is downloaded in SI units
  #   (*) CDEC is downloaded in US units
  if (model %in% c("PRMS") && dataSource %in% c("NOAA", "CDEC")) {
    
    # Convert US units to SI units
    
    # Convert precipitation data from units of inches into millimeters
    # in * 25.4 mm / in
    
    # Convert the temperature data from Fahrenheit into Celsius as well
    # (deg-F - 32) * 5/9 = deg-C
    
    climateDF <- climateDF |>
      convert_climate_units(fieldNameVec, "PRECIP", toSI = TRUE) |>
      convert_climate_units(fieldNameVec, "TMAX", toSI = TRUE) |>
      convert_climate_units(fieldNameVec, "TMIN", toSI = TRUE)
    
  } else if (model %in% c("SRP") && dataSource %in% c("RAWS", "CIMIS")) {
    
    # Convert SI units to US units
    
    # Convert precipitation data from units of millimeters into inches
    # mm * 1 in / 25.4 mm
    
    # Convert the temperature data from Celsius into Fahrenheit
    # deg-F = 9/5 * deg-C + 32
    
    climateDF <- climateDF |>
      convert_climate_units(fieldNameVec, "PRECIP", toSI = FALSE) |>
      convert_climate_units(fieldNameVec, "TMAX", toSI = FALSE) |>
      convert_climate_units(fieldNameVec, "TMIN", toSI = FALSE)
    
  }
  
  
  # Once CDEC and/or the units have been taken care of, 
  # start by renaming the columns in 'climateDF' to be consistent 
  # Then, pivot the dataset into a wider format (where each station has 
  # three of its own columns--one for each model parameter field)
  widerDF <- climateDF |>
    select(all_of(fieldNameVec)) |>
    pivot_wider(names_from = STATION_ID,
                values_from = c(PRECIP, TMIN, TMAX),
                names_sep = "_")
  
  
  # After that, prepare the model-equivalent column names using 'climateInput'
  # Appending the station IDs to "PRECIP"/"TMAX"/"TMIN" gives the 
  # column names that appear in 'widerDF'
  # The values in "PRMS_PRECIP_NAME", "PRMS_TMAX_NAME", and "PRMS_TMIN_NAME" (or similar)
  # are the intended replacements for these column names
  equivalentNames <- climateInput |>
    mutate(NAME_1 = paste0("PRECIP_", STATION_ID),
           NAME_2 = paste0("TMAX_", STATION_ID),
           NAME_3 = paste0("TMIN_", STATION_ID)) |>
    select(NAME_1, NAME_2, NAME_3, 
           all_of(paste0(model, c("_PRECIP_NAME", "_TMAX_NAME", "_TMIN_NAME"))))
  
  
  # Create a vector from 'equivalentNames' that can be used with rename()
  renameVec <- c(equivalentNames$NAME_1, equivalentNames$NAME_2,
                 equivalentNames$NAME_3) |>
    set_names(c(equivalentNames[[paste0(model, "_PRECIP_NAME")]], 
                equivalentNames[[paste0(model, "_TMAX_NAME")]], 
                equivalentNames[[paste0(model, "_TMIN_NAME")]]))
  
  
  # Not every station will be used for precipitation and max/min temperature
  # In those cases, the names will be "NA"
  # Remove them from 'renameVec'
  renameVec <- renameVec[!is.na(names(renameVec)) & renameVec != "NA"]
  
  
  # After that, apply 'renameVec' to 'widerDF'
  # Then, keep DATE and the renamed variables only
  processedDF <- widerDF |>
    rename(any_of(renameVec)) |>
    select(DATE, any_of(names(renameVec)))
  
  
  # Finally, ensure that 'processedDF' is limited to the bounds of 
  # 'startDate' and 'endDate'
  processedDF <- processedDF |>
    filter(DATE >= startDate & DATE <= endDate)
  
  
  # Return 'processedDF'
  return(processedDF)
  
}



convert_climate_units <- function (climateDF, fieldNameVec, 
                                   param = "PRECIP", toSI = TRUE) {
  
  # Convert the units of a variable in 'climateDF' 
  
  # Every climate tibble has its own version of the column names at this point
  
  # Use 'param' to find the corresponding name in 'fieldNameVec'
  
  # Then, apply a factor to that column based on 'toSI'
  
  # IMPORTANT:
  # This function converts from mm to inches (or vice versa) for precipitation
  # For temperature, this function converts between deg-C and deg-F
  
  
  # First get the column name in 'climateDF' that corresponds to 'param'
  colName <- fieldNameVec[names(fieldNameVec) == param]
  
  
  if (length(colName) == 0) {
    paste0("Parameter \"", param, "\" not found. Please revise the script!") |>
      stop_script()
  }
  
  
  # Make conversions based on the type of parameter specified 
  if (param == "PRECIP") {
    
    # Next, check whether 'toSI' is TRUE or FALSE
    
    # The assumption is that the input units are in millimeters or inches,
    # and the other unit is desired
    
    # 1 inch = 25.4 millimeters
    
    if (toSI == TRUE) {
      
      # Convert from US Customary to SI units
      # Multiply by 25.4 mm/in
      
      climateDF <- climateDF |>
        mutate(!! colName := get(colName) * 25.4)
      
    } else {
      
      # Convert from SI units to US Customary
      # Multiply by 1 in / 25.4 mm
      
      climateDF <- climateDF |>
        mutate(!! colName := get(colName) / 25.4)
      
    }
    
  } else if (param %in% c("TMIN", "TMAX")) {
    
    # The conversion formula depends on whether 'toSI' is TRUE or FALSE
    
    # The assumption is that the input units are in Fahrenheit or Celsius,
    # and the other unit is desired
    
    # deg-C = 5/9 * (deg-F - 32)
    
    if (toSI == TRUE) {
      
      # Convert from US Customary to SI units
      # deg-C = 5/9 * (deg-F - 32)
      
      climateDF <- climateDF |>
        mutate(!! colName := 5/9 * (get(colName) - 32))
      
    } else {
      
      # Convert from SI units to US Customary
      # deg-F = 9/5 * deg-C + 32
      
      climateDF <- climateDF |>
        mutate(!! colName := 9/5 * get(colName) + 32)
      
    }
    
  } else {
    
    stop_script("Unknown parameter specified. Please revise the script!")
    
  }
  
  
  # Return 'climateDF' after these updates
  return(climateDF)
  
}




apply_dat_qaqc <- function (meteorDF, outlierDF, corrDF, 
                            cimisInput, cimisDF, cimisOutputPath,
                            cdecInput, cdecDF, cdecOutputPath, 
                            prismProcessed, allTempSub, 
                            startDate, endDate, model, 
                            noQCPath, intermediatePath, 
                            fullQAQC = TRUE) {
  
  # Perform different QA/QC routines to flag and replace suspicious precipitation data
  
  # If 'allTempSub' is TRUE, all temperature data will come from PRISM too
  
  
  # Before beginning, for archival purposes, 
  # save 'meteorDF' without any data substitution or outlier modifications
  meteorDF |>
    writeOutput(noQCPath, quietly = TRUE)
  
  
  # Start by addressing CIMIS and CDEC data
  
  # Data from other stations is already modified with their own QA/QC procedures
  
  # CIMIS and CDEC have quality-control flags, but they are not applied by default
  # Use functions to perform those edits now
  meteorDF <- meteorDF |>
    apply_qc_flags_CIMIS(cimisInput, cimisDF, cimisOutputPath, model) |>
    apply_qc_flags_CDEC(cdecInput, cdecDF, cdecOutputPath, model)
  
  
  # Save 'meteorDF' to an intermediate file
  # CIMIS and CDEC flags are the only QA/QC applied at this point
  meteorDF |>
    writeOutput(intermediatePath)
  
  
  # If 'fullQAQC' is TRUE, the outlier procedure and correlation-based 
  # replacement algorithm will be applied next
  if (fullQAQC) {
    
    # Identify outliers in the dataset and make them 'NA'
    meteorDF <- remove_outliers(meteorDF, outlierDF)
    
    
    # Then, try to use gage data and/or modified PRISM data to replace missing values
    meteorDF <- sub_missing_gage_data(meteorDF, corrDF, prismProcessed)
    
  }
  
  # 'fullQAQC' should be set to FALSE when the original SDA workflow for 
  # PRMS and SRP is desired
  
  # (In that case, only PRISM-based substitution occurs)
  
  
  # Regardless of the value of 'fullQAQC', 
  # replace any remaining missing values with direct PRISM values
  # (Also, if 'allTempSub' is TRUE, all temperature values will come from PRISM)
  meteorDF <- sub_data_with_PRISM(meteorDF, prismProcessed, allTempSub)
  
  
  # Return 'meteorDF'
  return(meteorDF)
  
}



apply_qc_flags_CIMIS <- function (meteorDF, cimisInput, cimisDF, cimisOutputPath,
                                  model) {
  
  # The raw CIMIS data in 'cimisDF' has QA/QC flags as columns in the data
  # https://cimis.water.ca.gov/Content/PDF/CurrentFlags2.pdf
  
  # No corrections have been made, but these flags are present so that
  # users can make corrections at their discretion
  
  # This function will remove records that have certain precipitation flags
  
  
  # Though, if 'cimisDF' is NULL, that means that there is no CIMIS data
  # used by this model
  if (is.null(cimisDF)) {
    
    # Return 'meteorDF' without any changes in that case
    return(meteorDF)
    
  }
  
  
  # First, modify the column names in 'cimisDF' to be easier to work with
  # 'fieldNameVec' can help convert the raw headers in 'cimisDF' 
  fieldNameVec <- validateWebData_expectedColumnNames("CIMIS", siPRISM = TRUE)
  
  
  # Rename some columns in 'cimisDF'
  cimisDF <- cimisDF |>
    rename(all_of(fieldNameVec))
  
  
  # For convenience, filter 'cimisInput' as well
  # Keep only rows with values in "[MODEL]_PRECIP_NAME"
  # (These stations' precipitation values will be used in the model)
  cimisInput <- cimisInput |>
    filter(!is.na(get(paste0(model, "_PRECIP_NAME"))))
  
  
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
                             "(This error occurred for \"", cimisOutputPath, "\""))
  
  
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
  
  
  # Use 'cimisInput' to append precipitation information to 'flaggedDF'
  # "PRMS_PRECIP_NAME" will identify the actual PRMS column names (as it appears
  # in 'meteorDF') that correspond to CIMIS precipitation data
  flaggedDF <- flaggedDF |>
    left_join(cimisInput |> select(STATION_ID, all_of(paste0(model, "_PRECIP_NAME"))),
              by = "STATION_ID", relationship = "many-to-one")
  
  
  # Iterate through each of the stations in 'cimisInput'
  for (i in 1:nrow(cimisInput)) {
    
    # Get the flagged dates associated with this specific CIMIS station
    errDates <- flaggedDF$DATE[flaggedDF$STATION_ID == cimisInput$STATION_ID[i]]
    
    
    # If 'errDates' is empty, skip to the next precipitation station
    if (length(errDates) == 0) {
      next
    }
    
    
    # Otherwise, update entries in 'meteorDF' for this CIMIS station column
    # Flagged dates should have their precipitation values set to NA
    meteorDF[[cimisInput[[paste0(model, "_PRECIP_NAME")]][i]]][meteorDF$DATE %in% errDates] <- NA_real_
    
  }
  
  
  # Return 'meteorDF' afterwards
  return(meteorDF)
  
}



apply_qc_flags_CDEC <- function (meteorDF, cdecInput, cdecDF, cdecOutputPath,
                                 model) {
  
  # The raw CDEC data in 'cdecDF' has QA/QC flags as columns in the data
  # https://cdec.water.ca.gov/reportapp/javareports?name=FlagList
  
  # No corrections have been made, but these flags are present so that
  # users can make corrections at their discretion
  
  # This function will remove records that have certain precipitation flags
  
  
  # Though, if 'cdecDF' is NULL, that means that there is no CIMIS data
  # used by this model
  if (is.null(cdecDF)) {
    
    # Return 'meteorDF' without any changes in that case
    return(meteorDF)
    
  }
  
  
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
    filter(!is.na(get(paste0(model, "_PRECIP_NAME"))))
  
  
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
                             "(This error occurred for \"", cdecOutputPath, "\""))
  
  
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
  
  
  # Use 'cdecInput' to append model precipitation information to 'flaggedDF'
  # "[MODEL]_PRECIP_NAME" will identify the actual station column names (as it appears
  # in 'meteorDF') that correspond to CDEC precipitation data
  flaggedDF <- flaggedDF |>
    left_join(cdecInput |> select(STATION_ID, all_of(paste0(model, "_PRECIP_NAME"))),
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
    meteorDF[[cdecInput[[paste0(model, "_PRECIP_NAME")]][i]]][meteorDF$DATE %in% errDates] <- NA_real_
    
  }
  
  
  # Return 'meteorDF' afterwards
  return(meteorDF)
  
}



remove_outliers <- function (meteorDF, outlierDF) {
  
  # Given upper-limit bounds for each model precipitation gage, 
  # remove outliers from their datasets
  
  
  # If 'outlierDF' is NULL, do not apply this procedure
  if (is.null(outlierDF)) {
    return(meteorDF)
  }
  
  
  # Otherwise, get a vector of precipitation columns that appear in 'meteorDF'
  # (Exclude supplemental and extra gages from this)
  precipNames <- names(meteorDF) |>
    str_subset("^PRECIP[0-9]+")
  
  
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



sub_missing_gage_data <- function (meteorDF, corrDF, prismProcessed) {
  
  # Fill in missing data gaps in 'meteorDF'
  
  # Rely on regression models that link gages to other gages in the watershed
  
  # Models that fill in missing data based on PRISM may also be used
  # ('prismProcessed' contains correpsonding data for each gage)
  
  
  # If 'corrDF' is NULL, do not apply this procedure
  if (is.null(corrDF)) {
    return(meteorDF)
  }
  
  
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



sub_data_with_PRISM <- function (meteorDF, prismProcessed, allTempSub) {
  
  # Wherever missing values are present in 'meteorDF', use PRISM data as a substitute
  
  # And if 'allTempSub' is TRUE, all temperature data will come from PRISM
  
  
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



archive_climate_files <- function (startDate, endDate, model, 
                                   noQCPath, intermediatePath, meteorPath, 
                                   prismInputPath, prismOutputPath, 
                                   noaaInputPath, noaaOutputPath,
                                   rawsInputPath, rawsOutputPath,
                                   cimisInputPath, cimisOutputPath,
                                   cdecInputPath, cdecOutputPat,
                                   precipOutliersPath, precipCorrPath) {
  
  # In a previous script, a folder for archiving model files was established
  
  # This function will add climate files to this folder 
  # (if they are used by the process and not NULL)
  
  # The folder's metadata CSV file will be updated too 
  
  
  # Start by locating the archive folder
  # It can be found using 'startDate' and 'endDate'
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Next, save copies of the climate files
  
  # If they are not NULL, save each of the files whose paths were input 
  # into this function
  copy_file_to_archive(noQCPath, dirPath, model)
  copy_file_to_archive(intermediatePath, dirPath, model)
  copy_file_to_archive(meteorPath, dirPath, model)
  
  copy_file_to_archive(prismInputPath, dirPath, model)
  copy_file_to_archive(prismOutputPath, dirPath, model)
  
  copy_file_to_archive(noaaInputPath, dirPath, model)
  copy_file_to_archive(noaaOutputPath, dirPath, model)
  
  copy_file_to_archive(rawsInputPath, dirPath, model)
  copy_file_to_archive(rawsOutputPath, dirPath, model)
  
  copy_file_to_archive(cimisInputPath, dirPath, model)
  copy_file_to_archive(cimisOutputPath, dirPath, model)
  
  copy_file_to_archive(cdecInputPath, dirPath, model)
  copy_file_to_archive(cdecOutputPat, dirPath, model)
  
  copy_file_to_archive(precipOutliersPath, dirPath, model)
  copy_file_to_archive(precipCorrPath, dirPath, model)
  
  
  # Finally, update the folder's metadata CSV with 
  # information about the meteorological file
  list(meteorPath |> getFile() |> get_model_revision(model),
       file.info(meteorPath)[["ctime"]]) |>
    set_names(c(paste0(model, "_MODEL_REVISION"),
                paste0(model, "_METEOROLOGICAL_FILE_CREATED"))) |>
    updateMetadataCSV(dirPath = dirPath)
  
  # Note 1: Functions cannot be used as names in a list, so `set_names` was called
  #         after defining the list to name the elements
  #         If any future changes are made to this function, please ensure that
  #         the ordering of the values matches the ordering of the name assignments
  
  # Note 2: If 'meteorDF' was included as a function argument, 'meteorPath' and 
  #         `getFile` would not be needed to run `get_model_revision`
  #         However, doing it this way also serves as a test to ensure that the 
  #         file was written correctly without any issues
  
  
  # Return nothing
  return(invisible(NULL))
  
}



get_model_revision <- function (meteorDF, model) {
  
  # Get the revision number of the model being used
  # for a meteorological dataset
  
  # The column names of the precipitation and temperature stations may
  # contain a "_REV#" string at the end (e.g., "PRECIP17_REV2")
  
  # Use that to fill out the metadata field about the model revision number
  
  
  # Extract revision information from the precipitation and temperature columns
  revList <- meteorDF |>
    select(matches("^((PRECIP)|(TMAX)|(TMIN))")) |>
    names() |> 
    extractRevisionInfo()
  
  # `extractRevisionInfo` will extract revision strings from the names of 
  # each column and produce a list that has three vectors: 
  #   (1) The names without the revision string
  #   (2) The extracted revision strings
  #   (3) The actual numbers in the revision strings
  
  
  # Make sure only one type of revision is listed in 'revList'
  if (length(unique(revList[[3]])) != 1) {
    
    paste0("Multiple Revisions in Meteorological File\n\n",
           "Different revisions correspond to different configurations of ",
           model, ". A meteorological CSV should only have one revision string ",
           "that appears in all precipitation and temperature columns' names. ",
           "However, ", vec2QuotedStr(unique(revList[[3]])), " were detected. ",
           "Please investigate the cause.\n\n",
           "(This error occurred for '", meteorPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Get the revision string
  revStr <- revList[[2]] |>
    unique()
  
  
  # If 'revStr' is NA, that corresponds to the first revision of these files
  # (i.e., when the stations used names like "PRECIP1" and "TMIN8")
  if (is.na(revStr)) {
    revStr <- "_REV1"
  }
  
  
  # Finally, remove the starting underscore from 'revStr' and return it
  return(revStr |>
           str_remove("^_"))
  
}

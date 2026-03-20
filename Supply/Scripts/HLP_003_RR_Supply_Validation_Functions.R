# Several data validation functions would be useful at multiple steps in 
# the PRMS and SRP processes

# This script contains functions that will be used by multiple scripts


#### Dependencies ####

# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")


#### Functions ####

validateStationInputs <- function (inputDF, inputPath, 
                                   model = "PRMS", numPrecipFields = 15,
                                   numTempFields = 8) {
  
  # This function is used when producing meteorological datasets
  # (which will be integrated into a long-running DAT file)
  
  # The station input files for PRISM, NOAA, RAWS, and CIMIS were previously 
  # validated in their respective web scraping scripts
  
  # However, this script has additional requirements
  
  # This function checks specifically for the fields and formatting required 
  # in the PRMS and SRP procedures
  
  
  # Related to that requirement, make sure 'model' is either "PRMS" or "SRP"
  if (!(model %in% c("PRMS", "SRP"))) {
    
    paste0("Script Error - Unrecognized Value for 'model'\n\n", 
           "The function `validateStationInputs` checks station input ",
           "for connecting weather data to PRMS or SRP DAT files. ",
           "Therefore, the input variable 'model' should be either \"PRMS\" ",
           "or \"SRP\". However, it was input as \"", model, "\" instead.\n\n", 
           "Please correct the script and try again.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, 'numPrecipFields' and 'numTempFields' dictate the acceptable range of 
  # values for the station input file's PRMS/SRP-related fields
  
  
  # With the default values set above, the PRMS precipitation field can have 
  # values between "PRECIP1" and "PRECIP15" (inclusive)
  # Similarly, the maximum and minimum temperature fields can have values between 
  # "TMAX1"/"TMIN1" and "TMAX8"/"TMIN8" (inclusive)
  
  
  # For this script's procedure to succeed, all input files must have these four columns:
  #    (*) STATION_ID
  #    (*) PRMS_PRECIP_NAME / SRP_PRECIP_NAME
  #    (*) PRMS_TMIN_NAME / SRP_TMIN_NAME
  #    (*) PRMS_TMAX_NAME / SRP_TMAX_NAME
  inputFieldNames <- c("STATION_ID", 
                       paste0(model, "_PRECIP_NAME"),
                       paste0(model, "_TMIN_NAME"),
                       paste0(model, "_TMAX_NAME"))
  
  
  # Start by confirming that the field names appear in 'inputDF'
  if (anyFalse(inputFieldNames %in% names(inputDF))) {
    
    # Identify which fields are missing
    missingFields <- which(!(inputFieldNames %in% names(inputDF)))
    
    
    # Output an error message
    paste0("Station Input File - Missing Column Issue\n\n", 
           "For this script to work, the ",
           if_else(model == "PRMS", 
                   "PRISM, NOAA, RAWS, and CIMIS input files ",
                   "PRISM input file "), "must contain ", 
           length(inputFieldNames), " key column",
           if_else(length(inputFieldNames) > 1, "s", ""), " (",
           vec2QuotedStr(inputFieldNames), ")\n\n",
           "However, the \"", names(inputPath), "\" file is missing ",
           if_else(length(missingFields) > 1, "fields", "a field"), ":\n\n",
           paste0("(*) ", inputFieldNames[missingFields], collapse = "\n\n"), 
           "\n\n",
           "Please revise the input file (\"", inputPath, "\") accordingly") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The station ID was previously validated in the scraping scripts
  # The next focus will be the "PRMS"/"SRP" fields
  
  
  # In the PRMS DAT file, there are 15 precipitation fields and 8 max/min
  # temperature fields
  
  # In the SRP DAT file, there are 2 precipitation fields and 2 max/min
  # temperature fields
  
  # The values that appear in the PRMS/SRP fields should be one of 
  # these column names (or NA)
  
  
  # Start with the precipitation fields
  # For PRMS, the values should be "NA", or something between "PRECIP1" and 
  # "PRECIP15" (inclusive)--for SRP, it's up to "PRECIP2" only
  if (anyFalse(inputDF[[inputFieldNames[2]]] %in% c(NA, paste0("PRECIP", 1:numPrecipFields)))) {
    
    paste0("Station Input File - Invalid ", model, " Value Issue\n\n", 
           "The \"", names(inputPath), "\" file contains an invalid value ",
           "for the field \"", inputFieldNames[2], "\" \n\n",
           "Each row should either be blank, or it should contain a text ",
           "string like \"PRECIP1\" (up to \"PRECIP", numPrecipFields, 
           "\")\n\n", 
           "Please revise the input file (\"", inputPath, 
           "\") accordingly") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Use a similar check for the minimum temperature field next
  # For PRMS, the values should be "NA", or something between 
  # "TMIN1" and "TMIN8" (inclusive)--for SRP, it's up to "TMIN2"s
  if (anyFalse(inputDF[[inputFieldNames[3]]] %in% c(NA, paste0("TMIN", 1:numTempFields)))) {
    
    paste0("Station Input File - Invalid ", model, " Value Issue\n\n", 
           "The \"", names(inputPath), "\" file contains an invalid value ",
           "for the field \"", inputFieldNames[3], "\" \n\n",
           "Each row should either be blank, or it should contain a text ",
           "string like \"TMIN1\" (up to \"TMIN", numTempFields, "\")\n\n", 
           "Please revise the input file (\"", inputPath, 
           "\") accordingly") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Repeat the check for the "TMAX" field
  # The values should be "NA", or something between "TMAX1" and "TMAX8" 
  # (inclusive) for PRMS--for SRP, it's up to "TMAX2"
  if (anyFalse(inputDF[[inputFieldNames[4]]] %in% c(NA, paste0("TMAX", 1:numTempFields)))) {
    
    paste0("Station Input File - Invalid ", model, " Value Issue\n\n", 
           "The \"", names(inputPath), "\" file contains an invalid value for ",
           "the field \"", inputFieldNames[4], "\" \n\n",
           "Each row should either be blank, or it should contain a text string ",
           "like \"TMAX1\" (up to \"TMAX", numTempFields, "\")\n\n", 
           "Please revise the input file (\"", inputPath, "\") accordingly") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, confirm that every row has at least one non-NA value for the 
  # three PRMS/SRP fields
  # Every station should have a corresponding PRMS field
  # So at least one column between "PRECIP", "TMIN", and "TMAX" should have a 
  # non-NA value in each row
  
  # Define a temporary variable to help with this
  # If all three columns contain "NA", this column's value will be TRUE
  inputDF <- inputDF |>
    mutate(ALL_NA = is.na(get(inputFieldNames[2])) & 
             is.na(get(inputFieldNames[3])) &
             is.na(get(inputFieldNames[4])))
  
  
  # If TRUE appears for any row in "ALL_NA", output an error message
  if (TRUE %in% inputDF$ALL_NA) {
    
    paste0("Station Input File - Invalid ", model, " Value Issue\n\n", 
           "The \"", names(inputPath), "\" file contains a station without ",
           "a corresponding ", model, " field identified\n\n",
           "Across the ", length(inputFieldNames) - 1, " ", model, 
           " columns, each row should contain a ", model, 
           " field name in at least one column\n\n",
           "Please revise the input file (\"", inputPath, "\") accordingly") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The final check is to ensure that "TMIN" and "TMAX" have corresponding 
  # values in the same row
  # If the "TMIN" value is "NA", it should be "NA" for "TMAX" too
  # Similarly, if "TMIN" has a value, "TMAX" should have an equivalent value
  # (The numbers in both labels should be the same)
  inputDF <- inputDF |>
    mutate(TEMP_MISMATCH = 
             (is.na(get(inputFieldNames[3])) & !is.na(get(inputFieldNames[4]))) |
             (!is.na(get(inputFieldNames[3])) & is.na(get(inputFieldNames[4]))) |
             (!is.na(get(inputFieldNames[3])) & !is.na(get(inputFieldNames[4])) &
                as.numeric(str_extract(get(inputFieldNames[3]), "[0-9]+$")) != 
                as.numeric(str_extract(get(inputFieldNames[4]), "[0-9]+$"))))
  
  # There are three different "mismatch" conditions described in the above code
  # (1) The "TMIN" field is "NA", but the "TMAX" field is NOT "NA"
  # (2) The "TMAX" field is not "NA", but the "TMAX" field IS "NA"
  # (3) Both "TMIN" and "TMAX" do not contain "NA", but the numbers at the end
  #     of their values do not match (e.g., "TMIN7" and "TMAX8")
  if (TRUE %in% inputDF$TEMP_MISMATCH) {
    
    paste0("Station Input File - Invalid ", model, " Value Issue\n\n", 
           "The \"", names(inputPath), "\" file contains ", 
           sum(inputDF$TEMP_MISMATCH),
           " instance", if_else(sum(inputDF$TEMP_MISMATCH) > 1, "s", ""), " ",
           "where \"", inputFieldNames[3], "\" and \"", inputFieldNames[4], 
           "\" do not contain matching values\n\n",
           "Either both ", model, " temperature columns should be empty, or ",
           "they should have corresponding values (e.g., \"TMIN3\" and ",
           "\"TMAX3\"  in the same row)\n\n",
           "Please revise the input file (\"", inputPath, "\") accordingly") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



validateWebData <- function (climateDF, inputPath, stationVec, siPRISM = TRUE) {
  
  # Check for errors in the downloaded web data
  
  # This function mainly checks for expected column names and "NA" values
  
  
  # First, extract the data source name from the element name for 'inputPath'
  dataSource <- names(inputPath) |> str_extract("^[A-Z]+")
  
  
  # Make sure that procedure was successful
  if (!(dataSource %in% c("PRISM", "NOAA", "CIMIS", "RAWS"))) {
    
    paste0("Unexpected Data Source\n\n", 
           "The name \"", dataSource, "\" is not recognized; ",
           "please fix the script\n\n",
           "The function `validateWebData()` uses the vector names ",
           "in 'inputFiles' and extracts the data source name. It ",
           "expects \"PRISM\", \"NOAA\", \"RAWS\", or \"CIMIS\" as ",
           "acceptable values.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # After that, get a vector of the expected column names for this dataset
  colVec <- validateWebData_expectedColumnNames(dataSource, siPRISM = siPRISM)
  
  
  # Confirm that all of these column names appear in 'climateDF'
  if (anyFalse(colVec %in% names(climateDF))) {
    
    # Identify which columns are missing
    missingVals <- which(!(colVec %in% names(climateDF)))
    
    
    paste0("Web Data Output File - Formatting Issue\n\n",
           if_else(length(colVec) > 1,
                   paste0(length(missingVals), " of the ", 
                          length(colVec), " expected columns"),
                   "The expected column "),
           " could not be found in the \"", names(inputPath), "\" file (",
           vec2QuotedStr(colVec[missingVals]),
           ")\n\n",
           "The formatting of the data may have changed (this would require ",
           "revisions to the script). ",
           "Alternatively, there may be an issue with the downloaded file.\n\n",
           "Please investigate \"", inputPath, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # For ease of investigating 'climateDF' further, apply the column name updates
  # using the element names in 'colVec'
  # Since the revised names are the same in all cases ("STATION_ID", "DATE", etc.),
  # the code is simpler to write
  climateDF <- climateDF |> rename(all_of(colVec))
  
  
  # After that, confirm that every station that appears in 'climateDF' 
  # has a corresponding entry in the input list of stations ('stationVec')
  if (anyFalse(unique(climateDF$STATION_ID) %in% stationVec)) {
    
    # Identify the unexpected stations
    extraStations <- which(!(unique(climateDF$STATION_ID) %in% stationVec))
    
    
    paste0("Web Data Output File - Unrecognized Station(s)\n\n",
           "The \"", names(inputPath), "\" file has one or more stations ",
           "that do not appear in its corresponding input file (",
           vec2QuotedStr(unique(climateDF$STATION_ID)[extraStations]), 
           ")\n\n", 
           "Please investigate \"", inputPath, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Note: The reverse is not required because some stations may lack data 
  #       for the user-specified date range and be missing from the output
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateWebData_expectedColumnNames <- function (dataSource, siPRISM = TRUE) {
  
  # Different websites return climate data in different formats
  # As a result, the expected column names will differ in formatting
  
  # To make it easier to address changes to column names in the future, 
  # this function has the "hard-coded" column names for each data source
  
  # Scripts that call the `validateWebData` function will use this function 
  # to get this information
  
  
  # This function returns a named vector
  # The element names are the desired column names
  # The actual elements themselves are the names that appear in the 
  # weather data files
  
  # Note: In all cases, the expected revised column names are "STATION_ID", 
  # "DATE", "PRECIP", "TMIN", and "TMAX" (i.e., these should all appear 
  # as the element names)
  
  
  # 'siPRISM' is just a Boolean variable that tells whether the PRISM data is 
  # given in SI units or US Customary units (this affects the column names)
  
  
  if (dataSource == "PRISM") {
    
    nameVec <- c("STATION_ID" = "Name",
                 "DATE" = "Date",
                 "PRECIP" = if_else(siPRISM, 
                                    "ppt (mm)", "ppt (inches)"),
                 "TMIN" = if_else(siPRISM, 
                                  "tmin (degrees C)", "tmin (degrees F)"),
                 "TMAX" = if_else(siPRISM, 
                                  "tmax (degrees C)", "tmax (degrees F)"))
    
  } else if (dataSource == "NOAA") {
    
    nameVec <- c("STATION_ID" = "STATION",
                 "DATE" = "DATE",
                 "PRECIP" = "PRCP",
                 "TMIN" = "TMIN",
                 "TMAX" = "TMAX")
    
  } else if (dataSource == "RAWS") {
    
    nameVec <- c("STATION_ID" = "STATION_ID",
                 "DATE" = "DATE",
                 "PRECIP" = "PRECIPITATION",
                 "TMIN" = "TMIN",
                 "TMAX" = "TMAX")
    
  } else if (dataSource == "CIMIS") {
    
    nameVec <- c("STATION_ID" = "STATION_ID",
                 "DATE" = "DATE",
                 "PRECIP" = "PRECIP",
                 "TMIN" = "TMIN",
                 "TMAX" = "TMAX")
    
  } else {
    
    # An error message will appear for any unrecognized input
    paste0("Misuse of `expectedColumnNames()`\n\n", 
           "The input \"", dataSource, "\" is not recognized; ",
           "please fix the script\n\n",
           "The function `expectedColumnNames()` requires a data ",
           "source's name as input (either \"PRISM\", \"NOAA\", ",
           "\"RAWS\", or \"CIMIS\")\n\n") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Check that the developer coded this vector correctly
  # (All vectors should have the same length and the same replacement names)
  if (length(nameVec) != 5 ||
      anyFalse(c("STATION_ID", "DATE", "PRECIP", "TMIN", "TMAX") %in% 
               names(nameVec))) {
    
    paste0("Issue in `expectedColumnNames()`\n\n", 
           "The name vector for ", dataSource, " may contain an issue\n\n",
           "Regardless of source, 5 specific columns are expected (",
           vec2QuotedStr(c("STATION_ID", "DATE", "PRECIP", "TMIN", "TMAX")),
           ")\n\n",
           "The name vector should contain the corresponding raw data names ",
           "(and link them to one of these columns)") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there are no issues, return 'nameVec'
  return(nameVec)
  
}



checkForPreviousOutput <- function (filePath) {
  
  # Check for a file that was generated at a prior step in the workflow
  
  
  if (!file.exists(filePath)) {
    
    paste0("File From Previous Script Not Found\n\n",
           "The file \"", filePath, "\" should have been generated by a ",
           "preceding script in this process. However, it was not found. ",
           "Please run the previous scripts before running this one.\n\n") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return the normalized filepath if there are no issues
  return(filePath |> normalizePath(mustWork = TRUE))
  
}



validateHydroFolder <- function (startDate, endDate) {
  
  # Verify that previous scripts were run successfully and that a hydrology 
  # folder was created to store metadata and model files
  
  # This function can also return the directory path
  
  
  # Get the text file that contains the path to the hydrology folder
  folderFilePath <- paste0("ProcessedData/Hydrology_Output_Location_",
                           startDate, "_", endDate, ".txt")
  
  
  # Output an error message if the file was not found
  if (!file.exists(folderFilePath)) {
    
    paste0("Model Run Folder Not Found\n\n",
           "A folder to store metadata and model inputs/outputs should ",
           "have been generated by an earlier script in this process. ",
           "An accompanying output was a text file that identifies its ",
           "location. However, it was not found. ",
           "Please run the previous scripts before running this one.\n\n",
           "The expected file was \"", folderFilePath, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Read in the text file to get the path to the actual folder
  dirPath <- read_lines(folderFilePath)[1]
  
  
  # Make sure that folder exists 
  if (!dir.exists(dirPath)) {
    
    paste0("Model Run Folder Not Found\n\n",
           "A folder to store metadata and model inputs/outputs should ",
           "have been generated by an earlier script in this process. ",
           "However, it was not found. ",
           "Please run the previous scripts before running this one.\n\n",
           "The expected directory was \"", dirPath, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Confirm that the metadata CSV file exists too
  metaPath <- paste0(dirPath, "/metadata.csv") |>
    normalizePath(mustWork = FALSE)
  
  
  if (!file.exists(metaPath)) {
    
    stop(paste0("Missing Metadata File\n\n",
                "When the hydrology folder was setup, a metadata CSV file ", 
                "should have been generated. However, it was not found. ",
                "Please investigate.\n\n",
                "(This error occurred for \"", metaPath, "\")") |>
           errWrap())
    
  }
  
  
  # If there are no issues, return 'dirPath'
  return(dirPath)
  
}



validateModelCopy_PRMS <- function () {
  
  # In a prior script, PRMS model files were copied to the "ProcessedData" folder
  # Verify that it exists
  
  # This function also returns the path to the model folder
  
  
  # The expected path of the "RR_PRMS" folder
  prmsPath <- "ProcessedData/RR_PRMS" |> normalizePath(mustWork = FALSE)
  
  
  # Make sure that that folder exists 
  if (!dir.exists(prmsPath)) {
    
    paste0("PRMS Folder Not Found\n\n",
           "A copy of the PRMS model files should have been added ",
           "to the \"ProcessedData\" folder in an earlier script. ",
           "However, it was not found. ",
           "Please run the previous scripts before running this one.\n\n",
           "The expected directory was \"", prmsPath, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Also confirm that the control file for PRMS exists
  controlPath <- paste0(prmsPath, "/windows/prms_rr.control") |> 
    normalizePath(mustWork = FALSE)
  
  
  if (!file.exists(controlPath)) {
    
    paste0("Missing PRMS Control File\n\n",
           "When the PRMS folder was copied into the \"ProcessedData\" ", 
           "folder, a control file was present in the \"windows\" folder. ",
           "However, it cannot be found now. Please investigate.\n\n",
           "(This error occurred for \"", controlPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # A batch file should be present in the model files too
  # Check for that as well
  batPath <- paste0(prmsPath, "/windows/run.bat") |>
    normalizePath(mustWork = FALSE)
  
  
  if (!file.exists(batPath)) {
    
    paste0("Missing PRMS Batch File\n\n",
           "When the PRMS folder was copied into the \"ProcessedData\" ", 
           "folder, a batch file was present among the model files. ", 
           "However, it cannot be found now. Please investigate.\n\n",
           "(This error occurred for \"", batPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return 'prmsPath' if there are no issues
  return(prmsPath)
  
}



checkForModelOutputs_PRMS <- function (prmsPath, modelOutput = NULL,
                                       includeScriptGeneratedOutput = FALSE) {
  
  # Double-check that the model ran successfully
  
  # There should be several key files in the "output" folder
  
  
  # These files were all generated by PRMS
  outFiles <- c("gsflow.csv", 
                "rr_budget.out2",
                "RR_PRMS_Output_sub_cfs.csv",
                "RR_PRMS_Output_sub_inq.csv")
  
  
  # If 'includeScriptGeneratedOutput' is TRUE, include the console output 
  # text file generated by an earlier script in this check
  if (includeScriptGeneratedOutput) {
    
    outFiles <- c(outFiles,
                  "PRMS_Console_Output.txt")
    
  }
  
  
  # Check if any files are missing
  missingFiles <- which(!file.exists(outFiles |>
                                       paste0(prmsPath, "/PRMS/output/", 
                                              ... = _) |>
                                       normalizePath(mustWork = FALSE)))
  
  
  if (length(missingFiles) > 0) {
    
    # Include the model run outputs in the console if 'modelOutput' is not NULL
    if (!is.null(modelOutput)) {
      
      cat("\n\nModel Output Message(s):\n\n")
      print(modelOutput)
      
    }
    
    
    paste0("Missing PRMS Output File", 
           if_else(length(missingFiles) > 1, "s", ""), "\n\n",
           "The PRMS model run did not generate all of the expected ",
           "files (missing ", vec2QuotedStr(outFiles[missingFiles]),
           "). Please investigate ",
           if_else(is.null(modelOutput),
                   "the model's output messages (included above)",
                   "this issue"),
           ".\n\n", 
           "(This error occurred for \"", prmsPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}


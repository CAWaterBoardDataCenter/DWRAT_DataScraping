# Several data validation functions would be useful at multiple steps in 
# the PRMS and SRP processes

# This script contains functions that will be used by multiple scripts


#### Dependencies ####

# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")


#### Functions ####

validateStationInputFile <- function (stationDF, sourceField, dataSource) {
  
  # This function is used for initial validation of an input file that
  # identifies stations and/or locations where climate data will be downloaded
  
  # Make sure that 'stationDF' is formatted correctly
  # If there are any issues, notify the user
  
  
  # This function is intended to be used for the "PRISM", "NOAA", "CIMIS",
  # and "RAWS" station input files
  if (!(dataSource %in% c("PRISM", "NOAA", "CIMIS", "RAWS"))) {
    
    paste0("Unexpected Data Source\n\n", 
           "The name \"", dataSource, "\" is not recognized; ",
           "please fix the script\n\n",
           "The function `validateStationInputFile()` expects \"PRISM\", ",
           "\"NOAA\", \"RAWS\", or \"CIMIS\" as acceptable values.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The expected columns differ depending on 'dataSource'
  # Most sources only require a "STATION_ID" column
  if (dataSource %in% c("PRISM")) {
    
    expectedCols <- c("LATITUDE", "LONGITUDE", "STATION_ID")
    
  } else if (dataSource %in% c("NOAA", "RAWS", "CIMIS")) {
    
    expectedCols <- c("STATION_ID")
    
  }
  
  
  # Check for missing columns
  missingColumns <- which(!(expectedCols %in% names(stationDF)))
  
  
  # 'stationDF' should contain all expected columns
  if (length(missingColumns) > 0) {
    
    # There are slightly different error messages depending on the data source
    paste0("Station Input File - Column Issue\n\n",
           "The input file containing ",
           list("PRISM" = "PRISM target coordinates ",
                "NOAA" = "GHCND stations ",
                "RAWS" = "RAWS stations ",
                "CIMIS" = "CIMIS stations ")[[dataSource]], 
           "does not have ", 
           if_else(length(expectedCols) == 1, 
                   " the required column ", "all required columns "),
           "(", vec2QuotedStr(expectedCols), "). Please correct this file ",
           "and try again.\n\n",
           list("PRISM" = paste0("The input file must contain the WGS84 ",
                                 "coordinates and unique identifiers for ",
                                 "each location."),
                "NOAA" = paste0("The input file must contain the GHCND IDs ",
                                "(e.g., 'USC00043875') for each target ",
                                "location."),
                "RAWS" = paste0("The input file must contain the IDs that ",
                                "appear in RAWS's URLs for each target ",
                                "location (e.g., 'CHAW' for 'Hawkeye')."),
                "CIMIS" = paste0("The input file must contain the numeric ",
                                 "IDs that correspond to different CIMIS ",
                                 "stations (e.g., '103' for ",
                                 "'Windsor')."))[[dataSource]], " ",  
           "Also, the names of these columns must match exactly.\n\n",
           "(This error occurred for '", getFromSupplyControl_RR(sourceField),
           "')") |>
      errWrap() |>
      str_replace("(does not)", col_red("\\1")) |>
      str_replace("(exactly)", col_red("\\1")) |>
      stop()
    
  }
  
  
  # Make sure there are no missing entries in the required columns
  if (anyNA(stationDF[expectedCols])) {
    
    paste0("Station Input File - Missing Data Issue\n\n",
           "The input file containing target ", dataSource, " ",
           if_else(dataSource == "PRISM", "coordinates", "stations"), " ",
           "has one or more missing elements in its required column",
           if_else(length(expectedCols) > 1, "s", ""), " (",
           vec2QuotedStr(expectedCols), ")\n\n", 
           "Please fill in any empty entries in ",
           if_else(length(expectedCols) > 1, "these columns", "this column"),
           "\n\n",
           "(This error occurred for '", getFromSupplyControl_RR(sourceField), 
           "')") |>
      errWrap() |>
      str_replace("(missing)", col_red("\\1")) |>
      stop()
    
  }
  
  
  # Ensure that no IDs are duplicated in 'stationDF'
  if (length(stationDF$STATION_ID) != length(unique(stationDF$STATION_ID))) {
    
    paste0("Station Input File - Duplicate ID Issue\n\n",
           "The input file containing target ", dataSource, " ",
           if_else(dataSource == "PRISM", "coordinates", "stations"), " ",
           "has one or more values in its \"STATION_ID\" column that are ",
           "duplicated.\n\n", 
           "Please ensure that each row of the input file has a unique ",
           "value for this column\n\n",
           "(This error occurred for '", getFromSupplyControl_RR(sourceField), 
           "')") |>
      errWrap() |>
      str_replace("(duplicated)", col_red("\\1")) |>
      stop()
    
  }
  
  
  # Finally, check the types of different columns
  
  
  # If "LATITUDE" and "LONGITUDE" are present in 'expectedCols',
  # check if those columns are both numeric 
  if (all(c("LATITUDE", "LONGITUDE") %in% expectedCols)) {
    
    if (is.character(stationDF$LATITUDE) || is.character(stationDF$LONGITUDE)) {
      
      paste0("Station Input File - Coordinates Type Issue\n\n",
             "The \"LATITUDE\" and/or \"LONGITUDE\" columns of the input ",
             "file are being read in as character columns instead of ",
             "numeric columns\n\n", 
             "Since types are assigned automatically, this indicates ",
             "that the columns cannot be parsed as numeric columns due ",
             "to the presence of non-number-related characters (or the ",
             "absence of any values at all)\n\n",
             "Please correct these columns and ensure that they are ",
             "numeric values\n\n",
             "(This error occurred for '", 
             getFromSupplyControl_RR(sourceField), "')") |>
        errWrap() |>
        str_replace("(character)", col_red("\\1")) |>
        stop()
      
    } else if (!is.numeric(stationDF$LATITUDE) || !is.numeric(stationDF$LONGITUDE)) {
      
      paste0("Station Input File - Coordinates Type Issue\n\n",
             "The \"LATITUDE\" and/or \"LONGITUDE\" columns of the input ",
             "file are being read in as a different type of column ",
             "instead of numeric\n\n", 
             "Since types are assigned automatically, this indicates that ",
             "the columns cannot be parsed as numeric columns for some ",
             "reason, such as being empty\n\n",
             "Please correct these columns and ensure that they are ",
             "numeric values\n\n",
             "(This error occurred for '", 
             getFromSupplyControl_RR(sourceField), "')") |>
        errWrap() |>
        str_replace("(empty)", col_red("\\1")) |>
        stop()
      
    }
    
  }
  
  
  # For CIMIS, the station IDs must be numeric
  if (dataSource == "CIMIS") {
    
    if (!is.numeric(stationDF$STATION_ID)) {
      
      paste0("Station Input File - ID Type Issue\n\n",
             "The \"STATION_ID\" column of the input file is being read in ",
             "as something other than a numeric column\n\n", 
             "Since types are assigned automatically, this indicates that the ",
             "column cannot be parsed as a numeric column due to the presence ",
             "of non-number-related characters (or the absence of any value ",
             "at all)\n\n",
             "Please correct this column and ensure that it contains only ",
             "numeric values\n\n",
             "(This error occurred for '", getFromSupplyControl_RR(sourceField), 
             "')") |>
        errWrap() |>
        str_replace("(missing)", col_red("\\1")) |>
        stop()
      
    }
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}


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



validateInputDAT <- function (datFile, sourceField, model, modelCols,
                              startDate, endDate, datType) {
  
  # Verify the formatting of a DAT file for PRMS or SRP
  # This function will be applied to the long-running meteorological
  # DAT file, the SPI DAT file, and the final DAT file
  
  # DAT files run through this function gain a "DATE" column
  
  # 'model' and 'modelCols' are specific to either PRMS or SRP
  
  # 'datType' will be used if certain checks are specific to 
  # the main DAT file, the SPI DAT file, or the final DAT file
  
  
  if (!(model %in% c("PRMS", "SRP"))) {
    
    paste0("Script Error - Unknown Value for 'model'\n\n",
           "The input variable 'model' must be one of two ",
           "values (", vec2QuotedStr(c("PRMS", "SRP")), 
           "). \"", model, "\" is not a recognized value. ",
           "Please revise the script.") |>
      errWrap() |>
      stop()
    
  }
  
  
  if (!(datType %in% c("Main", "SPI", "Final"))) {
    
    paste0("Script Error - Unknown Value for 'datType'\n\n",
           "The input variable 'datType' must be one of three ",
           "values (", 
           vec2QuotedStr(c("Main", "SPI", "Final")), "). \"",
           datType, "\" is not a recognized value. Please revise ",
           "the script.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # First, check that the date- and time-related fields are present
  datetimeCols <- c("YEAR", "MONTH", "DAY", "HOUR", "MINUTE", "SECOND")
  
  
  if (anyFalse(datetimeCols %in% names(datFile))) {
    
    # Identify the missing columns
    missingCols <- which(!(datetimeCols %in% names(datFile)))
    
    
    paste0("DAT File - Column Issue\n\n",
           "The ", datType, " input file for ", model, " does not have ",
           "all of the required datetime columns (",
           vec2QuotedStr(datetimeCols[missingCols]), ").\n\n",
           "The DAT file must contain headers that match the names in ",
           "the meteorological CSV (\"", model, "_Meteorological_",  
           startDate, "_", endDate, ".csv\"). Please correct this file ",
           "and try again.\n\n", 
           if_else(datType != "Final",
                   paste0("(This error occurred for '", 
                          getFromSupplyControl_RR(sourceField), "')"),
                   paste0("Please investigate the component DAT files."))) |>
      errWrap() |>
      str_replace("(does not)", col_red("\\1")) |>
      stop()
    
  }
  
  
  # After that, perform a similar check for the model-specific columns
  if (anyFalse(modelCols %in% names(datFile))) {
    
    # Identify the missing columns
    missingCols <- which(!(modelCols %in% names(datFile)))
    
    
    paste0("DAT File - Column Issue\n\n",
           "The ", datType, " input file for ", model, " does not have ",
           "all of the required ", model, " columns (",
           vec2QuotedStr(prmsCols[missingCols]), ").\n\n",
           "The number of precipitation and temperature columns (and their ",
           "names) must match exactly with the meteorological CSV ",
           "(\"", model, "_Meteorological_", startDate, "_", endDate, 
           ".csv\"). Please correct this file and try again.\n\n", 
           if_else(datType != "Final",
                   paste0("(This error occurred for '", 
                          getFromSupplyControl_RR(sourceField), "')"),
                   paste0("Please investigate the component DAT files."))) |>
      errWrap() |>
      str_replace("(does not)", col_red("\\1")) |>
      stop()
    
  }
  
  
  # Make sure all fields are numeric next ("DATE" is an allowable exception)
  if (anyFalse(map_lgl(datFile[names(datFile) != "DATE"], is.numeric))) {
    
    # Identify the non-numeric columns
    nonNumCols <- which(!map_lgl(datFile, is.numeric))
    
    
    # Exclude the "DATE" column from this list, if it's present in 'datFile'
    if ("DATE" %in% names(datFile)) {
      
      nonNumCols <- nonNumCols |>
        base::setdiff(which(names(datFile) == "DATE"))
      
    }
    
    
    paste0("DAT File - Column Type Issue\n\n",
           "Every column in the ", datType, " input file for ", model, 
           " is expected to be numeric. However, ", length(nonNumCols),
           " column", if_else(length(nonNumCols) > 1, "s have", " has"), 
           " a different type (",
           vec2QuotedStr(names(datFile)[nonNumCols]), ").\n\n",
           "Please correct this file and try again.\n\n", 
           if_else(datType != "Final",
                   paste0("(This error occurred for '", 
                          getFromSupplyControl_RR(sourceField), "')"),
                   paste0("Please investigate the component DAT files."))) |>
      errWrap() |>
      str_replace("(does not)", col_red("\\1")) |>
      stop()
    
  }
  
  
  # For the next checks, give 'datFile' a "DATE" column
  datFile <- datFile |>
    mutate(DATE = paste0(YEAR, "-", MONTH, "-", DAY) |> 
             as.Date(format = "%Y-%m-%d"))
  
  
  # Next, get the start and end dates of the DAT file
  datRange <- datFile |>
    select(DATE) |>
    filter(DATE == min(DATE) | DATE == max(DATE)) |>
    arrange(DATE)
  
  
  # Make sure there are no missing dates in 'datFile'
  expectedDates <- seq(from = datRange$DATE[1],
                       to = datRange$DATE[2],
                       by = "days")
  
  
  # Check for missing dates
  if (anyFalse(expectedDates %in% datFile$DATE)) {
    
    # Identify the missing dates
    missingDates <- which(!(expectedDates %in% datFile$DATE))
    
    
    paste0("DAT File - Date Issue\n\n",
           "The ", datType, " input file for ", model, " is missing data for ", 
           length(missingDates), " day",
           if_else(length(missingDates) > 1, "s", ""), " (", 
           vec2QuotedStr(expectedDates[missingDates]), "). Please correct ",
           "this file and try again.\n\n", 
           if_else(datType != "Final",
                   paste0("(This error occurred for '", 
                          getFromSupplyControl_RR(sourceField), "')"),
                   paste0("Please investigate the component DAT files."))) |>
      errWrap() |>
      str_replace("(does not)", col_red("\\1")) |>
      stop()
    
  }
  
  
  # Similarly, check for mismatches between 'datFile' and 'expectedDates'
  # (Because of the previous check, this error will likely occur if a date is 
  #  duplicated in the DAT file)
  if (nrow(datFile) != length(expectedDates)) {
    
    # If the cause is a duplicate error, identify the duplicated dates
    dupDates <- table(datFile$DATE)
    
    dupDates <- dupDates[dupDates > 1]
    
    
    if (length(dupDates) > 0) {
      
      paste0("DAT File - Date Issue\n\n",
             "The ", datType, " input file for ", model, 
             " has multiple rows for the same date",
             if_else(length(dupDates) > 1, "s", ""), " (",
             vec2QuotedStr(names(dupDates)), ").\n\n", 
             "Please correct this issue. Every date should have exactly ",
             "one row in the DAT file.\n\n", 
             if_else(datType != "Final",
                     paste0("(This error occurred for '", 
                            getFromSupplyControl_RR(sourceField), "')"),
                     paste0("Please investigate the component ",
                            "DAT files."))) |>
        errWrap() |>
        str_replace("(does not)", col_red("\\1")) |>
        stop()
      
      # If this error occurred because of some other unknown issue,  
      # use this error message instead
    } else {
      
      paste0("DAT File - Data Issue\n\n",
             "The ", datType, " input file for ", model, " has an ",
             "unknown data issue. The number of rows in the dataset does ",
             "not match the number of days between the start and end ",
             "dates in that file. Please investigate.\n\n", 
             if_else(datType != "Final",
                     paste0("(This error occurred for '", 
                            getFromSupplyControl_RR(sourceField), "')"),
                     paste0("Please investigate the component ",
                            "DAT files."))) |>
        errWrap() |>
        str_replace("(unknown)", col_red("\\1")) |>
        stop()
      
    }
    
  }
  
  
  # If there are missing entries in the DAT file, alert the user
  if (anyNA(datFile)) {
    
    # Output the locations of missing values
    cat("\n\n\"NA\" Entries:\n")
    print(which(is.na(datFile), arr.ind = TRUE))
    
    paste0("DAT File - Missing Data Issue\n\n",
           if_else(sum(is.na(datFile)) > 1, 
                   "Missing values were ",
                   "A missing value was "), 
           " detected in the ", datType, " DAT file (see the above message ",
           "for locations). Please correct the file.\n\n", 
           if_else(datType != "Final",
                   paste0("(This error occurred for '", 
                          getFromSupplyControl_RR(sourceField), "')"),
                   paste0("Please investigate the component DAT files."))) |>
      errWrap() |>
      stop()
    
  }
  
  
  # For the next check, look for a logistical issue for the primary DAT file
  # (The one with long-running meteorological data)
  # 'startDate' can overlap with 'datFile', but if not, there should be no gaps
  # 'startDate' for the meteorological dataset should be, at most, on the next day
  # after the end of the dates in 'primaryDAT'
  if (datType == "Main" && startDate > max(datFile$DATE) + 1) {
    
    paste0("Main DAT File and 'startDate' - Data Gap Issue\n\n",
           "Because the meteorological dataset starts at ", startDate,
           ", there will be a data gap issue with this DAT file. Its ",
           "latest date is ", max(datFile$DATE), ", which means ",
           "that not all dates will have data when running ", model, 
           " with these files. Please adjust either the DAT file or ",
           "'startDate' in the control script.\n\n",
           "(This error occurred for '", 
           getFromSupplyControl_RR(sourceField), "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The final checks are specific to the SPI DAT file 
  if (datType == "SPI") {
    
    # Get the bounds of the water year being modeled
    wyBounds <- getModeledWY(endDate)
    
    
    # The predicted values in 'datFile' must extend to the end of the water year
    if (max(datFile$DATE) < wyBounds[2]) {
      
      paste0("Incomplete SPI DAT File - End of Water Year\n\n",
             "The DAT file that contains predictions for the current water ",
             "year is missing data. It does not extend to the end of the ",
             "water year (\"", wyBounds[2], "\"). Please adjust ",
             "this file.\n\n",
             "(This error occurred for '", 
             getFromSupplyControl_RR(sourceField), "')") |>
        errWrap() |>
        stop()
      
      # Similarly, if 'endDate' is an earlier date than the start of 'datFile', 
      # there should be no gap between then
    } else if (endDate + 1 < min(datFile$DATE)) {
      
      paste0("SPI DAT File and 'endDate' - Data Gap Issue\n\n",
             "Because the meteorological dataset ends at ", endDate,
             ", there will be a data gap issue with this DAT file. Its ",
             "earliest date is ", min(datFile$DATE), ", which means ",
             "that not all dates will have data when running ", model,
             " with these files. Please adjust either the DAT file or ",
             "'endDate' in the control script.\n\n",
             "(This error occurred for '", 
             getFromSupplyControl_RR(sourceField), "')") |>
        errWrap() |>
        stop()
      
    }
    
  }
  
  
  # If there are no issues, return 'datFile'
  # (It now has a "DATE" column)
  return(datFile |> arrange(DATE))
  
}



validateHistoricPrecipFile <- function (precipDF, sourceField, wyStart) {
  
  # For both PRMS and SRP DAT files, calculations are performed 
  # based on average precipitation data from PRISM
  
  # These daily precipitation values are averaged from grid cells located 
  # entirely within the watershed boundary 
  # (they correspond to the model domains of PRMS and SRP)
  
  # This function verifies that continuous precipitation data is present 
  # from "1981-01-01" to the beginning of the modeled water year in 'precipDF'
  prismStart <- "1981-01-01" |>
    as.Date(format = "%Y-%m-%d")
  
  
  # First, confirm that both expected columns are present
  expectedCols <- c("Date", "ppt (mm)")
  
  
  if (anyFalse(expectedCols %in% names(precipDF))) {
    
    # Identify the missing column(s)
    missingFields <- which(!(expectedCols %in% names(precipDF)))
    
    
    paste0("Historic Precipitation File - Missing Column Issue\n\n", 
           "For this script to work, the historic precipitation CSV must ",
           "contain ", length(expectedCols), " key column",
           if_else(length(expectedCols) > 1, "s", ""), " (",
           vec2QuotedStr(expectedCols), "). However, the file is missing ",
           if_else(length(missingFields) > 1, "fields", "a field"), ":\n\n",
           paste0("(*) ", expectedCols[missingFields], collapse = "\n\n"), 
           "\n\n",
           "Please revise this file accordingly.\n\n",
           "(This error occurred for '", getFromSupplyControl_RR(sourceField),
           "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # There should be no empty entries in 'precipDF' either
  if (anyNA(precipDF)) {
    
    paste0("Historic Precipitation File - Missing Value Issue\n\n", 
           "One or more missing elements were noted in the historic ",
           "precipitation CSV file. All data columns should have a value. ",
           "Please revise this file accordingly.\n\n",
           "(This error occurred for '", getFromSupplyControl_RR(sourceField),
           "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # After that, check for missing dates in 'precipDF'
  dateSeq <- seq(from = prismStart, to = wyStart - 1, by = "days")
  
  
  # Every date in 'dateSeq' should appear in 'precipDF' 
  # If that is the case, 'missingDates' will be empty
  missingDates <- dateSeq[!(dateSeq %in% precipDF$Date)]
  
  
  if (length(missingDates) > 0) {
    
    paste0("Historic Precipitation File - Missing Data Issue\n\n", 
           "The historic precipitation CSV is expected to contain ",
           "daily precipitation data from ", dateSeq[1], " to ", 
           wyStart - 1, ". However, ", length(missingDates), " ",
           "date", if_else(length(missingDates) > 1, "s are", " is"), " ",
           "missing. Please revise this file accordingly.\n\n",
           "(This error occurred for '", getFromSupplyControl_RR(sourceField),
           "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Check for duplicated dates next
  if (nrow(precipDF) != length(unique(precipDF[[expectedCols[1]]]))) {
    
    paste0("Historic Precipitation File - Duplicate Date Issue\n\n", 
           "The historic precipitation CSV file is expected to contain ",
           "one precipitation value per day (an average daily value for ",
           "the entire watershed domain). However, one or more dates in ",
           "this file are duplicated. Please revise this file.\n\n",
           "(This error occurred for '", getFromSupplyControl_RR(sourceField),
           "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Finally, ensure that the precipitation data is numeric
  if (!is.numeric(precipDF[[expectedCols[2]]])) {
    
    paste0("Historic Precipitation File - Non-Numeric Data Issue\n\n",
           "The precipitation column of this CSV file (\"", expectedCols[2],
           "\") is not being parsed as a numeric column. Since types ",
           "are assigned automatically, this indicates that the column ",
           "either contains non-number-related characters or is completely ",
           "empty.\n\n",
           "Please adjust this file and ensure that this column contains only ",
           "numeric values.\n\n",
           "(This error occurred for '", 
           getFromSupplyControl_RR(sourceField), "')") |>
      errWrap() |>
      str_replace("(not)", col_red("\\1")) |>
      stop()
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



checkForPreviousOutput <- function (filePath) {
  
  # Check for a file that was generated at a prior step in the workflow
  
  # 'filePath' can be a single string or a vector of paths
  
  
  if (anyFalse(file.exists(filePath))) {
    
    # Identify which files in 'filePath' are missing
    missingFiles <- which(!file.exists(filePath))
    
    
    # Use that in an error message
    paste0("File", if_else(length(missingFiles) > 1, "s", ""), " ",
           "From Previous Script Not Found\n\n",
           "The file", if_else(length(missingFiles) > 1, "s", ""), " ",
           vec2QuotedStr(filePath[missingFiles]), "should have been generated ",
           "by ", if_else(length(missingFiles) > 1, "", "a"), " ",
           "preceding script", if_else(length(missingFiles) > 1, "s", ""), " ",
           "in this process. However, ",
           if_else(length(missingFiles) > 1, "they were ", "it was "),
           "not found. Please run the previous scripts to completion before ",
           "running this one.\n\n") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return the normalized filepath if there are no issues
  return(filePath |> normalizePath(mustWork = TRUE))
  
}



validateSourceModelDirectory <- function (sourceDir, sourceField, model,
                                          reqFolders, reqFiles) {
  
  # Ensure that the user-provided directory containing model files is valid
  # For the PRMS and SRP models, certain files and folders are expected
  
  # If there are no errors, this function will return the normalized path to  
  # the model directory at the end
  
  
  # Make sure the script was given "PRMS" or "SRP" as input for 'model'
  if (!(model %in% c("PRMS", "SRP"))) {
    
    paste0("Script Error - Unrecognized Value for 'model'\n\n", 
           "The function `validateSourceModelDirectory` checks a source model ",
           "directory that contains files for running a specified model. ",
           "Therefore, the input variable 'model' should be either \"PRMS\" ",
           "or \"SRP\". However, it was input as \"", model, "\" instead.\n\n", 
           "Please correct the script and try again.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # After that, check if the directory is a SharePoint fragment
  # If it exists on SharePoint, convert 'sourceDir' into a SharePoint path
  if (dir.exists(makeSharePointPath(sourceDir))) {
    
    sourceDir <- makeSharePointPath(sourceDir)
    
  }
  
  
  # If the directory cannot be found, notify the user
  if (!dir.exists(sourceDir)) {
    
    paste0("Cannot Find the Specified ", model, " Directory\n\n",
           "In the RR Supply Control File, the location of the ", model, 
           " model files was specified in \"", sourceField, "\". ",
           "However, \"", sourceDir, "\" does not appear to exist.\n\n",
           "Please correct the value specified for \"", sourceField,
           "\" in the control spreadsheet.") |>
      errWrap() |> 
      stop()
    
  }
  
  
  # Next, check that the folders specified in 'reqFolders' are present
  folderExists <- paste0(sourceDir, "/", reqFolders) |>
    normalizePath(mustWork = FALSE) |>
    dir.exists()
  
  
  if (anyFalse(folderExists)) {
    
    paste0("Missing Components in the ", model, " Model Folder\n\n",
           "In the RR Supply Control File, the location of the ", model, 
           " model files was set to be \"", sourceDir, "\"\n\n", 
           "However, this directory does not contain of all the required ",
           "folders that a proper installation of ", model, " would have ", 
           "(", vec2QuotedStr(reqFolders[!folderExists]), "). Please obtain ",
           "a proper installation of ", model, " and/or correct the value ",
           "given in the control spreadsheet for \"", sourceField, "\".") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Repeat a similar check for files rather than folders this time
  # Every file in 'reqFiles' should appear in the model folder
  fileExists <- paste0(sourceDir, "/", reqFiles) |>
    normalizePath(mustWork = FALSE) |>
    file.exists()
  
  
  if (anyFalse(fileExists)) {
    
    paste0("Missing Components in the ", model, " Model Folder\n\n",
           "In the RR Supply Control File, the location of the ", model, 
           " model files was set to be \"", sourceDir, "\"\n\n", 
           "However, this directory does not contain of all the required ",
           "files that a proper installation of ", model, " would have ", 
           "(", vec2QuotedStr(reqFiles[!fileExists]), "). Please obtain ",
           "a proper installation of ", model, " and/or correct the value ",
           "given in the control spreadsheet for \"", sourceField, "\".") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return 'sourceDir' if there are no issues
  # (If 'sourceDir' points to a SharePoint location, it has been updated 
  #  in this function to reflect that)
  return(sourceDir |> normalizePath())
  
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
  
  
  # Finally, check for the main executable file
  exePath <- paste0(prmsPath, "/bin/gsflow.exe") |>
    normalizePath(mustWork = FALSE)
  
  
  if (!file.exists(exePath)) {
    
    paste0("Missing PRMS EXE File\n\n",
           "When the PRMS folder was copied into the \"ProcessedData\" ", 
           "folder, \"gsflow.exe\" was present among the model files. ", 
           "However, it cannot be found now. Please investigate.\n\n",
           "(This error occurred for \"", exePath, "\")") |>
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



validateModelCopy_SRP <- function () {
  
  # In a prior script, SRP model files were copied to the "ProcessedData" folder
  # Verify that it exists
  
  # This function also returns the path to the model folder
  
  
  # The expected path of the "SRPHM_update_ag" folder
  srpPath <- "ProcessedData/SRPHM_update_ag" |> normalizePath(mustWork = FALSE)
  
  
  # Make sure that that folder exists 
  if (!dir.exists(srpPath)) {
    
    paste0("SRP Folder Not Found\n\n",
           "A copy of the SRP model files should have been added ",
           "to the \"ProcessedData\" folder in an earlier script. ",
           "However, it was not found. ",
           "Please run the previous scripts before running this one.\n\n",
           "The expected directory was \"", srpPath, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Also confirm that the control file for SRP exists
  controlPath <- paste0(srpPath, "/SRPHM_update.control") |> 
    normalizePath(mustWork = FALSE)
  
  
  if (!file.exists(controlPath)) {
    
    paste0("Missing SRP Control File\n\n",
           "When the SRP folder was copied into the \"ProcessedData\" ", 
           "folder, a control file was present in the root folder. ",
           "However, it cannot be found now. Please investigate.\n\n",
           "(This error occurred for \"", controlPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # A batch file should be present in the model files too
  # Check for that as well
  batPath <- paste0(srpPath, "/Run_updated_Model.bat") |>
    normalizePath(mustWork = FALSE)
  
  
  if (!file.exists(batPath)) {
    
    paste0("Missing SRP Batch File\n\n",
           "When the SRP folder was copied into the \"ProcessedData\" ", 
           "folder, a batch file was present among the model files. ", 
           "However, it cannot be found now. Please investigate.\n\n",
           "(This error occurred for \"", batPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Finally, check for the main executable file
  exePath <- paste0(prmsPath, "/gsflow_ag.exe") |>
    normalizePath(mustWork = FALSE)
  
  
  if (!file.exists(exePath)) {
    
    paste0("Missing SRP EXE File\n\n",
           "When the SRP folder was copied into the \"ProcessedData\" ", 
           "folder, \"gsflow_ag.exe\" was present among the model files. ", 
           "However, it cannot be found now. Please investigate.\n\n",
           "(This error occurred for \"", exePath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return 'srpPath' if there are no issues
  return(srpPath)
  
}



checkForModelOutputs_SRP <- function (srpPath, modelOutput = NULL,
                                      includeScriptGeneratedOutput = FALSE) {
  
  # Double-check that the model ran successfully
  
  # There should be several key files in the "output" folder
  
  
  # These files were all generated by SRP
  outFiles <- c("gsflow.log",
                paste0("SRP_inflow_", 1:6, ".gag"))
  
  
  # If 'includeScriptGeneratedOutput' is TRUE, include the console output 
  # text file generated by an earlier script in this check
  if (includeScriptGeneratedOutput) {
    
    outFiles <- c(outFiles,
                  "SRP_Console_Output.txt")
    
  }
  
  
  # Check if any files are missing
  missingFiles <- which(!file.exists(outFiles |>
                                       paste0(srpPath, "/", 
                                              ... = _) |>
                                       normalizePath(mustWork = FALSE)))
  
  
  if (length(missingFiles) > 0) {
    
    # Include the model run outputs in the console if 'modelOutput' is not NULL
    if (!is.null(modelOutput)) {
      
      cat("\n\nModel Output Message(s):\n\n")
      print(modelOutput)
      
    }
    
    
    paste0("Missing SRP Output File", 
           if_else(length(missingFiles) > 1, "s", ""), "\n\n",
           "The SRP model run did not generate all of the expected ",
           "files (missing ", vec2QuotedStr(outFiles[missingFiles]),
           "). Please investigate ",
           if_else(is.null(modelOutput),
                   "the model's output messages (included above)",
                   "this issue"),
           ".\n\n", 
           "(This error occurred for \"", srpPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}

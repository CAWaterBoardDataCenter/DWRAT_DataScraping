# Verify that all required weather data has been downloaded
# Then, reformat the data into a structure suitable for the SRP DAT file


# NOTE: It is unclear yet if CIMIS data will be incorporated too
#       Hold off on deleting all the extra code until that point is clarified


# This script has two required input files:

# The first one is the station input file for PRISM

# This time, in addition to the "STATION_ID" column, the script requires 
# columns that link these stations to specific columns in the SRP DAT input file

# The required fields are:
#  (1) STATION_ID
#  (2) SRP_PRECIP_NAME
#  (3) SRP_TMIN_NAME
#  (4) SRP_TMAX_NAME

# Every SRP station should be linked to at least one column among the 
# 2 precipitation columns and 2 max/min temperature columns

# In addition to these files, the outputs of the web scraping scripts are all required:
#  (1) "WebData/PRISM_SRP_Data_[startDate]_[endDate].csv"


# These files will be combined into a single output file:
#  (1) "ProcessedData/SRP_Meteorological_[startDate]_[endDate].csv"


#### Setup ####

# Clear the environment
remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")


#### Functions ####

mainProcedure <- function (allTempColumnsFromPRISM = TRUE) {
  
  cat("\n\n")
  cat("Starting 'RRS_011_Process_SRP_Weather_Data.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Start with a vector containing every single required input file
  inputFiles <- c("PRISM INPUT" = getFromSupplyControl_RR("PRISM_SRP_STATIONS_CSV"),
                  
                  "CIMIS INPUT" = getFromSupplyControl_RR("CIMIS_STATIONS_CSV"),
                  
                  "PRISM OUTPUT" = paste0("WebData/PRISM_SRP_Data_",
                                          startDate, "_", endDate, ".csv"),
                  "CIMIS OUTPUT" = paste0("WebData/CIMIS_API_Data_",
                                          startDate, "_", endDate, ".csv")
                  )
  
  
  # Check if any required input files are missing
  if (anyFalse(file.exists(inputFiles))) {
    
    # Get the names of the missing files before sending a message
    missingFiles <- inputFiles[!file.exists(inputFiles)]
    
    
    # Output the error
    stop(paste0("Missing Required Input File", 
                if_else(length(missingFiles) > 1, "s", ""), "\n\n",
                "This script requires that the PRISM web scraping scripts ",
                "was run for the chosen date range (",
                startDate, " to ", endDate, ")\n\n",
                "However, the following file", 
                if_else(length(missingFiles) > 1, "s are", " is"), 
                " missing:\n\n",
                paste0(" (*) ", names(missingFiles), ": \"", 
                       missingFiles, "\"", collapse = "\n\n"), "\n\n",
                "Please prepare any required input files and then run ",
                "the corresponding script", 
                if_else(length(missingFiles) > 1, "s", ""),
                " first") |>
           errWrap())
    
  }
  
  
  # Read in the files next
  prismInput <- inputFiles[1] |> getFile() |> unique()
  #noaaInput <- inputFiles[2] |> getFile() |> unique()
  #rawsInput <- inputFiles[3] |> getFile() |> unique()
  cimisInput <- inputFiles[2] |> getFile() |> unique() |> 
    filter(!is.na(SRP_PRECIP_NAME) | !is.na(SRP_TMIN_NAME))
  
  
  prismDF <- getPRISM(inputFiles[3])
  #noaaDF <- getDelim(inputFiles[6], delim = ",")
  #rawsDF <- getDelim(inputFiles[7], delim = ",")
  cimisDF <- getDelim(inputFiles[4], delim = ",") |> 
    filter(STATION_ID %in% cimisInput$STATION_ID)
  
  
  # Validate all eight variables next
  cat("[1/2]\tChecking all input files...\n")
  
  
  # Ensure that all of them have the expected formatting
  validateInputs(prismInput, #noaaInput, rawsInput, 
                 cimisInput,
                 prismDF,#, noaaDF, rawsDF, 
                 cimisDF, 
                 inputFiles)
  
  
  cat("\tDone!\n\n")
  
  
  # After all validation requirements have been cleared, prepare a single
  # meteorological dataset (combining data from NOAA, RAWS, and CIMIS)
  #cat("[2/2]\tPreparing final meteorological dataset...\n")
  
  
  #meteorDF <- combineMeteorologicalDatasets(cimisInput, cimisDF,
  #                                          startDate, endDate)
  
  
  # Missing entries in this dataset will be substituted with PRISM data
  # (And if 'allTempColumnsFromPRISM' is set to TRUE, all temperature data will 
  #  come from PRISM)
  #meteorDF <- prismSub(meteorDF, prismDF, prismInput, TRUE)
  prismProcessed <- reformatClimateData(prismDF, prismInput, "PRISM")
  
  cat("\tDone!\n\n")
  
  
  # Once this step is complete, write 'meteorDF' to a file
  outFile <- paste0("ProcessedData/SRP_Meteorological_", startDate, "_",
                    endDate, ".csv")
  
  
  prismProcessed |>
    writeOutput(outFile, "write_csv")
  
  
  # Output a completion message
  cat(col_green("\n'RRS_011_Process_SRP_Weather_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateInputs <- function (prismInput, cimisInput, prismDF, cimisDF, inputFiles) {
  
  # Verify that all eight tibbles are formatted as expected
  
  
  # First, check the four "INPUT" tibbles
  validateStationInputs(prismInput, inputFiles[1])
  validateStationInputs(cimisInput, inputFiles[2])
  
  
  # Validate the weather output tibbles next
  
  # Each website returns data in a slightly different format
  # But the general expectations are similar in all cases
  validateWebData(prismDF, inputFiles[3], prismInput$STATION_ID)
  validateWebData(cimisDF, inputFiles[4], cimisInput$STATION_ID)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateStationInputs <- function (inputDF, inputPath) {
  
  # The station input files for PRISM, NOAA, RAWS, and CIMIS were previously 
  # validated in their respective web scraping scripts
  
  # However, this script has additional requirements
  
  # This function checks specifically for the fields and formatting required in this procedure
  
  
  # HARD-CODED EXPECTATION
  # The number of SRP "PRECIP" and "TMIN"/"TMAX" fields is specified here 
  # These numbers dictate the acceptable range of values for the station input 
  # file's SRP-related fields 
  numPrecipFields <- 2 
  numTempFields <- 2 
  
  # With the values set above, the precipitation field can have values between 
  # "PRECIP1" and "PRECIP2" (inclusive)
  # Similarly, the maximum and minimum temperature fields can have values between 
  # "TMAX1"/"TMIN1" and "TMAX2"/"TMIN2" (inclusive)
  
  
  # For this script's procedure to succeed, all input files must have these four columns:
  #    (*) STATION_ID
  #    (*) SRP_PRECIP_NAME
  #    (*) SRP_TMIN_NAME
  #    (*) SRP_TMAX_NAME
  inputFieldNames <- c("STATION_ID", "SRP_PRECIP_NAME", 
                       "SRP_TMIN_NAME", "SRP_TMAX_NAME")
  
  
  # Start by confirming that the field names appear in 'inputDF'
  if (anyFalse(inputFieldNames %in% names(inputDF))) {
    
    # Identify which fields are missing
    missingFields <- which(!(inputFieldNames %in% names(inputDF)))
    
    
    # Output an error message
    stop(paste0("Station Input File - Missing Column Issue\n\n", 
                "For this script to work, the PRISM input files must contain ", 
                length(inputFieldNames), " key column",
                if_else(length(inputFieldNames) > 1, "s", ""), " (",
                vec2QuotedStr(inputFieldNames), ")\n\n",
                "However, the \"", names(inputPath), "\" file is missing ",
                if_else(length(missingFields) > 1, "fields", "a field"), ":\n\n",
                paste0("(*) ", inputFieldNames[missingFields], collapse = "\n\n"), "\n\n",
                "Please revise the input file (\"", inputPath, "\") accordingly") |>
           errWrap())
    
  }
  
  
  # The station ID was previously validated in the scraping scripts
  # The next focus will be the "SRP" fields
  
  
  # In the SRP DAT file, there are 2 precipitation fields and 2 max/min
  # temperature fields
  
  # The values that appear in the SRP fields should be one of these 
  # column names (or NA)
  
  
  # Start with the SRP Precipitation field
  # The values should be "NA", or something between "PRECIP1" and "PRECIP2" (inclusive)
  if (anyFalse(inputDF[[inputFieldNames[2]]] %in% c(NA, paste0("PRECIP", 1:numPrecipFields)))) {
    
    stop(paste0("Station Input File - Invalid SRP Value Issue\n\n", 
                "The \"", names(inputPath), "\" file contains an invalid value for the ",
                "field \"", inputFieldNames[2], "\" \n\n",
                "Each row should either be blank, or it should contain a text string ",
                "like \"PRECIP1\" (up to \"PRECIP", numPrecipFields, "\")\n\n", 
                "Please revise the input file (\"", inputPath, "\") accordingly") |>
           errWrap())
    
  }
  
  
  # Use a similar check for the minimum temperature field next
  # The values should be "NA", or something between "TMIN1" and "TMIN8" (inclusive)
  if (anyFalse(inputDF[[inputFieldNames[3]]] %in% c(NA, paste0("TMIN", 1:numTempFields)))) {
    
    stop(paste0("Station Input File - Invalid SRP Value Issue\n\n", 
                "The \"", names(inputPath), "\" file contains an invalid value for the ",
                "field \"", inputFieldNames[3], "\" \n\n",
                "Each row should either be blank, or it should contain a text string ",
                "like \"TMIN1\" (up to \"TMIN", numTempFields, "\")\n\n", 
                "Please revise the input file (\"", inputPath, "\") accordingly") |>
           errWrap())
    
  }
  
  
  # Repeat the check for the "TMAX" field
  # The values should be "NA", or something between "TMAX1" and "TMAX8" (inclusive)
  if (anyFalse(inputDF[[inputFieldNames[4]]] %in% c(NA, paste0("TMAX", 1:numTempFields)))) {
    
    stop(paste0("Station Input File - Invalid SRP Value Issue\n\n", 
                "The \"", names(inputPath), "\" file contains an invalid value for the ",
                "field \"", inputFieldNames[4], "\" \n\n",
                "Each row should either be blank, or it should contain a text string ",
                "like \"TMAX1\" (up to \"TMAX", numTempFields, "\")\n\n", 
                "Please revise the input file (\"", inputPath, "\") accordingly") |>
           errWrap())
    
  }
  
  
  # Next, confirm that every row has at least one non-NA value for the three SRP fields
  # Every station should have a corresponding SRP field
  # So at least one column between "PRECIP", "TMIN", and "TMAX" should have a non-NA value
  # in each row
  
  # Define a temporary variable to help with this
  # If all three columns contain "NA", this column's value will be TRUE
  inputDF <- inputDF |>
    mutate(ALL_NA = is.na(get(inputFieldNames[2])) & 
             is.na(get(inputFieldNames[3])) &
             is.na(get(inputFieldNames[4])))
  
  
  # If TRUE appears for any row in "ALL_NA", output an error message
  if (TRUE %in% inputDF$ALL_NA) {
    
    stop(paste0("Station Input File - Invalid SRP Value Issue\n\n", 
                "The \"", names(inputPath), "\" file contains a station without ",
                "a corresponding SRP field identified\n\n",
                "Across the ", length(inputFieldNames) - 1, " SRP columns, each ",
                "row should contain a SRP field name in at least one column\n\n",
                "Please revise the input file (\"", inputPath, "\") accordingly") |>
           errWrap())
    
  }
  
  
  # The final check is to ensure that "TMIN" and "TMAX" have corresponding 
  # values in the same row
  # If the "TMIN" value is "NA", it should be "NA" for "TMAX" too
  # Similarly, if "TMIN" has a value, "TMAX" should have an equivalent value
  # (The numbers in both labels should be the same)
  inputDF <- inputDF |>
    mutate(TEMP_MISMATCH = (is.na(get(inputFieldNames[3])) & !is.na(get(inputFieldNames[4]))) |
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
    
    stop(paste0("Station Input File - Invalid SRP Value Issue\n\n", 
                "The \"", names(inputPath), "\" file contains ", sum(inputDF$TEMP_MISMATCH),
                " instance", if_else(sum(inputDF$TEMP_MISMATCH) > 1, "s", ""), " ",
                "where \"", inputFieldNames[3], "\" and \"", inputFieldNames[4], 
                "\" do not contain matching values\n\n",
                "Either both SRP temperature columns should be empty, or they ",
                "should have corresponding values (e.g., \"TMIN3\" and \"TMAX3\" ",
                " in the same row)\n\n",
                "Please revise the input file (\"", inputPath, "\") accordingly") |>
           errWrap())
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



validateWebData <- function (climateDF, inputPath, stationVec) {
  
  # Check for errors in the downloaded web data
  
  # This function mainly checks for expected column names and "NA" values
  
  
  # First, extract the data source name from the element name for 'inputPath'
  dataSource <- names(inputPath) |> str_extract("^[A-Z]+")
  
  
  # Make sure that procedure was successful
  if (!(dataSource %in% c("PRISM", "NOAA", "CIMIS", "RAWS"))) {
    
    stop(paste0("Unexpected Data Source\n\n", 
                "The name \"", dataSource, "\" is not recognized; ",
                "please fix the script\n\n",
                "The function `validateWebData()` uses the vector names ",
                "in 'inputFiles' and extracts the data source name. It ",
                "expects \"PRISM\", \"NOAA\", \"RAWS\", or \"CIMIS\" as ",
                "acceptable values.") |>
           errWrap())
    
  }
  
  
  # After that, get a vector of the expected column names for this dataset
  colVec <- expectedColumnNames(dataSource)
  
  
  # Confirm that all of these column names appear in 'climateDF'
  if (anyFalse(colVec %in% names(climateDF))) {
    
    # Identify which columns are missing
    missingVals <- which(!(colVec %in% names(climateDF)))
    
    
    stop(paste0("Web Data Output File - Formatting Issue\n\n",
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
           errWrap())
    
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
    
    
    stop(paste0("Web Data Output File - Unrecognized Station(s)\n\n",
                "The \"", names(inputPath), "\" file has one or more stations ",
                "that do not appear in its corresponding input file (",
                vec2QuotedStr(unique(climateDF$STATION_ID)[extraStations]), 
                ")\n\n", 
                "Please investigate \"", inputPath, "\"") |>
           errWrap())
    
  }
  
  
  # Note: The reverse is not required because some stations may lack data 
  #       for the user-specified date range and be missing from the output
  
  
  # Return nothing
  return(invisible(NULL))
  
}



expectedColumnNames <- function (dataSource) {
  
  # Different websites return climate data in different formats
  # As a result, the expected column names will differ in formatting
  
  # To make it easier to address changes to column names in the future, 
  # this function has the "hard-coded" column names for each data source
  
  # Other functions in this script will call this function to get this information
  
  
  # This function returns a named vector
  # The element names are the desired column names
  # The actual elements themselves are the names that appear in the weather data files
  
  # Note: In all cases, the expected revised column names are "STATION_ID", "DATE",
  # "PRECIP", "TMIN", and "TMAX" (i.e., these should all appear as the element names)
  
  
  if (dataSource == "PRISM") {
    
    nameVec <- c("STATION_ID" = "Name",
                 "DATE" = "Date",
                 "PRECIP" = "ppt (inches)",
                 "TMIN" = "tmin (degrees F)",
                 "TMAX" = "tmax (degrees F)")
    
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
    stop(paste0("Misuse of `expectedColumnNames()`\n\n", 
                "The input \"", dataSource, "\" is not recognized; ",
                "please fix the script\n\n",
                "The function `expectedColumnNames()` requires a data ",
                "source's name as input (either \"PRISM\", \"NOAA\", ",
                "\"RAWS\", or \"CIMIS\")\n\n") |>
           errWrap())
    
  }
  
  
  # Check that the developer coded this vector correctly
  # (All vectors should have the same length and the same replacement names)
  if (length(nameVec) != 5 ||
      anyFalse(c("STATION_ID", "DATE", "PRECIP", "TMIN", "TMAX") %in% 
               names(nameVec))) {
    
    stop(paste0("Issue in `expectedColumnNames()`\n\n", 
                "The name vector for ", dataSource, " may contain an issue\n\n",
                "Regardless of source, 5 specific columns are expected (",
                vec2QuotedStr(c("STATION_ID", "DATE", "PRECIP", "TMIN", "TMAX")),
                ")\n\n",
                "The name vector should contain the corresponding raw data names ",
                "(and link them to one of these columns)") |>
           errWrap())
    
  }
  
  
  # If there are no issues, return 'nameVec'
  return(nameVec)
  
}



combineMeteorologicalDatasets <- function (cimisInput, cimisDF,
                                           startDate, endDate) {
  
  # Format the data for easier integration into the SRP DAT file
  # For each station, the relevant SRP column names are listed in the input files
  
  
  # Start with building a skeleton for the final dataset
  meteorDF <- tibble(DATE = seq(from = startDate, to = endDate, by = "days"))
  
  
  # Add columns for precipitation, minimum temperature, and maximum temperature
  
  
  # To help specify these column names (and get their ordering right),
  # make a data frame for the column names
  srpColumnNames <- c(#noaaInput$SRP_PRECIP_NAME, rawsInput$SRP_PRECIP_NAME, 
                       cimisInput$SRP_PRECIP_NAME,
                       #noaaInput$SRP_TMIN_NAME, rawsInput$SRP_TMIN_NAME, 
                       cimisInput$SRP_TMIN_NAME,
                       #noaaInput$SRP_TMAX_NAME, rawsInput$SRP_TMAX_NAME, 
                       cimisInput$SRP_TMAX_NAME) |>
    unique() |> sort() |>
    matrix(ncol = 1) |> data.frame() |> set_names("COLUMN") |>
    filter(!is.na(COLUMN)) |>
    mutate(TYPE = str_remove(COLUMN, "[0-9]+$"),
           NUMBER = str_extract(COLUMN, "[0-9]+$") |> as.numeric()) |>
    arrange(TYPE, NUMBER)
  
  # The above code pools together all SRP-related field names into a vector, 
  # then a matrix, and finally a data frame
  # The column is arbitrarily titled "COLUMN" 
  # Then, two variables are created based on the type of SRP variable and the 
  # value of the column name's number
  # Finally, the data frame is sorted based on the column type 
  # (PRECIP > TMAX > TMIN) and the column number
  
  # Note: With the default value of "NA" for the argument "na.last" in sort(), 
  # the NA entries are removed automatically
  # But just for redundancy, a filter to remove "NA" is also applied to the data frame
  
  
  # Add these columns to 'meteorDF'
  meteorDF[srpColumnNames$COLUMN] <- NA_real_
  
  
  
  # In another function, reformat 'noaaDF', 'rawsDF', and 'cimisDF'
  # noaaProcessed <- noaaDF |>
  #   reformatClimateData(noaaInput, "NOAA")
  # 
  # rawsProcessed <- rawsDF |>
  #   reformatClimateData(rawsInput, "RAWS")
  # 
  cimisProcessed <- cimisDF |>
    reformatClimateData(cimisInput, "CIMIS")
  
  
  # Bind these processed data frames to 'meteorDF'
  # (Other than "DATE", the columns in the processed tibbles should replace 
  #  the corresponding ones in 'meteorDF')
  meteorDF <- meteorDF |>
    # Aside from "DATE", remove all SRP fields from 'meteorDF' that 
    # appear in the processed tibble
    #select(-all_of(names(noaaProcessed)[names(noaaProcessed) != "DATE"])) |>
    # Then, join the processed tibble to 'meteorDF'
    #left_join(noaaProcessed, by = "DATE", relationship = "one-to-one") |>
    # Repeat with RAWS
    #select(-all_of(names(rawsProcessed)[names(rawsProcessed) != "DATE"])) |>
    #left_join(rawsProcessed, by = "DATE", relationship = "one-to-one") |>
    # Repeat with CIMIS
    select(-all_of(names(cimisProcessed)[names(cimisProcessed) != "DATE"])) |>
    left_join(cimisProcessed, by = "DATE", relationship = "one-to-one")
  
  
  # Return the revised 'meteorDF'
  return(meteorDF |>
           select(DATE, all_of(srpColumnNames$COLUMN)))
  
}



reformatClimateData <- function (climateDF, climateInput, dataSource) {
  
  # The 'climateDF' data frames need to be widened 
  # (so that each station's data is in its own separate column)
  
  # The "SRP" column names in 'climateInput' will then be used to switch 
  # from the station IDs to the SRP field names
  
  
  # Start by renaming the columns in 'climateDF' to be consistent 
  # Then, pivot the dataset into a wider format (where each station has 
  # three of its own columns--one for each SRP field)
  widerDF <- climateDF |>
    select(all_of(expectedColumnNames(dataSource))) |>
    pivot_wider(names_from = STATION_ID,
                values_from = c(PRECIP, TMIN, TMAX),
                names_sep = "_")
  
  
  # After that, prepare the SRP-equivalent names using 'climateInput'
  # Appending the station IDs to "PRECIP"/"TMAX"/"TMIN" gives the 
  # column names that appear in 'widerDF'
  # The values in "SRP_PRECIP_NAME", "SRP_TMAX_NAME", and "SRP_TMIN_NAME" 
  # are the intended replacements for these column names
  equivalentNames <- climateInput |>
    mutate(NAME_1 = paste0("PRECIP_", STATION_ID),
           NAME_2 = paste0("TMAX_", STATION_ID),
           NAME_3 = paste0("TMIN_", STATION_ID)) |>
    select(NAME_1, NAME_2, NAME_3, 
           SRP_PRECIP_NAME, SRP_TMAX_NAME, SRP_TMIN_NAME)
  
  
  # Create a vector from 'equivalentNames' that can be used with rename()
  renameVec <- c(equivalentNames$NAME_1, equivalentNames$NAME_2,
                 equivalentNames$NAME_3) |>
    set_names(c(equivalentNames$SRP_PRECIP_NAME, equivalentNames$SRP_TMAX_NAME, 
                equivalentNames$SRP_TMIN_NAME))
  
  
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
                "precipitation column used in the SRP model.\n\n", 
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
remove(list = ls())

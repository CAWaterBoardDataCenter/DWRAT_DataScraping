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
remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Supply_Validation_Functions.R")


#### Functions ####

mainProcedure <- function (allTempColumnsFromPRISM = TRUE) {
  
  cat("\n\n")
  cat("Starting 'RRS_005_Process_PRMS_Weather_Data.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Start with a vector containing every single required input file
  inputFiles <- c("PRISM INPUT" = getFromSupplyControl_RR("PRISM_PRMS_STATIONS_CSV"),
                  
                  "NOAA INPUT" = getFromSupplyControl_RR("NOAA_STATIONS_CSV"), 
                  
                  "RAWS INPUT" = getFromSupplyControl_RR("RAWS_STATIONS_CSV"), 
                  
                  "CIMIS INPUT" = getFromSupplyControl_RR("CIMIS_STATIONS_CSV"),
                  
                  "PRISM OUTPUT" = paste0("WebData/PRISM_PRMS_Data_",
                                          startDate, "_", endDate, ".csv"),
                  "NOAA OUTPUT" = paste0("WebData/NOAA_API_Data_",
                                         startDate, "_", endDate, ".csv"),
                  "RAWS OUTPUT" = paste0("WebData/RAWS_HTTP_Data_",
                                         startDate, "_", endDate, ".csv"),
                  "CIMIS OUTPUT" = paste0("WebData/CIMIS_API_Data_",
                                          startDate, "_", endDate, ".csv"))
  
  
  # Check if any required input files are missing
  if (anyFalse(file.exists(inputFiles))) {
    
    # Get the names of the missing files before sending a message
    missingFiles <- inputFiles[!file.exists(inputFiles)]
    
    
    # Output the error
    stop(paste0("Missing Required Input File", 
                if_else(length(missingFiles) > 1, "s", ""), "\n\n",
                "This script requires that the PRISM, NOAA, RAWS, and CIMIS ",
                "web scraping scripts are run for the chosen date range (",
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
  noaaInput <- inputFiles[2] |> getFile() |> unique()
  rawsInput <- inputFiles[3] |> getFile() |> unique()
  cimisInput <- inputFiles[4] |> getFile() |> unique()
  
  prismDF <- getPRISM(inputFiles[5])
  noaaDF <- getDelim(inputFiles[6], delim = ",")
  rawsDF <- getDelim(inputFiles[7], delim = ",")
  cimisDF <- getDelim(inputFiles[8], delim = ",")
  
  
  # Validate all eight variables next
  cat("[1/2]\tChecking all input files...\n")
  
  
  # Ensure that all of them have the expected formatting
  validateInputs(prismInput, noaaInput, rawsInput, cimisInput,
                 prismDF, noaaDF, rawsDF, cimisDF, inputFiles)
  
  
  cat("\tDone!\n\n")
  
  
  # After all validation requirements have been cleared, prepare a single
  # meteorological dataset (combining data from NOAA, RAWS, and CIMIS)
  cat("[2/2]\tPreparing final meteorological dataset...\n")
  
  
  meteorDF <- combineMeteorologicalDatasets(noaaInput, rawsInput, cimisInput,
                                            noaaDF, rawsDF, cimisDF,
                                            startDate, endDate)
  
  
  # For archival purposes, save 'meteorDF' without any PRISM data substitution
  meteorDF |>
    writeOutput(paste0("ProcessedData/PRMS_Pre-PRISM_Meteorological_", 
                       startDate, "_", endDate, ".csv"), "write_csv",
                quietly = TRUE)
  
  
  # Missing entries in this dataset will be substituted with PRISM data
  # (And if 'allTempColumnsFromPRISM' is set to TRUE, all temperature data will 
  #  come from PRISM)
  meteorDF <- prismSub(meteorDF, prismDF, prismInput, allTempColumnsFromPRISM)
  
  
  cat("\tDone!\n\n")
  
  
  # Once this step is complete, write 'meteorDF' to a file
  outFile <- paste0("ProcessedData/PRMS_Meteorological_", startDate, "_",
                    endDate, ".csv")
  
  
  meteorDF |>
    writeOutput(outFile, "write_csv")
  
  
  # Output a completion message
  cat(col_green("\n'RRS_005_Process_PRMS_Weather_Data.R' is complete!\n\n"))
  
  
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
  validateStationInputs(prismInput, inputFiles[1], "PRMS", numPrecip, numTemp)
  validateStationInputs(noaaInput, inputFiles[2], "PRMS", numPrecip, numTemp)
  validateStationInputs(rawsInput, inputFiles[3], "PRMS", numPrecip, numTemp)
  validateStationInputs(cimisInput, inputFiles[4], "PRMS", numPrecip, numTemp)
  
  
  # Validate the four weather output tibbles next
  
  # Each website returns data in a slightly different format
  # But the general expectations are similar in all cases
  validateWebData(prismDF, inputFiles[5], prismInput$STATION_ID, siPRISM = TRUE)
  validateWebData(noaaDF, inputFiles[6], noaaInput$STATION_ID)
  validateWebData(rawsDF, inputFiles[7], rawsInput$STATION_ID)
  validateWebData(cimisDF, inputFiles[8], cimisInput$STATION_ID)
  
  
  # Return nothing
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
remove(list = ls())

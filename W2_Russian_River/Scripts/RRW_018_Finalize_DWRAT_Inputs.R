# This script gathers and validates the inputs for Paradigm DWRAT

# Then, "RR_Connected.py" is adjusted to reference input files' paths


# The required inputs for Paradigm DWRAT are:

#  (1) The Russian River's Master Demand Table from the SDA Demand procedure
#      (e.g., "RR_MDT_[YEAR1]_[YEAR2].csv")

#  (2) Estimates of evapotranspiration in the Upper Russian River and Lower
#      Russian River (e.g., "ET.csv")

#  (3) A CSV specifying the connectivity between sub-basins in the watershed
#      (e.g., "basins.csv")

#  (4) Historic PVP flow estimates (from Sonoma County Water Agency)

#  (5) Forecasted PVP flow estimates (from Sonoma County Water Agency)

#  (6) Choices on the type of forecasted PVP flows to use
#      (PVP Variance or No Variance; Similar Water Year or Dry Water Year)


# Consequently, the script has additional required inputs:

#  (1) 


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_018_Finalize_DWRAT_Inputs.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # First check for the required components from previous scripts
  cat("\n[1/4]\tChecking components from previous scripts...\n")
  
  
  # Confirm that the model hydrology folder exists and get its path
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Confirm that the "Raw_Flows" CSV was generated previously as well
  # Its filename is written in the metadata CSV
  metaDF <- paste0(dirPath, "/metadata.csv") |>
    getFile()
  
  
  if (!("RAW_FLOWS_CSV" %in% names(metaDF))) {
    
    paste0("Missing Field in Metadata\n\n", 
           "\"RAW_FLOWS_CSV\" is a column added to the metadata CSV file ",
           "at the end of a prior script. However, it was not detected. ",
           "Please make sure to run all prior scripts to completion ",
           "before trying to run this one.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Use 'metaDF' to get the path to the Raw Flows CSV
  rawFlowsPath <- paste0(dirPath, "/DWRAT/Input/", metaDF$RAW_FLOWS_CSV[1]) |>
    checkForPreviousOutput()
  
  
  cat("\tDone!\n\n")
  
  
  # Import required input values from the control sheet
  cat("[2/4]\tGathering and validating inputs...\n")
  
  
  # Define a tibble with all required paths and input values
  inputDF <- tibble("MDT" = 
                      getFromControl_RR("MASTER_DEMAND_TABLE_CSV"),
                    "ET" = 
                      getFromControl_RR("EVAPOTRANSPIRATION_ESTIMATES_XLSX"),
                    "BASIN" =
                      getFromControl_RR("SUBBASIN_CONNECTIVITY_CSV"),
                    "PVP_HISTORIC" = 
                      getFromControl_RR("PVP_HISTORIC_FLOWS_LOCATION"),
                    "PVP_FORECAST" =
                      getFromControl_RR("PVP_FORECASTED_FLOWS_LOCATION"),
                    "PVP_VARIANCE" =
                      getFromControl_RR("PVP_FORECASTED_FLOWS_VARIANCE"),
                    "PVP_WY_TYPE" =
                      getFromControl_RR("PVP_FORECASTED_FLOWS_WY_TYPE")) |>
    setupInputPaths()
  
  
  # Next, read in the five input files
  # Validate their data as well
  mdtDF <- getFile(inputDF$MDT) |>
    validateMDT(inputDF$MDT)
  
  etDF <- getFile(inputDF$ET) |>
    validateET(inputDF$ET)
  
  basinDF <- getFile(inputDF$BASIN) |>
    validateBasins(inputDF$BASIN)
  
  pvpHistoric <- getFile(inputDF$PVP_HISTORIC) |>
    validateHistoricPVP(inputDF$PVP_HISTORIC)
  
  pvpForecast <- getXLSX(inputDF$PVP_FORECAST,
                         range = cell_cols("A:N"), col_names = FALSE) |>
    validateForecastPVP(inputDF$PVP_FORECAST, endDate)
  
  
  # NOTE: "...#" will become column names in 'pvpHistoric' and 'pvpForecast' for
  # locations that lack a value in the spreadsheets' first non-empty row
  
  
  # Validate the two PVP input variables as well
  if (!(inputDF$PVP_VARIANCE[1] %in% c("Var", "NoVar"))) {
    
    paste0("Control File Value Issue\n\n", 
           "In the control file for the Russian River workflow, users must ",
           "specify the type of PVP forecast to use in the modeling ",
           "procedure. In \"PVP_FORECASTED_FLOWS_VARIANCE\", either ",
           "\"Var\" or \"NoVar\" must be specified. However, ",
           inputDF$PVP_VARIANCE[1], " was given. Please revise the control ",
           "file and try again.") |>
      errWrap() |>
      stop()
    
  }
  
  
  if (!(inputDF$PVP_WY_TYPE[1] %in% c("Similar", "Dry"))) {
    
    paste0("Control File Value Issue\n\n", 
           "In the control file for the Russian River workflow, users must ",
           "specify the type of PVP forecast to use in the modeling ",
           "procedure. In \"PVP_FORECASTED_FLOWS_WY_TYPE\", either ",
           "\"Similar\" or \"Dry\" must be specified. However, ",
           inputDF$PVP_WY_TYPE[1], " was given. Please revise the control ",
           "file and try again.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Perform one final extra check for 'mdtDF' using 'basinDF' next
  # Every value in the "BASIN" column of 'mdtDF' should appear in 
  # the "BASIN" column of 'basinDF'
  if (anyFalse(mdtDF$BASIN %in% basinDF$BASIN)) {
    
    paste0("Master Demand Table - Basin Column Issue\n\n", 
           "All values in the \"BASIN\" column of the Master Demand Table ",
           "should appear in the Basin Connectivity CSV file. However, ", 
           mdtDF$BASIN[!(mdtDF$BASIN %in% basinDF$BASIN)] |>
             unique() |> vec2QuotedStr(), " ",
           "appeared in the Master Demand Table despite not being in the ",
           "Basin CSV. Please investigate.\n\n",
           "(This error occurred for '", inputDF$MDT, "' and '", 
           inputDF$BASIN, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  
  cat("\tDone!\n\n")
  
  
  # Next, archive these inputs in the hydrology folder
  cat("[3/4]\tSaving inputs...\n")
  
  
  # Create a new tibble based on the filepaths in 'inputDF'
  # Replace the paths with new paths based on 'dirPath' 
  # (the actual filenames are still maintained though)
  newPathDF <- inputDF |>
    select(-PVP_VARIANCE, -PVP_WY_TYPE) |>
    mutate(across(everything(),
                  ~ paste0(dirPath, "/DWRAT/Input/",
                           . |> str_remove("^.+[/\\\\]")) |>
                    normalizePath(mustWork = FALSE)))
  
  
  # Use 'newPathDF' and write the input files to the model hydrology folder
  writeOutput(mdtDF, newPathDF$MDT[1])
  
  writeOutput(etDF, newPathDF$ET[1])
  
  writeOutput(basinDF, newPathDF$BASIN[1])
  
  writeOutput(pvpHistoric, newPathDF$PVP_HISTORIC[1])
  
  
  # The PVP forecast file will be an exception though
  # Its formatting is non-standard, so the original input will just be copied
  # directly into the DWRAT Input folder of 'dirPath'
  copyFile(from = inputDF$PVP_FORECAST[1],
           to = newPathDF$PVP_FORECAST[1])
  
  
  # Add information to the metadata file as well
  updateMetadataCSV(dirPath,
                    list("MASTER_DEMAND_TABLE" = inputDF$MDT[1],
                         "EVAPOTRANSPIRATION_CSV" = inputDF$ET[1],
                         "BASIN_CONNECTIVITY" = inputDF$BASIN[1],
                         "PVP_HISTORIC_DATA" = inputDF$PVP_HISTORIC[1],
                         "PVP_FORECAST_DATA" = inputDF$PVP_FORECAST[1],
                         "PVP_FORECAST_VARIANCE_TYPE" = 
                           inputDF$PVP_VARIANCE[1],
                         "PVP_FORECAST_WY_TYPE" = 
                           inputDF$PVP_WY_TYPE[1]))
  
  
  cat("\tDone!\n\n")
  
  
  # Finally, edit the Paradigm DWRAT script to point to these files
  # and prepare for the model run
  cat("[4/4]\tUpdating Paradigm DWRAT scripts...\n")
  
  
  # Perform updates to the Paradigm DWRAT scripts in another function
  updateScripts(dirPath, rawFlowsPath, inputDF, newPathDF)
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'RRW_018_Finalize_DWRAT_Inputs.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



setupInputPaths <- function (inputDF) {
  
  # Do initial processing of the inputs given in 'inputDF'
  # These values are required components for running Paradigm DWRAT
  
  # Initially, they contain raw values extracted from the control file
  
  # The file paths will be checked and processed into proper absolute paths
  
  # The folder paths will be validated as well, and a file will be extracted
  # from each of those directories
  
  
  # To start, for the five file/folder paths, check if they are SharePoint paths
  # Convert them if that is the case
  inputDF$MDT[1] <- inputDF$MDT[1] |>
    sharepointPathCheck(isFolder = FALSE)
  
  inputDF$ET[1] <- inputDF$ET[1] |>
    sharepointPathCheck(isFolder = FALSE)
  
  inputDF$BASIN[1] <- inputDF$BASIN[1] |>
    sharepointPathCheck(isFolder = FALSE)
  
  # inputDF$PVP_HISTORIC[1] <- inputDF$PVP_HISTORIC[1] |>
  #   sharepointPathCheck(isFolder = TRUE)
  # 
  # inputDF$PVP_FORECAST[1] <- inputDF$PVP_FORECAST[1] |>
  #   sharepointPathCheck(isFolder = TRUE)
  
  # The above commented-out code is not needed for the two PVP directories 
  # because the next step has `sharepointPathCheck` integrated into its 
  # procedure already
  
  
  # From the two PVP directories, extract a file
  # The script will attempt to choose the latest versions of the historic 
  # and forecasted PVP flows
  inputDF$PVP_HISTORIC[1] <- inputDF$PVP_HISTORIC[1] |>
    getLatestFile(filePattern = "PVP_Transfers_Observed",
                  title = "Historic PVP Flows File")
  
  
  inputDF$PVP_FORECAST[1] <- inputDF$PVP_FORECAST[1] |>
    getLatestFile(filePattern = "PotterValleyProjectProjection",
                  title = "Forecasted PVP Flows File")
  
  
  # Return 'inputDF' after this processing is complete
  return(inputDF)
  
}



validateMDT <- function (mdtDF, mdtPath) {
  
  # Validate the Russian River watershed's Master Demand Table
  
  # Check for a variety of issues and 
  # return 'mdtDF' if no errors are detected
  
  
  # Confirm that all required columns appear in 'mdtDF'
  reqCols <- c("APPLICATION_NUMBER",
               paste0(toupper(month.abb), "_MEAN_DIV"),
               "ASSIGNED_PRIORITY_DATE_SUB", "RIPARIAN", 
               "BASIN")
  
  
  missingCols <- which(!(reqCols %in% names(mdtDF)))
  
  
  # Output an error message if any columns are missing
  if (length(missingCols) > 0) {
    
    paste0("Master Demand Table - Missing Column Issue\n\n", 
           "Several columns in the Master Demand Table are required ",
           "for running DWRAT. However, ", length(missingCols), " key column",
           if_else(length(missingCols) > 1, "s", ""), " appear",
           if_else(length(missingCols) > 1, "", "s"), " to be missing (",
           vec2QuotedStr(reqCols[missingCols]), ").\n\n",
           "Please investigate and revise this file accordingly.\n\n",
           "(This error occurred for '", mdtPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # For the purposes of this modeling procedure, any "NA" values in 
  # monthly diversion columns should be set to zero
  mdtDF <- mdtDF |>
    mutate(across(contains("_MEAN_DIV"),
                  ~ if_else(is.na(.), 0, .)))
  
  
  # Check for missing values in other columns next
  # All of the other columns must contain values in every row
  if (anyNA(mdtDF[reqCols])) {
    
    # Among the required columns in 'mdtDF', get the names of 
    # columns that contain missing values
    missingCols <- mdtDF[reqCols] |>
      select(where(anyNA)) |>
      names()
    
    
    paste0("Master Demand Table - Missing Data Issue\n\n", 
           "Several columns in the Master Demand Table are required ",
           "for running DWRAT. They cannot contain \"NA\" entries. ",
           "However, ", length(missingCols), " key column",
           if_else(length(missingCols) > 1, "s are", " is"), " ",
           "missing values (", vec2QuotedStr(missingCols), ").\n\n",
           "Please investigate and revise this file accordingly.\n\n",
           "(This error occurred for '", mdtPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # After that, check that each value in "APPLICATION_NUMBER" is unique
  if (length(unique(mdtDF$APPLICATION_NUMBER)) != nrow(mdtDF)) {
    
    # Get the water right IDs that appear more than once in 'mdtDF'
    dupWR <- which(table(mdtDF$APPLICATION_NUMBER) > 1) |>
      names()
    
    
    paste0("Master Demand Table - Duplicate Water Right Issue\n\n", 
           "One row per water right is expected in the Master Demand ",
           "Table. However, ", length(dupWR), " water right",
           if_else(length(dupWR) > 1, "s have", " has"), " ",
           "multiple entries (", vec2QuotedStr(dupWR), ").\n\n",
           "Please investigate and revise this file accordingly.\n\n",
           "(This error occurred for '", mdtPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, confirm that each diversion column is numeric
  if (mdtDF |> select(contains("_MEAN_DIV")) |>
      map_lgl(is.numeric) |> anyFalse()) {
    
    # Get the names of the diversion columns that are not numeric
    nonNumericDiv <- mdtDF |>
      select(contains("_MEAN_DIV") & !where(is.numeric)) |>
      names()
    
    
    paste0("Master Demand Table - Non-Numeric Diversion Column Issue\n\n", 
           "Column types are assigned automatically based on the detected ",
           "values in the input file. Normally, diversion data should be ",
           "parsed as numeric values. However, that was not the case for ",
           "this file. Please investigate ", vec2QuotedStr(nonNumericDiv),
           ". The column", if_else(length(nonNumericDiv) > 1, "s", ""), " ",
           "may be completely empty, or ",
           if_else(length(nonNumericDiv) > 1, "their", "its"), " values ", 
           "may contain non-number-related characters.\n\n",
           "(This error occurred for '", mdtPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Ensure that the values in "ASSIGNED_PRIORITY_DATE_SUB" are numeric as well
  # If it is numeric, there should be no negative values and no decimals either
  if (!is.numeric(mdtDF$ASSIGNED_PRIORITY_DATE_SUB) ||
      any(mdtDF$ASSIGNED_PRIORITY_DATE_SUB < 0) ||
      any(mdtDF$ASSIGNED_PRIORITY_DATE_SUB != 
          round(mdtDF$ASSIGNED_PRIORITY_DATE_SUB))) {
    
    paste0("Master Demand Table - Priority Date Column Issue\n\n", 
           "The \"ASSIGNED_PRIORITY_DATE_SUB\" column should contain only ",
           "eight-digit numbers that correspond to priority dates ",
           "(in the format \"YYYYMMDD\"). However, that is not the case for ",
           "this file.\n\n",
           "Please investigate the column, and revise the file ",
           "accordingly.\n\n",
           "(This error occurred for '", mdtPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Ensure that the numbers in "ASSIGNED_PRIORITY_DATE_SUB" are preserved as
  # eight-digit strings
  mdtDF <- mdtDF |>
    mutate(ASSIGNED_PRIORITY_DATE_SUB = sprintf("%8d", 
                                                ASSIGNED_PRIORITY_DATE_SUB))
  
  # (This is relevant for cases like "10000000", which may get interpreted 
  #  with scientific notation as "1e+07")
  
  
  # Check the "RIPARIAN" column next
  # It should contain only "Y" or "N"
  if (anyFalse(mdtDF$RIPARIAN %in% c("Y", "N"))) {
    
    paste0("Master Demand Table - Riparian Column Issue\n\n", 
           "The \"RIPARIAN\" column should contain either \"Y\" or \"N\" ",
           "only. However, a different value was detected. Please ",
           "investigate the column, and revise the file accordingly.\n\n",
           "(This error occurred for '", mdtPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Finally, check the formatting of values in the "BASIN" column
  # All values should follow this basic format:
  #  (*) Starts with "R_"
  #  (*) Followed by two digits
  #  (*) Optionally ends with "_M"
  nonStandardValues <- mdtDF$BASIN |> 
    str_subset("^R_[0-9]{2}(_M)?$", negate = TRUE)
  
  
  # Raise an exception if non-standard values are found
  if (length(nonStandardValues) > 0) {
    
    paste0("Master Demand Table - Basin Column Issue\n\n", 
           "All values in the \"BASIN\" column of the Master Demand Table ",
           "should follow this format: \"R_##\", with \"_M\" sometimes ",
           "at the end. However, ", vec2QuotedStr(nonStandardValues), " ",
           "appeared in this file. Please investigate.\n\n",
           "(This error occurred for '", mdtPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return 'mdtDF' if there are no issues
  return(mdtDF)
  
}



validateET <- function (etDF, etPath) {
  
  # Validate the Russian River watershed's evapotranspiration input table
  
  # Return 'etDF' afterwards
  
  
  # Confirm that all required columns appear in 'etDF'
  reqCols <- c("Watershed", month.abb)
  
  
  missingCols <- which(!(reqCols %in% names(etDF)))
  
  
  # Output an error message if any columns are missing
  if (length(missingCols) > 0) {
    
    paste0("Evapotranspiration Table - Missing Column Issue\n\n", 
           "Several columns in the ET input CSV are required ",
           "for running DWRAT. However, ", length(missingCols), " key column",
           if_else(length(missingCols) > 1, "s", ""), " appear",
           if_else(length(missingCols) > 1, "", "s"), " to be missing (",
           vec2QuotedStr(reqCols[missingCols]), ").\n\n",
           "Please investigate and revise this file accordingly.\n\n",
           "(This error occurred for '", etPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Check for missing values in these columns next
  # All columns must contain values in every row
  if (anyNA(etDF)) {
    
    # Among the required columns in 'etDF', get the names of 
    # columns that contain missing values
    missingCols <- etDF |>
      select(where(anyNA)) |>
      names()
    
    
    paste0("Evapotranspiration Table - Missing Data Issue\n\n", 
           "Several columns in the ET input CSV are required ",
           "for running DWRAT. They cannot contain \"NA\" entries. ",
           "However, ", length(missingCols), " key column",
           if_else(length(missingCols) > 1, "s are", " is"), " ",
           "missing values (", vec2QuotedStr(missingCols), ").\n\n",
           "Please investigate and revise this file accordingly.\n\n",
           "(This error occurred for '", etPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, confirm that each month's column is numeric
  if (etDF |> select(all_of(month.abb)) |> map_lgl(is.numeric) |> anyFalse()) {
    
    # Get the names of the month columns that are not numeric
    nonNumericDiv <- etDF |>
      select(all_of(month.abb)) |>
      select(!where(is.numeric)) |>
      names()
    
    
    paste0("Evapotranspiration Table - Non-Numeric Column Issue\n\n", 
           "Column types are assigned automatically based on the detected ",
           "values in the input file. Normally, the ET data should be ",
           "parsed as numeric values. However, that was not the case for ",
           "this file. Please investigate ", vec2QuotedStr(nonNumericDiv),
           ". The column", if_else(length(nonNumericDiv) > 1, "s", ""), " ",
           "may be completely empty, or ",
           if_else(length(nonNumericDiv) > 1, "their", "its"), " values ", 
           "may contain non-number-related characters.\n\n",
           "(This error occurred for '", etPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Check the "Watershed" column next
  # It should contain only "URR" or "LRR"
  if (nrow(etDF) != 2 || anyFalse(etDF$Watershed %in% c("URR", "LRR"))) {
    
    paste0("Evapotranspiration Table - Watershed Column Issue\n\n", 
           "The \"Watershed\" column should contain either \"URR\" or \"LRR\" ",
           "only. As a result, it should have only two rows. However, that is ",
           "not the case. Please investigate the column, and revise the ",
           "file accordingly.\n\n",
           "(This error occurred for '", etPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return 'etDF' if there are no issues
  return(etDF)
  
}



validateBasins <- function (basinDF, basinPath, numBasins = 28) {
  
  # Validate the Russian River watershed's sub-basin connectivity CSV file
  
  # Check for a variety of issues and 
  # return 'basinDF' if no errors are detected
  
  
  # Confirm that all required columns appear in 'basinDF'
  reqCols <- c("BASIN", "FLOWS_TO", "MAINSTEM", "UPPER_RUSSIAN")
  
  
  missingCols <- which(!(reqCols %in% names(basinDF)))
  
  
  # Output an error message if any columns are missing
  if (length(missingCols) > 0) {
    
    paste0("Basin Connectivity Table - Missing Column Issue\n\n", 
           "Four columns in the basin CSV file are required ",
           "for running DWRAT. However, ", length(missingCols), " column",
           if_else(length(missingCols) > 1, "s", ""), " appear",
           if_else(length(missingCols) > 1, "", "s"), " to be missing (",
           vec2QuotedStr(reqCols[missingCols]), ").\n\n",
           "Please investigate and revise this file accordingly.\n\n",
           "(This error occurred for '", basinPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Check for missing values next
  # All columns must contain values in each row
  if (anyNA(basinDF)) {
    
    # Get the names of columns that contain missing values
    missingCols <- basinDF |>
      select(where(anyNA)) |>
      names()
    
    
    paste0("Basin Connectivity Table - Missing Data Issue\n\n", 
           "Four columns in the basin CSV file are required ",
           "for running DWRAT. They cannot contain \"NA\" entries. ",
           "However, ", length(missingCols), " column",
           if_else(length(missingCols) > 1, "s are", " is"), " ",
           "missing values (", vec2QuotedStr(missingCols), ").\n\n",
           "Please investigate and revise this file accordingly.\n\n",
           "(This error occurred for '", basinPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Focus on the "BASIN" and "FLOWS_TO" columns next
  # Make sure all values in both columns follow this basic format:
  #  (*) Starts with "R_"
  #  (*) Followed by two digits
  #  (*) Optionally ends with "_M"
  nonStandardValues <- c(basinDF$BASIN, basinDF$FLOWS_TO) |> 
    str_subset("^R_[0-9]{2}(_M)?$", negate = TRUE)
  
  
  # Raise an exception if non-standard values are found
  if (length(nonStandardValues) > 0) {
    
    paste0("Basin Connectivity Table - Basin Columns Issue\n\n", 
           "All values in the \"BASIN\" and \"FLOWS_TO\" columns ",
           "should follow this format: \"R_##\", with \"_M\" sometimes ",
           "at the end. However, ", vec2QuotedStr(nonStandardValues), " ",
           "appeared in this file. Please investigate.\n\n",
           "(This error occurred for '", basinPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, make sure that every basin number that appears in the two columns 
  # are valid basin numbers for the watershed
  basinNums <- c(basinDF$BASIN, basinDF$FLOWS_TO) |> 
    unique() |>
    str_extract("[0-9]{2}") |>
    as.numeric() |>
    unique()
  
  
  if (anyFalse(basinNums %in% 1:numBasins)) {
    
    paste0("Basin Connectivity Table - Basin Columns Issue\n\n", 
           "All values in the \"BASIN\" and \"FLOWS_TO\" columns ",
           "should contain a number that corresponds to one of the ",
           numBasins, " sub-basins in the watershed. However, that is not ",
           "the case. Please investigate.\n\n",
           "(This error occurred for '", basinPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Every basin in "FLOWS_TO" should also appear in "BASIN"
  if (anyFalse(basinDF$FLOWS_TO %in% basinDF$BASIN)) {
    
    paste0("Basin Connectivity Table - Basin Columns Issue\n\n", 
           "All values in the \"FLOWS_TO\" column should also appear ",
           "in the \"BASIN\" column. However, that is not the case for ",
           "this file. Please investigate.\n\n",
           "(This error occurred for '", basinPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, investigate the "MAINSTEM" and "UPPER_RUSSIAN" columns
  # They should contain only "Y" and "N" as values
  
  # Start with "MAINSTEM"
  if (anyFalse(basinDF$MAINSTEM %in% c("Y", "N"))) {
    
    paste0("Basin Connectivity Table - Mainstem Column Issue\n\n", 
           "The \"MAINSTEM\" column should only contain \"Y\" or \"N\" ",
           "as values in the basin CSV file. However, that is not the case ",
           "for this file. Please investigate.\n\n",
           "(This error occurred for '", basinPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Make sure the "Y" and "N" values are applied properly as well
  # Any "BASIN" value that contains "_M" should have "Y" for "MAINSTEM"
  basinDF <- basinDF |>
    mutate(MAINSTEM = if_else(grepl("_M$", BASIN), "Y", "N"))
  
  
  # Check "UPPER_RUSSIAN" next
  if (anyFalse(basinDF$UPPER_RUSSIAN %in% c("Y", "N"))) {
    
    paste0("Basin Connectivity Table - Mainstem Column Issue\n\n", 
           "The \"UPPER_RUSSIAN\" column should only contain \"Y\" or \"N\" ",
           "as values in the basin CSV file. However, that is not the case ",
           "for this file. Please investigate.\n\n",
           "(This error occurred for '", basinPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Make sure the "Y" and "N" values are applied properly as well
  # Any "BASIN" value that contains a number between 1 and 13 should have 
  # "Y" for "UPPER_RUSSIAN"
  basinDF <- basinDF |>
    mutate(UPPER_RUSSIAN = if_else(as.numeric(str_extract(BASIN, "[0-9]{2}")) 
                                   <= 13, "Y", "N"))
  
  
  # Return 'basinDF' if there are no issues
  return(basinDF)
  
}



validateHistoricPVP <- function (pvpHistoric, pvpPath) {
  
  # Validate the Russian River watershed's historic PVP flows spreadsheet
  
  # Check for a variety of issues and 
  # return 'pvpHistoric' if no errors are detected
  
  
  # Confirm that all required columns appear in 'mdtDF'
  reqCols <- c("...1",
               "Lake Mendocino Inflow (cfs)")
  
  
  missingCols <- which(!(reqCols %in% names(pvpHistoric)))
  
  
  # Output an error message if any columns are missing
  if (length(missingCols) > 0) {
    
    paste0("Historic PVP Flows File - Missing Column Issue\n\n", 
           "Two columns in the historic PVP flows spreadsheet are required ",
           "for running DWRAT. However, ", length(missingCols), " key column",
           if_else(length(missingCols) > 1, "s", ""), " appear",
           if_else(length(missingCols) > 1, "", "s"), " to be missing (",
           vec2QuotedStr(reqCols[missingCols]), ").\n\n",
           "Please investigate and revise this file accordingly.\n\n",
           "(This error occurred for '", pvpPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Check for missing values in these columns next
  if (anyNA(pvpHistoric[reqCols])) {
    
    # Among the required columns in 'pvpHistoric', get the names of 
    # columns that contain missing values
    missingCols <- pvpHistoric[reqCols] |>
      select(where(anyNA)) |>
      names()
    
    
    paste0("Historic PVP Flows File - Missing Data Issue\n\n", 
           "Two columns in the historic PVP flows spreadsheet are required ",
           "for running DWRAT. They cannot contain \"NA\" entries. ",
           "However, ", length(missingCols), " key column",
           if_else(length(missingCols) > 1, "s are", " is"), " ",
           "missing values (", vec2QuotedStr(missingCols), ").\n\n",
           "Please investigate and revise this file accordingly.\n\n",
           "(This error occurred for '", pvpPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # After that, if the date column ("...1") contains datetimes,
  # convert those values into plain dates
  if ("POSIXct" %in% class(pvpHistoric$...1) ||
      "POSIXt" %in% class(pvpHistoric$...1)) {
    
    pvpHistoric <- pvpHistoric |>
      mutate(...1 = as.Date(...1))
    
  }
  
  
  # Then, confirm that the date column ("...1") contains dates only
  if (!is.Date(pvpHistoric$...1) || anyNA(pvpHistoric$...1)) {
    
    paste0("Historic PVP Flows File - Date Column Issue\n\n", 
           "The unnamed date column in the historic PVP flows file could ",
           "not be parsed correctly. It should only contain dates, but ",
           "its values were parsed differently. ",
           "Two columns in the historic PVP flows spreadsheet are required ",
           "for running DWRAT. However, ", length(missingCols), " key column",
           if_else(length(missingCols) > 1, "s", ""), " appear",
           if_else(length(missingCols) > 1, "", "s"), " to be missing (",
           vec2QuotedStr(reqCols[missingCols]), ").\n\n",
           "Please investigate and revise this file accordingly.\n\n",
           "(This error occurred for '", pvpPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # After that, check that each date in "...1" is unique
  if (length(unique(pvpHistoric$...1)) != nrow(pvpHistoric)) {
    
    # Get the water right IDs that appear more than once in 'mdtDF'
    dupDates <- which(table(pvpHistoric$...1) > 1) |>
      names()
    
    
    paste0("Historic PVP Flows File - Duplicate Date Issue\n\n", 
           "One row per date is expected in the historic PVP flows CSV ",
           "file However, ", length(dupDates), " date",
           if_else(length(dupDates) > 1, "s have", " has"), " ",
           "multiple entries (", vec2QuotedStr(dupDates), ").\n\n",
           "Please investigate and revise this file accordingly.\n\n",
           "(This error occurred for '", pvpPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, confirm that the Lake Mendocino column is numeric
  if (!is.numeric(pvpHistoric$`Lake Mendocino Inflow (cfs)`)) {
    
    paste0("Historic PVP Flows File - Non-Numeric Column Issue\n\n", 
           "Column types are assigned automatically based on the detected ",
           "values in the input file. Normally, the \"Lake Mendocino Inflow ",
           "(cfs)\" column should be parsed as numeric. However, that was ",
           "not the case for this file. This column may be empty, or it may ",
           "contain non-number-related characters. Please investigate the ",
           "file.\n\n",
           "(This error occurred for '", pvpPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there are any negative flow values in the "Lake Mendocino" column, 
  # replace them with zero
  negativeIndices <- which(pvpHistoric$`Lake Mendocino Inflow (cfs)` < 0)
  
  
  if (length(negativeIndices) > 0) {
    
    pvpHistoric$`Lake Mendocino Inflow (cfs)`[negativeIndices] <- 0
    
  }
  
  
  # Finally, rename "...1" to "Date" and return the tibble
  return(pvpHistoric |>
           rename(Date = ...1))
  
}



validateForecastPVP <- function (pvpForecast, pvpPath, endDate) {
  
  # Validate the Russian River watershed's PVP forecast spreadsheet
  
  # Check for different issues and return 'pvpForecast' 
  # if no errors are detected
  
  # The checks in this function are somewhat more vague compared to the other
  # data validation functions
  
  # This is due to the irregular formatting of the spreadsheet
  
  
  # First confirm that the spreadsheet contains at least 14 columns
  if (ncol(pvpForecast) < 14) {
    
    paste0("PVP Forecasted Flows File - Insufficient Data\n\n", 
           "The forecast file is expected to contain values up to Column ",
           "\"N\" of the spreadsheet. Please investigate and revise ",
           "this file accordingly.\n\n",
           "(This error occurred for '", pvpPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The second column should contain only dates or datetime values
  if (!is.Date(pvpForecast$...2) && !("POSIXct" %in% class(pvpForecast$...2)) &&
      !any(grepl("^[0-9]{4}[/\\-][0-9]{2}[/\\-][0-9]{2}$", pvpForecast$...2) |
           grepl("^[0-9]{2}[/\\-][0-9]{2}[/\\-][0-9]{4}$", pvpForecast$...2))) {
    
    # The required conditions for this error message are:
    #   (1) The second column does NOT have the "date" type
    #   (2) The second column does NOT have the "datetime" type
    #   (3) The second column does NOT have one of four formats:
    #         (*) yyyy-mm-dd
    #         (*) yyyy/mm/dd
    #         (*) mm-dd-yyyy
    #         (*) mm/dd/yyyy
    # (All three conditions must be TRUE)
    
    paste0("PVP Forecasted Flows File - Date Issue\n\n", 
           "The file containing PVP flow forecasts is expected to have ",
           "dates in its second column (Column \"B\"). Please investigate ",
           "and revise this spreadsheet accordingly.\n\n",
           "(This error occurred for '", pvpPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # For the next checks, find the first non-empty row in the date column
  valStart <- which(!is.na(pvpForecast$...2)) |>
    min()
  
  # In other columns, there should be numeric values from 'valStart' onwards
  
  
  # First confirm that no "NA" values appear in the date column after 'valStart'
  if (anyNA(pvpForecast[valStart:nrow(pvpForecast), ]$...2)) {
    
    paste0("PVP Forecasted Flows File - Missing Date Issue\n\n", 
           "The file containing PVP flow forecasts is expected to have ",
           "a continuous stream of dates starting from Row ", valStart, " ",
           "in its second column (Column \"B\"). Please investigate ",
           "and make revisions accordingly.\n\n",
           "(This error occurred for '", pvpPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Check for a continuous stream of dates next
  # For this check, the second column in 'pvpForecast' needs
  # to be the "date" or "datetime" type
  if (!is.Date(pvpForecast$...2) && !("POSIXct" %in% class(pvpForecast$...2))) {
    
    # Check the formatting of the dates and apply `as.Date` accordingly
    
    if (grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", pvpForecast$...2[valStart])) {
      
      dateFormat <- "%Y-%m-%d"
      
    } else if (grepl("[0-9]{4}/[0-9]{2}/[0-9]{2}", pvpForecast$...2[valStart])) {
      
      dateFormat <- "%Y/%m/%d"
      
    } else if (grepl("[0-9]{2}/[0-9]{2}/[0-9]{4}", pvpForecast$...2[valStart])) {
      
      dateFormat <- "%m/%d/%Y"
      
    } else if (grepl("[0-9]{2}-[0-9]{2}-[0-9]{4}", pvpForecast$...2[valStart])) {
      
      dateFormat <- "%m-%d-%Y"
      
    } else {
      
      paste0("PVP Forecasted Flows File - Unknown Date Format\n\n", 
             "The file containing PVP flow forecasts is expected to have ",
             "a continuous stream of dates starting from Row ", valStart, " ",
             "in its second column (Column \"B\"). However, the column's ",
             "values could not be parsed as dates. Please investigate.\n\n",
             "(This error occurred for '", pvpPath, "')") |>
        errWrap() |>
        stop()
      
    }

    
    # Apply 'dateFormat' to 'pvpForecast'
    pvpForecast$...2 <- pvpForecast$...2 |>
      as.Date(format = dateFormat)
    
  }
  
  
  # Now that the second column in 'pvpForecast' contains dates,
  # check for missing dates between the start and end dates
  dateVec <- seq(from = min(pvpForecast$...2, na.rm = TRUE),
                 to = max(pvpForecast$...2, na.rm = TRUE),
                 by = "days")
  
  
  missingDates <- dateVec[!(dateVec %in% pvpForecast$...2)]
  
  
  if (length(missingDates) > 0) {
    
    paste0("PVP Forecasted Flows File - Missing Date Issue\n\n", 
           "The file containing PVP flow forecasts is expected to have ",
           "a continuous stream of dates starting from Row ", valStart, " ",
           "in its second column (Column \"B\"). However, ",
           length(missingDates), " date", 
           if_else(length(missingDates) > 1, "s are", " is"), " missing (", 
           vec2QuotedStr(missingDates), "). Please investigate.\n\n",
           "(This error occurred for '", pvpPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Check if the last forecast day comes before 'endDate'
  # That's a sign that the forecast spreadsheet must be updated
  if (max(pvpForecast$...2, na.rm = TRUE) < endDate) {
    
    # Prepare the output message
    # It may be either an error message or warning message
    outStr <- paste0("PVP Forecasted Flows - Outdated File Issue\n\n", 
                     "The file containing PVP flow forecasts ends at ",
                     max(pvpForecast$...2, na.rm = TRUE), ". However, the final ",
                     "date of the data scraping bounds is ", endDate, ". ",
                     "This means that the forecast spreadsheet is outdated. ",
                     "Please obtain an updated copy.\n\n",
                     "(This occurred for '", pvpPath, "')") |>
      errWrap()
    
    
    # Check the control spreadsheet and decide whether 'outStr' should be
    # an error message
    if (getFromControl_RR("PVP_FORECASTED_FLOWS_OUTOFDATE_ERROR_TOGGLE") |>
        trimws() |> toupper() %in% c("T", "TRUE", "YES")) {
      
      # If "PVP_FORECASTED_FLOWS_OUTOFDATE_ERROR_TOGGLE" contains something
      # similar to "TRUE", output 'outStr' as an error message
      stop(outStr)
      
    } else {
      
      # Otherwise, use the string in a message only
      cat("\n\n")
      message(outStr)
      cat("\n\n")
      
    }
    
  }
  
  
  
  # After that, use 'valStart' and check for "NA" or non-numeric values
  # in all subsequent columns
  if (pvpForecast[valStart:nrow(pvpForecast), 3:ncol(pvpForecast)] |>
      mutate(across(everything(), as.numeric)) |>
      anyNA()) {
    
    paste0("PVP Forecasted Flows File - Data Issue\n\n", 
           "The file containing PVP flow forecasts is expected to have ",
           "numeric data in Columns \"C\" through \"N\" once dates in ",
           "Column \"B\" start appearing. Please investigate and make ",
           "revisions accordingly.\n\n",
           "(This error occurred for '", pvpPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Finally, check Columns 11 through 14
  # Specifically, look at the two rows before 'valStart'
  
  # Two rows above 'valStart', the values in these four columns should be:
  #   (11) "Variance Start ..."
  #   (12) "Variance Start ..."
  #   (13) "No Variance..."
  #   (14) "No Variance..."
  if (!grepl("^\\s*Variance Start.*$", pvpForecast$...11[valStart - 2], 
             ignore.case = TRUE) ||
      !grepl("^\\s*Variance Start.*$", pvpForecast$...12[valStart - 2], 
             ignore.case = TRUE) ||
      !grepl("^.*No Variance.*$", pvpForecast$...13[valStart - 2], 
             ignore.case = TRUE) ||
      !grepl("^\\s*No Variance.*$", pvpForecast$...14[valStart - 2], 
             ignore.case = TRUE)) {
    
    paste0("PVP Forecasted Flows File - Variance Columns Issue\n\n", 
           "The file containing PVP flow forecasts is expected to have ",
           "predictions with and without flow variance in Columns \"K\" ",
           "through \"N\".\n\n",
           "\"K\" and \"L\" should have variance incorporated into their ",
           "estimates, while \"M\" and \"N\" should have no variance ",
           "included in their predicted flows.\n\n",
           "These differences should be made clear in Row ", valStart - 2, " ",
           "of the spreadsheet. However, the label text appears to have ",
           "changed now. Please investigate and make revisions as needed.\n\n",
           "(This error occurred for '", pvpPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # After that, look at the row above 'valStart'
  # The values in these four columns should be:
  #   (11) "Similar... Year ..."
  #   (12) "Dry ... Year ..."
  #   (13) "Similar... Year ..."
  #   (14) "Dry ... Year ..."
  if (!grepl("^\\s*Similar Hydro.* Year.*$", pvpForecast$...11[valStart - 1], 
             ignore.case = TRUE) ||
      !grepl("^\\s*Dry Hydro.* Year.*$", pvpForecast$...12[valStart - 1], 
             ignore.case = TRUE) ||
      !grepl("^\\s*Similar Hydro.* Year.*$", pvpForecast$...13[valStart - 1], 
             ignore.case = TRUE) ||
      !grepl("^\\s*Dry Hydro.* Year.*$", pvpForecast$...14[valStart - 1], 
             ignore.case = TRUE)) {
    
    paste0("PVP Forecasted Flows File - WY Type Columns Issue\n\n", 
           "The file containing PVP flow forecasts is expected to have ",
           "predictions based on dry and similar hydrologic water years ",
           " in Columns \"K\" through \"N\".\n\n",
           "\"K\" and \"M\" should use a similar water year as the basis of ",
           "their forecasts, while \"L\" and \"N\" should rely on a dry ",
           "water year in their predicted flows.\n\n",
           "These differences should be made clear in Row ", valStart - 1, " ",
           "of the spreadsheet. However, the label text appears to have ",
           "changed now. Please investigate and make revisions as needed.\n\n",
           "(This error occurred for '", pvpPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there are no issues, return 'pvpForecast'
  return(pvpForecast)
  
}



updateScripts <- function (dirPath, rawFlowsPath, inputDF, newPathDF) {
  
  # Update the Paradigm DWRAT scripts to reflect the filepaths and
  # configurations specified by the user for this model run
  
  
  # First, double-check that the scripts exist as expected
  scriptPaths <- tibble("RR_CONNECTED" = 
                          "../Paradigm_DWRAT/RR_Connected.py",
                        "PVP_PROCESSOR" = 
                          "../Paradigm_DWRAT/dwrat/preprocessing/PVP_Processor.py") |>
    mutate(across(everything(), ~ normalizePath(., mustWork = FALSE)))
  
  
  if (scriptPaths |> unlist(use.names = FALSE) |> file.exists() |> anyFalse()) {
    
    paste0("Paradigm DWRAT Scripts - Missing Scripts Issue\n\n", 
           "The SDA DWRAT_DataScraping repository should contain a ",
           "\"Paradigm_DWRAT\" folder that is adjacent to the \"Supply\" ",
           "folder. Inside that folder should be various Python scripts. ",
           "This script could not locate all of the required DWRAT scripts ",
           "for this operation. Please investigate and make adjustments as ",
           "needed.\n\n",
           "The required scripts are: ", vec2QuotedStr(scriptPaths)) |>
      errWrap() |>
      stop()
    
  }
  
  
  # Then, perform updates to each of the scripts in 'scriptPaths'
  
  
  #### Start with "RR_Connected.py" ####
  
  # This script requires several edits: 
  
  #   (*) Update "supply_file" with the path to the raw supply flows in 'dirPath'
  #   (*) Update "demand_file" with the path to the Master Demand Table
  #   (*) Update "basin_file" with the path to the basin connectivity CSV file
  
  #   (*) Set an output path for the "URR" config file
  #   (*) Set an output path for the "LRR" config file
  
  #   (*) Update "LakeMendoBalance_FileLocation" with the path to the historic
  #       PVP flows file
  #   (*) Update "SCWAForecast_FileLocation" with the path to the PVP forecasted
  #       flows file
  
  #   (*) Update "Variance" with the user's selection for 
  #       "PVP_FORECASTED_FLOWS_VARIANCE"
  #   (*) Update "SimilarDry" with the user's selection for 
  #       "PVP_FORECASTED_FLOWS_WY_TYPE"
  
  #   (*) Update "modelName" in the "upperModel" model input variable based
  #       on parameters related to the model run
  #   (*) Update "modelName" in the "lowerModel" model input variable based
  #       on parameters related to the model run
  
  #   (*) Update "outputPath" to point to the DWRAT Output folder in 'dirPath'
  #   (*) Update "name" in the combined output function to point to the same 
  #       DWRAT Output folder in 'dirPath' as well
  
  pyScript <- getFile(scriptPaths$RR_CONNECTED[1], fileType = "OTHER")
  
  
  # First update 'supply_file' with 'rawFlowsPath'
  pyScript <- pyScript |>
    updateLine("^supply_file = ", 
               paste0("supply_file = '",
                      rawFlowsPath |> normalizePath(winslash = "/"),
                      "'"),
               scriptPaths$RR_CONNECTED[1])
  
  
  # Then, use the path to the Master Demand Table to define "demand_file"
  pyScript <- pyScript |>
    updateLine("^demand_file = ", 
               paste0("demand_file = '",
                      newPathDF$MDT[1] |> normalizePath(winslash = "/"),
                      "'"),
               scriptPaths$RR_CONNECTED[1])
  
  
  # Update "basin_file" with the path to the basin connectivity file
  pyScript <- pyScript |>
    updateLine("^basin_file = ", 
               paste0("basin_file = '",
                      newPathDF$BASIN[1] |> normalizePath(winslash = "/"),
                      "'"),
               scriptPaths$RR_CONNECTED[1])
  
  
  # Set a path in the hydrology folder for the "URR" config file that 
  # will be generated by Paradigm DWRAT
  pyScript <- pyScript |>
    updateLine("^urr_config_file = ", 
               paste0("urr_config_file = '",
                      paste0(dirPath, "/DWRAT/Input/urr_config_file.csv") |> 
                        normalizePath(winslash = "/", mustWork = FALSE),
                      "'"),
               scriptPaths$RR_CONNECTED[1])
  
  
  # Set a path in the hydrology folder to store the "LRR" config file
  pyScript <- pyScript |>
    updateLine("^lrr_config_file = ", 
               paste0("lrr_config_file = '",
                      paste0(dirPath, "/DWRAT/Input/lrr_config_file.csv") |> 
                        normalizePath(winslash = "/", mustWork = FALSE),
                      "'"),
               scriptPaths$RR_CONNECTED[1])
  
  
  # Implement PVP-related updates next
  # Set "LakeMendoBalance_FileLocation" to the filepath for historic PVP flows
  pyScript <- pyScript |>
    updateLine("^LakeMendoBalance_FileLocation = ", 
               paste0("LakeMendoBalance_FileLocation = '",
                      newPathDF$PVP_HISTORIC[1] |> normalizePath(winslash = "/"),
                      "'"),
               scriptPaths$RR_CONNECTED[1])
  
  
  # Update "SCWAForecast_FileLocation" with the path to the 
  # PVP forecasted flows spreadsheet
  pyScript <- pyScript |>
    updateLine("^SCWAForecast_FileLocation = ", 
               paste0("SCWAForecast_FileLocation = '",
                      newPathDF$PVP_FORECAST[1] |> normalizePath(winslash = "/"),
                      "'"),
               scriptPaths$RR_CONNECTED[1])
  
  
  # Replace the string input for 'Variance' with the user's selection for
  # "PVP_FORECASTED_FLOWS_VARIANCE"
  pyScript <- pyScript |>
    updateLine("^Variance = ", 
               paste0("Variance = '", inputDF$PVP_VARIANCE[1], "'"),
               scriptPaths$RR_CONNECTED[1], 
               preserveComment = TRUE)
  
  
  # Similarly, update "SimilarDry" with the value specified in 
  # "PVP_FORECASTED_FLOWS_WY_TYPE"
  pyScript <- pyScript |>
    updateLine("^SimilarDry = ", 
               paste0("SimilarDry = '", inputDF$PVP_WY_TYPE[1], "'"),
               scriptPaths$RR_CONNECTED[1], 
               preserveComment = TRUE)
  
  
  # For the Upper Russian River model, reference the sub-basins and model type
  # in the model name
  pyScript <- pyScript |>
    updateLine("^\\s*modelName\\s?=\\s?.URR", 
               "modelName = 'URR_Connected', ",
               scriptPaths$RR_CONNECTED[1])
  
  
  # For the Lower Russian River model, reference the LRR sub-basins and the 
  # model type in the model name
  pyScript <- pyScript |>
    updateLine("^\\s*modelName\\s?=\\s?.LRR", 
               "modelName = 'LRR_Connected', ",
               scriptPaths$RR_CONNECTED[1])
  
  
  # Update the output folder locations too
  # (They will point to the hydrology folder's DWRAT Output folder)
  
  # The first replacement will still use "os.path.join" and reference the 
  # URR and LRR model names defined under "modelName" above
  # (This line is part of an iterative loop)
  pyScript <- pyScript |>
    updateLine("^\\s*outputPath = ", 
               paste0("outputPath = os.path.join('",
                      paste0(dirPath, "/DWRAT/Output") |> 
                        normalizePath(winslash = "/"),
                      "', model.name)"),
               scriptPaths$RR_CONNECTED[1],
               preserveSpacing = TRUE)
  
  
  # The second edit simply points to the DWRAT Output folder in 'dirPath'
  pyScript <- pyScript |>
    updateLine("^\\s*name\\s*=\\s*", 
               paste0("name = '",
                      paste0(dirPath, "/DWRAT/Output") |> 
                        normalizePath(winslash = "/"),
                      "'"),
               scriptPaths$RR_CONNECTED[1],
               preserveSpacing = TRUE)
  
  
  # Write 'pyScript' back to a file
  writeOutput(pyScript, scriptPaths$RR_CONNECTED[1], quietly = TRUE)
  
  
  #### Edit "PVP_Processor.R" next ####
  
  # This script requires only a few edits: 
  
  #   (*) Update "writePath" for the Calpella gage data to point to the 
  #       DWRAT Input folder
  
  #   (*) Update "ET_xlsx_location" to reference the ET spreadsheet filepath
  
  pyScript <- getFile(scriptPaths$PVP_PROCESSOR[1], fileType = "OTHER")
  
  
  # First update 'writePath' with a path in 'dirPath'
  pyScript <- pyScript |>
    updateLine("^\\s*writePath = ", 
               paste0("writePath = '",
                      paste0(dirPath, "/DWRAT/Input/Calpella_Gage_",
                             format(Sys.Date(), "%Y%m%d"), ".csv") |>
                        normalizePath(winslash = "/", mustWork = FALSE),
                      "'"),
               scriptPaths$PVP_PROCESSOR[1])
  
  
  # Then, update "ET_xlsx_location" 
  pyScript <- pyScript |>
    updateLine("^\\s*ET_xlsx_location = ", 
               paste0("ET_xlsx_location = '",
                      newPathDF$ET[1] |> normalizePath(winslash = "/"),
                      "'"),
               scriptPaths$PVP_PROCESSOR[1])
  
  
  # Write 'pyScript' back to a file
  writeOutput(pyScript, scriptPaths$PVP_PROCESSOR[1], quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



updateLine <- function (pyScript, idRegex, replaceStr, scriptPath, 
                        preserveComment = FALSE, preserveSpacing = TRUE) {
  
  # Update a line of code in 'pyScript' (a text-based vector)
  # (generally one that defines a variable or argument)
  
  # Replace the line chosen by 'idRegex' with 'replaceStr'
  
  # Additional checks are necessary to ensure that extra closing
  # parentheses ")" are closed
  
  # Similarly, if a definition on a line continues onto subsequent lines, 
  # those extra lines must be removed too
  
  # And 'preserveComment' is used if a line containing a comment 
  # should include the comment after 'replaceStr'
  
  # 'preserveSpacing' has a similar purpose since spacing is so crucial for Python
  # The initial spacing at the beginning of the original code line is copied 
  # to the start of the replacement line when this variable is 'TRUE'
  
  
  # Start by searching for 'idRegex' 
  # It should identify a single line in 'pyScript'
  editLoc <- grep(idRegex, pyScript)
  
  
  # Make sure that exactly one match was found using 'idRegex'
  if (length(editLoc) > 1) {
    
    paste0("Multiple Matches Found in Python Script\n\n", 
           "The script attempted to update a single line of code in a Python ",
           "script. However, the regular expression \"", idRegex, "\" yielded ",
           length(editLoc), " matches.\n\n",
           "The intended rewrite was \"", replaceStr, "\". Please ",
           "investigate the scripts.\n\n",
           "(This error occurred for '", scriptPath, "')") |>
      errWrap() |>
      stop()
    
  } else if (length(editLoc) == 0) {
    
    paste0("Could Not Find Line in Python Script\n\n", 
           "The script attempted to update a single line of code in a Python ",
           "script. However, the regular expression \"", idRegex, "\" yielded ",
           "no matches.\n\n",
           "The intended rewrite was \"", replaceStr, "\". Please ",
           "investigate the scripts.\n\n",
           "(This error occurred for '", scriptPath, "')") |>
      errWrap() |>
      stop()
    
  }
  
  
  # After that, check if subsequent lines are a continuation of 
  # defining that variable
  extraLines <- continuityCheck(pyScript, editLoc, scriptPath)
  
  
  # If additional lines continue the code initiated on 'editLoc', 
  # remove them from 'pyScript'
  # ('replaceStr' will be specified on one line only)
  if (!is.null(extraLines)) {
    
    pyScript <- pyScript[-extraLines]
    
  }
  
  
  # Next, check if additional closing parentheses are needed after 'replaceStr'
  # If the variable definition or function argument is part of a larger 
  # assignment, there may be additional closing parentheses that need to be
  # accounted for
  if (netCount(pyScript[editLoc], "\\)") > netCount(pyScript[editLoc], "\\(")) {
    
    # If the line of code contains MORE ")" than "(" on that line, 
    # record the number of excess closing parentheses
    extraClosing <- str_dup(")", 
                            netCount(pyScript[editLoc], "\\)") -
                              netCount(pyScript[editLoc], "\\("))
    
    # These extra ")" will be added to the end of 'replaceStr'
    
  } else {
    
    # If no excess ")" are present, make 'extraClosing' an empty string
    extraClosing <- ""
    
  }
  
  
  # Then, if 'preserveComment' is TRUE, extract a comment from the line 
  # of 'pyScript' indicated by 'editLoc'
  if (preserveComment) {
    
    commentStr <- paste0(" #",
                         pyScript[editLoc] |>
                           str_extract("(?<=\\s?#) .+$"))
    
    # Note: A positive look-behind regex is used for "#"
    #       This means that "#" is used for pattern-matching, 
    #       but it does NOT appear in the result of `str_extract`
    
    #       This is intentional in case `str_extract` returns "NA"
    #       (That happens when 'preserveComment' is TRUE, but the line contains
    #        no comment--so the extracted string is just "NA")
    
    #       With the "#" always included by default in 'commentStr', even if 
    #       the extracted string is "NA", there will be no runtime error when
    #       executing DWRAT
    
  } else {
    
    commentStr <- ""
    
  }
  
  
  # And if 'preserveSpacing' is TRUE, the initial spaces at the beginning 
  # of 'pyScript[editLoc]' are saved in a variable and reused for the new line
  if (preserveSpacing) {
    
    # Initial spaces in the original line are extracted into 'spaceStr' 
    # (If there are no spaces at the beginning, the returned result will be 
    #  the empty string "")
    spaceStr <- pyScript[editLoc] |>
      str_extract("^\\s*")
    
  } else {
    
    spaceStr <- ""
    
  }
  
  
  # Finally, update 'editLoc' in 'pyScript' with the revised information
  pyScript[editLoc] <- paste0(spaceStr, replaceStr, extraClosing, commentStr)
  
  
  # Return 'pyScript' afterwards
  return(pyScript)
  
}



netCount <- function (string, pattern) {
  
  # Use `str_count` to count instances of 'pattern' in 'string'
  
  # However, if 'pattern' appears after a comment hashtag "#" or within quotes, 
  # exclude instances of 'pattern' that come after "#" or within quotes (" or ')
  
  
  # If "#" appears in 'string' (and it's not part of a quoted string),
  # remove that portion of 'string' for these checks
  if (grepl("#", string)) {
    
    # The intention is to remove comments only (which start with "#")
    
    # The main complicating factor is that "#" can also appear in strings
    # (e.g., in filenames)
    
    # Check for several cases and apply different regular expressions 
    
    
    # In the simplest case, there are no quotation marks to worry about
    if (grepl("^[^'\"]*#", string)) {
      
      # In that case, just remove everything that comes after "#"
      # (And then remove "#" as well)
      string <- string |>
        str_extract("^[^'\"]*#") |>
        str_remove("#")
      
      
    # Even if quotation marks are present, if none of them follow "#"
    # Then the comment can be safely removed
    } else if (grepl("^.*#[^'\"]*$", string)) {
      
      # Keep only the portion of 'string' that appears before the 
      # comment "#"
      string <- string |>
        str_replace("^(.*)#[^'\"]*$", "\\1")
      
      # There can be other "#" in this string (matched by ".*"), 
      # but only the "#" that seems to lead a comment is 
      # matched by "#" in the regex
      
      # The limitation of this regex, though, is that no quotation marks
      # can appear within the comment string
      
      # The next two checks will allow comments to have quotes in them
      # However, they have to be either single quote only or double quote only
      
    # If a string contains single quotes (and no double quotes),
    # check for "#" that do not appear between quotes
    } else if (grepl("'", string) && !grepl("\"", string) &&
               grepl("^[^']*([^']*'[^']*'[^']*)*[^']*#", string)) {
      
      # The regex looks complicated, but the main portion to focus on 
      # is "([^']*'[^']*'[^']*)*"
      
      # This group pattern matches strings that are encased in single quotes
      # (With optional non-single-quote characters able to appear before and 
      #  after the opening and closing of the single quotes)
      
      # Any "#" that appears within quotes will count as 
      # part of that group pattern
      
      # So the "#" at the end of the regex should belong to a comment
      
      string <- string |>
        str_extract("^[^']*([^']*'[^']*'[^']*)*[^']*#") |>
        str_remove("#")
      
      # Here's a more thorough breakdown of the regex:
      
      # "^[^']*([^']*'[^']*'[^']*)*[^']*#"
      
      #  (1) Start looking from the beginning of the string
      
      #  (2) Optionally starts with 0 or more non-single-quote characters
      
      #  (3) Optionally contains 0 or more instances of this group pattern:
      #       (a) Optionally starts with 0 or more non-single-quote characters
      #       (b) A single quote '
      #       (c) Optionally contains 0 or more non-single-quote characters
      #       (d) A single quote '
      #       (e) Optionally followed by 0 or more non-single-quote characters
      
      #  (4) Optionally followed by 0 or more non-single-quote characters
      
      #  (5) A "#"
      
      
    # Repeat the same procedure for instances where the string contains
    # double quotes, but no single quotes
    } else if (grepl("\"", string) && !grepl("'", string) &&
               grepl("^[^\"]*([^\"]*\"[^\"]*\"[^\"]*)*[^\"]*#", string)) {
      
      string <- string |>
        str_extract("^[^\"]*([^\"]*\"[^\"]*\"[^\"]*)*[^\"]*#") |>
        str_remove("#")
      
      # This regex is essentially the same as the previously described one
      # Just switch single quotes with double quotes
      
      
      # The previous regular expressions cover most target scenarios:
      
      # No Quotes                                        [First Check]
      
      # Single Quotes Before # Only                      [Second Check] 
      # Single Quotes After # Only                       [First Check] 
      # Single Quotes Before & After # Only              [Third Check]
      
      # Double Quotes Before # Only                      [Second Check] 
      # Double Quotes After # Only                       [First Check] 
      # Double Quotes Before & After # Only              [Fourth Check]
      
      # Single &/OR Double Quotes Before # Only          [Second Check] 
      # Single &/OR Double Quotes After # Only           [First Check] 
      # Single &/OR Double Quotes Before & After # Only  [???]
      
      
      # The next check is for strings that have both single and double quotes 
      # In addition, to reach this point, the string should have single and/or 
      # double quotes before AND after the "#"
      # (To make sure this is the worth the effort, the function also confirms
      #  whether 'pattern' may even be present after a "#")
    } else if (grepl("'", string) && grepl("\"", string) &&
               grepl(paste0("#.*", pattern), string)) {
      
      paste0("Rare Case Issue\n\n",
             "The script was not designed to handle this unusual border case. ",
             "Please investigate the procedure for excluding comments (denoted ",
             "by \"#\") from pattern counts.\n\n",
             "This error occurred when attempting to find instances of \"",
             pattern, "\" within \"", string, "\".") |>
        errWrap() |>
        stop()
      
    }
    
  }
  
  
  # If 'pattern' appears after double quotes, use the modified count formula
  if (grepl(paste0("\".*", pattern, ".*\""), string)) {
    
    # Count the number of instances of 'pattern' in 'string' and 
    # exclude the number of instances of 'pattern' contained within double quotes
    count <- str_count(string, pattern) - 
      str_count(string |> str_extract(paste0("\".*", pattern, ".*\"")), 
                pattern)
    
  # Repeat the modification for single quotes
  } else if (grepl(paste0("'.*", pattern, ".*'"), string)) {
    
    # Count the number of instances of 'pattern' in 'string' and 
    # exclude the number of instances of 'pattern' contained within single quotes
    count <- str_count(string, pattern) - 
      str_count(string |> str_extract(paste0("'.*", pattern, ".*'")), 
                pattern)
    
  } else {
    
    # If there are no quotes in 'string', use `str_count` as normal
    count <- str_count(string, pattern)
    
  }
  
  
  # Return 'count'
  return(count)
  
}



continuityCheck <- function (pyScript, editLoc, scriptPath) {
  
  # Some variables may use multiple lines to define their values
  
  # For a variable whose definition begins on 'editLoc',  
  # check subsequent lines to determine whether those lines are also part of
  # defining the variable
  
  # This function returns a vector containing the extra line numbers that are 
  # also part of the variable definition
  
  # If the variable is defined on a single line only (i.e., just on 'editLoc'),
  # the vector will be NULL
  
  
  # The main way to assess continuity is to rely on parentheses
  
  # If there is at least one opening parenthesis "(" in 'pyScript' at 'editLoc',
  # and there are more open parentheses "(" than closing parentheses ")",
  # then the number of subsequent related lines is equal to the number of lines
  # required to have a corresponding closing parenthesis ")" for each of the 
  # open parentheses "("
  if (netCount(pyScript[editLoc], "\\(") > 0 && 
      netCount(pyScript[editLoc], "\\(") > netCount(pyScript[editLoc], "\\)")) {
    
    # Get the initial balance of opening and closing parentheses on 'editLoc'
    parenBalance <- netCount(pyScript[editLoc], "\\(") - 
      netCount(pyScript[editLoc], "\\)")
    
    # Based on the conditions in the `if` statement, 'parenBalance' should
    # start out as a positive number
    # This means that there are more open parentheses "(" than closing 
    # parentheses ")"
    # Once every open parenthesis "(" is closed, 'parenBalance' will equal 0
    
    
    # Start by checking the line after 'editLoc'
    # Count the number of "(" and ")" on 'checkLine' and see if
    # all of the parentheses balance out 
    checkLine <- editLoc + 1
    
    
    # This vector will hold all of the subsequent lines after 'editLoc' 
    # that are a continuation of its code
    lineVec <- c()
    
    
    # Loop until all of the open parentheses are closed
    # (To prevent infinite loops, 'checkLine' should never exceed the length
    #  of 'pyScript')
    while (parenBalance != 0 && checkLine <= length(pyScript)) {
      
      # Add 'checkLine' to 'lineVec'
      lineVec <- c(lineVec, checkLine)
      
      
      # Update 'parenBalance' based on the number of open and closed 
      # parentheses on 'checkLine'
      parenBalance <- parenBalance + 
        netCount(pyScript[checkLine], "\\(") - 
        netCount(pyScript[checkLine], "\\)")
      
      
      # Increment 'checkLine' for the next iteration
      # (if 'parenBalance' isn't already 0)
      checkLine <- checkLine + 1
      
    }
    
    
    # If 'checkLine' exceeds the length of 'pyScript', the script may not 
    # have resolved successfully 
    if (checkLine > length(pyScript) && parenBalance != 0) {
      
      paste0("Could Not Find Continuation of Line in Python Script\n\n", 
             "The script attempted to replace a single argument in a Python ",
             "script. However, it was determined that the definition code ",
             "continues on more than one line of text. But the script failed ",
             "to identify a proper stopping point for the definition that ",
             "began on Line ", editLoc, ". Please investigate.\n\n",
             "(This error occurred for '", scriptPath, "')") |>
        errWrap() |>
        stop()
      
    }
    
    
    # Return 'lineVec'
    return(lineVec)
    
  }
  
  
  # If there are no parentheses in use on 'editLoc', or if there is no 
  # positive parenthesis balance, return 'NULL'
  # It is unlikely that the next line is a continuation of the previous line
  return(NULL)
  
  
  # In the future, in case this turns out to be wrong or insufficient, here's 
  # some additional code that applies a different methodology to try and 
  # identify continuity on subsequent lines of code
  
  # # Start this process from 'editLoc'
  # checkLine <- editLoc
  # 
  # 
  # # Add lines to 'lineVec' if there is continuity
  # lineVec <- c()
  # 
  # 
  # # As long as 'checkLine' is not the end of the script, perform this check
  # # (This prevents infinite loops)
  # while (checkLine < length(pyScript) && 
  #        grepl("^\\s*['\"]?[a-zA-Z0-9_/\\.\\-]+['\"]?[\\),]", 
  #              pyScript[checkLine + 1])) {
  #   
  #   
  #   # If the regex evaluates to TRUE, assume that the next line is a 
  #   # continuation of the current line
  #   
  #   # Record 'checkLine + 1' in 'lineVec' and increment 'checkLine'
  #   lineVec <- c(lineVec, checkLine + 1)
  #   
  #   checkLine <- checkLine + 1
  #   
  #   
  #   # "^\\s*['\"]?[a-zA-Z0-9_/\\.\\-]+['\"]?[\\),]"
  #   
  #   # The regular expression is interpreted as follows:
  #   # (*) Starts with 1 or more space characters
  #   # (*) May start with a single quote ' or double quote "
  #   # (*) Contains text characters (A-Z, 0-9, underscores, periods, hyphens)
  #   # (*) May end with a single quote ' or double quote "
  #   # (*) Followed up with a closing parenthesis ")" or comma ","
  #   
  #   # Essentially, this regex is meant to match lines like these:
  #   
  #   # myVar <- defStart("string1", "string2",
  #   #                   'string3', "string4")   <-- this kind of line
  #   
  #   # myVar <- defStart("string1", "string2",
  #   #                   model.name)             <-- this kind of line
  #   
  # }
  # 
  # 
  # # Return 'lineVec' after the loop
  # return(lineVec)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())



# Compare data in a SRP gag file to USGS gage data at the same location
# This script is specifically designed for comparing SRP gag files' values to 
# USGS gage 11446680 and other Russian River gages


# Precipitation data is included in some versions of thes comparison plots too

# There are two sources of precipitation data: PRISM grid cell averages and
# the average precipitation among gages in the SRP DAT file


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function (gageID = "11466800") {
  
  cat("\n\n")
  cat("Starting 'HLP_011_Compare_SRP_Output_to_USGS_Gage.R'!\n")
  
  
  # Notify the user which USGS gage is being assessed
  cat(paste0("\n\nRunning comparison for USGS Gage ", gageID, "!\n\n"))
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  cat("\n[1/3]\tGetting gag file and precipitation data...\n")
  
  
  # Confirm that the model hydrology folder exists and get its directory path
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Confirm that "SRP_inflow_6.gag" is present in the SRP "output" folder
  gagPath <- paste0(dirPath, "/SRP/output/SRP_inflow_", gageID, ".gag") |>
    checkForPreviousOutput()
  
  
  # Read in the gag file
  gagDF <- read_gag(gagPath)
  
  
  # Validate the contents of 'gagDF'
  # To do this, borrow the "validateGag" function from the Raw Flows script
  c("validateGag", "getColsFromMetadata") |>
    map(~ functionStealer("Scripts/RRW_016_Generate_Raw_Flows.R", .))
  
  
  gagDF <- gagDF |>
    validateGag(gagPath, dirPath)
  
  # NOTE: `validateGag` will also add a "DATE" column to the file
  
  
  # Gather precipitation data next
  
  
  # Use the average precipitation among PRISM grid cells in the SRP model domain
  # However, this may be split between one to three files
  
  # Because of its complexity, use a separate function to gather (and archive)
  # this dataset
  prismDF <- gatherPrecipPRISM(dirPath, endDate)
  
  
  # Read in precipitation data from the SRP DAT file too
  datDF <- gatherPrecipDAT(dirPath, startDate, endDate)
  
  
  # For consistency, have both 'prismDF' and 'datDF' use "PRECIP" as the
  # name for their precipitation columns (and "Date" for dates)
  
  # Also, their units should be inches
  
  # 'datDF' is already setup to meet these requirements
  # 'prismDF' must be adjusted though
  # (mm = 1/25.4 in)
  prismDF <- prismDF |>
    mutate(PRECIP = `ppt (mm)` / 25.4)
  
  
  cat("\tDone!\n\n")
  
  
  # Next, request data from USGS 
  cat("\n[2/3]\tGetting data from USGS...\n")
  
  
  # Check if the user provided an API key in the control spreadsheet
  # If so, import and validate it
  apiKey <- checkForValidKey()
  
  
  # Send a HTTP GET request
  usgsDF <- requestUSGS(stationID = gageID, 
                        startDate = min(gagDF$DATE), endDate = max(gagDF$DATE),
                        apiKey = apiKey)
  
  
  # Validate and process the returned dataset
  usgsDF <- usgsDF |>
    validateUSGS()
  
  
  cat("\tDone!\n\n")
  
  
  # Finally, compare the two datasets
  # Produce plots and calculate parameters such as Nash-Sutcliffe Efficiency
  cat("\n[3/3]\tComparing gage data and model results...\n")
  
  
  compareGageAndModel(usgsDF, gagDF, dirPath, gageID, gagPath, prismDF, datDF)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  "'HLP_011_Compare_SRP_Output_to_USGS_Gage.R' is complete!\n\n" |>
    col_green() |>
    cat()
  
  
  # Return nothing
  return(invisible(NULL))
  
}



gatherPrecipPRISM <- function (dirPath, endDate, model = "SRP") {
  
  # This function produces a tibble containing precipitation data for the 
  # entire modeled timeframe
  
  # This can come from:
  #   (*) The historic PRISM precipitation dataset, which covers 
  #       CY1981 to a recent WY
  #   (*) The downloaded precipitation dataset from the current run, which 
  #       covers the start of the water year to 'endDate'
  
  # Additional data may be required if there is a gap between the historic 
  # dataset and the recent precipitation dataset
  
  # (Similarly, if more PRISM data is available today compared to on the model
  #  run date, that should be downloaded too)
  
  
  # For subsequent runs of this function, generate and archive a compiled version
  # of these datasets to avoid having to redownload or redo this procedure
  
  # (In that case, there would be only one source for precipitation data)
  
 
  # First check if the "Input" SRP folder contains a single CSV for these 
  # PRISM precipitation averages
  compiledPath <- paste0(dirPath, "/", model, "/Input/",
                         "PRISM_Precip_", model, "_Domain_QAQC_",
                         Sys.Date() - 1, ".csv") |>
    normalizePath(mustWork = FALSE)
  
  
  # If the compiled file already exists from a previous run, use that
  if (file.exists(compiledPath)) {
    
    # Read in that file 
    compiledDF <- compiledPath |>
      getFile()
    
    
    # Validate 'compiledDF'
    compiledDF |>
      validateHistoricPrecipFile(compiledPath, getModeledWY(endDate)[1])
    
    
    # Then, return it
    return(compiledPath |>
             getFile())
    
  }
  
  
  # Otherwise, get both the historic precipitation data and the current WY
  # precipitation files
  historicPath <- getFromControl_RR(paste0("PRISM_", model, 
                                           "_HISTORIC_PRECIP_FOLDER")) |>
    getLatestFile(paste0("^RR_Workflow_PRISM_", model, "_Avg_Historic_Precip_",
                         "CY1981_to_WY[0-9]{4}\\.csv$"),
                  paste0(model, " Historic Precip File"))
  
  
  currentPath <- paste0(dirPath, "/", model, "/Input/PRISM_", model, 
                        "_Domain_Data_", 
                        getModeledWY(endDate)[1], "_", endDate, ".csv")
  
  
  # Read in both files
  historicDF <- historicPath |>
    getFile()
  
  
  currentDF <- currentPath |>
    getPRISM()
  
  
  # Validate these variables
  validateHistoricPrecipFile(historicDF, historicPath, getModeledWY(endDate)[1])
  
  
  # This function will also convert 'currentDF'
  # into the same format as 'historicDF'
  # (An average precipitation value for each day)
  currentDF <- currentDF |>
    validateAndSummarizePRISM(currentPath)
  
  
  # After that, combine both files
  compiledDF <- bind_rows(historicDF,
                          currentDF |> select(Date, `ppt (mm)`)) |>
    arrange(Date)
  
  
  # Check for missing data
  missingDF <- tibble(Date = seq(from = min(compiledDF$Date),
                                 to = max(c(compiledDF$Date, Sys.Date() - 1)),
                                 by = "days")) |>
    filter(!(Date %in% compiledDF$Date))
  
  
  # If 'missingDF' is empty, there are no missing dates
  if (nrow(missingDF) == 0) {
    
    # Save 'compiledDF' to the archive hydrology folder
    compiledDF |>
      writeOutput(compiledPath, quietly = TRUE)
    
    
    # Then, return 'compiledDF'
    return(compiledDF)
    
  }
  
  
  # If there are missing dates in 'missingDF', they must be downloaded from PRISM
  cat("\n\n")
  message("Additional PRISM data is required for this analysis!")
  cat("\n\n")
  
  
  # Setup the filepath for the new dataset
  extraPath <- paste0("WebData/PRISM_Precip_", model, "_Domain_Extra_QAQC_Data_",
                      Sys.Date() - 1, ".csv")
  
  
  # Then, import `runModifiedPRISM` from a prior script 
  # ('HLP_008_Update_Main_DAT_and_Historic_Precip_Files.R')
  # This function can download PRISM data 
  functionStealer("Scripts/HLP_008_Update_Main_DAT_and_Historic_Precip_Files.R",
                  "runModifiedPRISM")
  
  
  runModifiedPRISM(paste0("PRISM_", model, "_GRID_CELLS_CSV"), 
                   startDate = min(missingDF$Date), 
                   endDate = max(missingDF$Date), 
                   outFile = extraPath,
                   useHighRes = TRUE, interpCells = FALSE,
                   getPrecip = TRUE, getTemp = FALSE, useMetric = TRUE)
  
  
  # After that, read in the dataset
  extraDF <- getPRISM(extraPath)
  
  
  # Validate it and convert the dataset into the same format as 'compiledDF'
  extraDF <- extraDF |>
    validateAndSummarizePRISM(extraPath)
  
  
  # Merge it with 'compiledDF'
  compiledDF <- bind_rows(compiledDF,
                          extraDF) |>
    arrange(Date)
  
  
  # Validate the dataset one extra time
  validateHistoricPrecipFile(compiledDF, NA_character_, getModeledWY(endDate)[1])
  
  
  # Archive 'extraDF' and 'compiledDF' in the hydrology folder
  copyFile(extraPath,
           paste0(dirPath, "/", model, "/Input/",
                  extraPath |> str_remove("^.+[/\\\\]")), 
           quietly = TRUE)
  
  
  compiledDF |>
    writeOutput(compiledPath, quietly = TRUE)
  
  
  # Finally, return 'compiledDF'
  return(compiledDF)
  
}



gatherPrecipDAT <- function (dirPath, startDate, endDate) {
  
  # Use the DAT file that is input into SRP
  
  # It contains precipitation data for different gages
  
  # Take the averages of these values to get an estimate of basin precipitation 
  
  
  # Get the path to the DAT file and confirm that it exists
  datPath <- paste0(dirPath, "/SRP/Input/DAT_SRP_", Sys.info()[["user"]], "_",
                    startDate, "_", endDate, ".dat") |>
    checkForPreviousOutput()
  
  
  # Read in 'datPath'
  datDF <- getFile(datPath)
  
  
  # Check for the location of the header row
  headerIndex <- grep("^#+\\s*[a-zA-Z]", datDF)
  
  
  # Output an error message if it cannot be found
  if (length(headerIndex) != 1) {
    
    paste0("Could Not Locate Column Header\n\n", 
           "This script attempted to find the header row in the SRP DAT ",
           "file. However, the regular expression that identifies this ",
           "line returned ", length(headerIndex), " matches.\n\n", 
           "Please investigate '", datPath, "'") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Extract the headers from this row
  # Split the values at the spaces and exclude the "#####" string
  # (and "date" if it appears in the vector)
  headers <- datDF[headerIndex] |>
    str_split("\\s+") |> unlist() |>
    tolower() |>
    str_subset("^#+$", negate = TRUE) |>
    str_subset("^date$", negate = TRUE)
  
  
  # Consider only the rows after 'headerIndex'
  # Then, split the values at the spaces and reformat the data
  # Shape it into a matrix and then a tibble
  # Finally, apply the column headers to it
  datDF <- datDF[(headerIndex + 1):length(datDF)] |>
    str_split("\\s+") |> unlist() |>
    matrix(ncol = length(headers), byrow = TRUE) |>
    as_tibble() |>
    set_names(headers)
  
  
  # Use 'year', 'month', and 'day' to define a "Date" variable
  # After that, select the new "Date" column and any precipitation columns
  datDF <- datDF |>
    mutate(Date = paste0(year, "-", month, "-", day) |>
             as.Date("%Y-%m-%d")) |>
    select(Date, contains("precip"))
  
  
  # Then, calculate a new "PRECIP" column 
  # Take the average of the precipitation values
  # Make sure the precipitation values are numeric and then reshape the tibble
  # so that all precipitation columns appear in the same column
  # After that, group by "Date" and average precipitation values 
  # that occurred on the same day
  datDF <- datDF |>
    mutate(across(contains("precip"), as.numeric)) |>
    pivot_longer(contains("precip"), 
                 names_to = "STATION", values_to = "PRECIP") |>
    group_by(Date) |>
    summarize(PRECIP = mean(PRECIP), .groups = "drop")

  
  # Return 'datDF' afterwards
  return(datDF)
  
}



validateAndSummarizePRISM <- function (prismDF, prismPath) {
  
  # Given a dataset containing PRISM data, 
  # Validate it using `validateWebData`
  
  # Then, summarize its values into a daily average precipitation
  
  # The final tibble will just have "Date" and "ppt (mm)" columns
  
  
  # 'prismDF' may not contain temperature columns
  # Therefore, add dummy rows before applying the validation function
  # (Since 'prismDF' will be wrapped up into daily average precipitation,
  #  the temperature columns are unimportant anyways)
  prismDF |>
    mutate(`tmin (degrees C)` = 0, `tmax (degrees C)` = 0) |>
    validateWebData(dataSource = "PRISM",
                    inputPath = prismPath,
                    stationVec = prismDF$Name |> unique(),
                    siPRISM = TRUE)
  
  
  # Group 'prismDF' by Date and average the precipitation values
  # of all stations in its dataset
  prismDF <- prismDF |>
    group_by(Date) |>
    summarize(`ppt (mm)` = mean(`ppt (mm)`), .groups = "drop")
  
  
  # Return 'prismDF'
  return(prismDF)
  
}



checkForValidKey <- function () {
  
  # If a key for USGS's API has been provided, use it in the request
  
  
  # Check if the user has provided a file containing a key
  apiPath <- getFromControl_RR("USGS_API_KEY")
  
  
  # If the field is empty, return NA
  if (is.na(apiPath)) {
    return(NA_character_)
  }
  
  
  # Otherwise, try to read in the file
  apiKey <- apiPath |>
    sharepointPathCheck(isFolder = FALSE) |>
    getFile()
  
  
  # Borrow the API key validation function from the CIMIS script
  functionStealer("Scripts/RRW_004_CIMIS_API_Scraper.R",
                  "validateAPI")
  
  
  # Try to validate the API key
  validateAPI(apiKey, "USGS_API_KEY", provider = "USGS")
  
  
  # If no errors were detected, extract the first element in 'apiKey'
  # and return it
  return(apiKey |> unlist(use.names = FALSE) |> head(1))
  
}



requestUSGS <- function (stationID, startDate, endDate, 
                         limit = 50000, offset = 0, apiKey = NA_character_) {
  
  # Request daily average streamflow data from a USGS station
  
  # This function uses the new OCG API from USGS
  # (NWIS will be decommissioned over time)
  
  # A "GET" request will be sent, and data will be retrieved (in CSV format)
  
  
  # API documentation page
  # https://api.waterdata.usgs.gov/ogcapi/v0/
  
  # Sandbox
  # https://api.waterdata.usgs.gov/ogcapi/v0/openapi?f=html#/daily/getDailyFeature
  
  
  # NOTE
  
  # Parameter Code 00060 in the OCG API corresponds to 
  # discharge in cubic feet per second (cfs)
  
  # Statistic ID 00003 corresponds to average values ("mean")
  
  
  # Prepare the request URL first
  # (The "api_key" parameter will only be included if the user provided 
  #  a valid API key)
  requestURL <- paste0("https://api.waterdata.usgs.gov/ogcapi/v0/",
                       "collections/daily/items?",
                       "f=csv&lang=en-US",
                       if_else(is.na(apiKey), "", paste0("&api_key=", apiKey)),
                       "&limit=", limit, 
                       "&skipGeometry=false",
                       "&offset=", offset,  
                       "&datetime=", format(startDate, "%Y-%m-%d"), "T00:00:00Z",
                       URLencode("/", reserved = TRUE), 
                       format(endDate, "%Y-%m-%d"), "T12%3A31%3A12Z",
                       "&monitoring_location_id=USGS-", stationID,
                       "&parameter_code=00060&statistic_id=00003")
  
  
  # Wait a bit before proceeding
  Sys.sleep(1.1)
  
  
  # Submit the request
  req <- GET(requestURL)
  
  
  # Wait a bit again
  Sys.sleep(0.4)
  
  
  # Check for a successful request
  if (req$status_code != 200) {
    
    # Output the results of the request and the response's content
    cat("\n\n")
    print(req)
    cat("\n\n")
    print(content(req))
    cat("\n\n")
    
    
    # Send an error message too
    paste0("USGS HTTP Request Failed\n\n",
           "A GET request sent to USGS's server returned an error code of ", 
           req$status_code, ". This could be a problem with the request ",
           "and/or USGS's server. An excessive number of requests can ",
           "trigger an error too.\n\n",
           "Please investigate this issue.") |>
      errWrap() |>
      stop()
    
  } 
  
  
  # Extract the CSV result from 'req'
  usgsDF <- content(req)
  
  
  # If 'usgsDF' contains as many rows as specified in 'limit',
  # it may have hit the limit for the maximum number of returned rows
  if (nrow(usgsDF) == limit) {
    
    # Wait a bit before proceeding
    Sys.sleep(0.8)
    
    
    # Recursively call this function
    # Submit another request with the "offset" parameter incremented
    # Bind that result to this run's 'usgsDF'
    return(usgsDF |>
             bind_rows(requestUSGS(stationID, startDate, endDate, limit, 
                                   offset = offset + limit)) |>
             unique())
    
  }
  
  
  # Return 'usgsDF' otherwise
  return(usgsDF)
  
}



validateUSGS <- function (usgsDF) {
  
  # Validate a dataset containing streamflow values from a USGS gage
  
  # The values should be daily average discharge (in cfs)
  
  
  # First, confirm that 'usgsDF' contains the expected columns
  requiredCols <- tibble(NAME = c("time", "value", "unit_of_measure"),
                         TYPE = c("Date", "numeric", "character"))
  
  
  # Check for the exact column names too
  if (anyFalse(requiredCols$NAME %in% names(usgsDF))) {
    
    missingColumns <- which(!(requiredCols$NAME %in% names(usgsDF)))
    
    paste0("Missing Column",
           if_else(length(missingColumns) > 1, "s", ""),
           " in USGS Gage Data\n\n",
           "The dataset containing streamflow data from a USGS gage should ",
           "have several specific columns (", vec2QuotedStr(requiredCols$NAME), 
           "). However, this is not the case (missing ", 
           vec2QuotedStr(requiredCols$NAME[missingColumns]), 
           "). Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Confirm the column types next
  
  
  # Iterate through 'requiredCols'
  for (i in 1:nrow(requiredCols)) {
    
    # Check if each variable has the proper assigned class
    if (is.null(class(usgsDF[[requiredCols$NAME[i]]])) || 
        class(usgsDF[[requiredCols$NAME[i]]]) != requiredCols$TYPE[i]) {
      
      paste0("\"", requiredCols$NAME[i], "\" Variable Type Issue\n\n",
             "The \"", requiredCols$NAME[i], "\" variable in the USGS gage ",
             "dataset could not be parsed as a \"", requiredCols$TYPE[i],
             "\" type variable. This suggests that there is a problem with ",
             "the data. Please investigate.") |>
        errWrap() |>
        stop()
      
    }
    
  }
  
  
  # Make sure the "unit_of_measure" column only contains "ft^3/s"
  if (length(unique(usgsDF$unit_of_measure)) != 1 ||
      anyNA(usgsDF$unit_of_measure) || 
      unique(usgsDF$unit_of_measure) != "ft^3/s") {
    
    # Output the unique values in this column first
    cat("\n\n")
    print(usgsDF$unit_of_measure |> unique())
    cat("\n\n")
    
    
    # Then display an error message
    paste0("\"unit_of_measure\" Variable Issue\n\n",
           "The \"unit_of_measure\" variable in the USGS gage dataset should ",
           "have only one possible value (\"ft^3/s\"). However, the column ",
           "contains something else. Its unique values were printed above. ",
           "Please investigate the dataset for errors.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The "time" column in 'usgsDF' should not have any duplicate days
  if (length(unique(usgsDF$time)) != nrow(usgsDF)) {
    
    paste0("\"time\" Variable Issue\n\n",
           "The \"time\" variable in the USGS gage dataset should have one ",
           "row per day. However, at least one date appears to be duplicated ",
           "in the dataset. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Finally, to aid in processing the dataset, look for missing dates
  # Note which "YEAR-MONTH" pairs in the dataset are incomplete
  
  
  # Get a tibble of all dates between the first and last dates in 'usgsDF'
  # This will be used to identify gaps in 'usgsDF'
  dateDF <- tibble(DATE = seq(from = min(usgsDF$time),
                              to = max(usgsDF$time),
                              by = "days")) |>
    mutate(YEAR_MONTH = paste0(year(DATE), "-", month(DATE)))
  
  
  # Filter 'dateDF' to dates that are missing from "time" in 'usgsDF'
  missingDF <- dateDF |>
    filter(!(DATE %in% usgsDF$time))
  
  
  # Add "YEAR_MONTH" to 'usgsDF' and another column that indicates whether
  # its month has a complete set of data
  # (Months that are *incomplete* will appear in 'missingDF')
  usgsDF <- usgsDF |>
    mutate(YEAR_MONTH = paste0(year(time), "-", month(time))) |>
    mutate(IS_COMPLETE_MONTH = !(YEAR_MONTH %in% missingDF$YEAR_MONTH))
  
  
  # Return 'usgsDF' if there are no issues
  return(usgsDF)
  
}



compareGageAndModel <- function (usgsDF, gagDF, dirPath, gageID, gagPath,
                                 prismDF, datDF) {
  
  # Compare the streamflow data in 'usgsDF' and 'gagDF' 
  
  # On both daily and monthly timescales, perform comparisons:
  #   (*) 1-year comparisons
  #   (*) 5-year comparisons
  #   (*) 10-year comparisons
  #   (*) Full dataset range comparisons
  
  # Produces plots and calculate statistical metrics too
  # (Nash-Sutcliffe efficiency, P-Bias, etc.)
  
  
  # Filter both 'usgsDF' and 'gagDF'
  # They need to contain data for the same dates
  modelDF <- gagDF[gagDF$DATE %in% usgsDF$time, ] |>
    arrange(DATE)
  
  gageDF <- usgsDF[usgsDF$time %in% modelDF$DATE, ] |>
    arrange(time)
  
  # 'modelDF' will hold the modeled streamflow data
  # 'gageDF' will contain the USGS gage discharge data
  
  
  # Then, merge the two together into one tibble
  # It will have daily streamflow data from both sources
  dailyDF <- left_join(gageDF |>
                         select(time, value, YEAR_MONTH, IS_COMPLETE_MONTH) |>
                         rename(DATE = time, GAGE = value),
                       modelDF |>
                         select(DATE, Flow) |>
                         rename(MODEL = Flow),
                       by = "DATE")
  
  
  # Make sure no "NA" values appear in 'dailyDF'
  if (anyNA(dailyDF)) {
    
    paste0("Failed to Combine Gage and Model Streamflow Data\n\n",
           "The script attempted to create a dataset with both USGS gage data ",
           "and modeled streamflow in the same data frame. However, \"NA\" ",
           "values occurred when combining the two tables. An unknown error ",
           "has occurred. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Currently, the modeled streamflow has units of cubic feet per day (cfd)
  # Meanwhile, the gage data is using cubic feet per second (cfs)
  
  # Adjust the units in "GAGE" and convert it to cfd
  # cfs * 60 s/min * 60 min/hr * 24 hr/day = cfd
  dailyDF <- dailyDF |>
    mutate(GAGE = GAGE * 60 * 60 * 24)
  
  
  # After that, convert both "GAGE" and "MODEL" flows into acre-feet per day (AFD)
  # cfd * 1/43559.9 AF/ft^3 = AF/d
  dailyDF <- dailyDF |>
    mutate(GAGE = GAGE / 43559.9,
           MODEL = MODEL / 43559.9)
  
  
  # Prepare to generate plots and tables
  
  
  # First, create a new folder in the SRP "output" directory 
  # This will hold the data output by this function
  newDir <- prepNewDirectory(dirPath, gageID)
  
  
  # Once 'newDir' has been established, 
  # create plots and summary statistics for different timescales
  
  
  # First generate plots and a table for the full datasets
  statDF <- generatePlotsAndTable(dailyDF, newDir, "All", prismDF, datDF)
  
  
  # If the dataset contains at least one year of data, 
  # generate a one-year version too
  if (nrow(dailyDF) > 365) {
    
    statDF <- bind_rows(statDF,
                        generatePlotsAndTable(dailyDF, newDir, "1_yr",
                                              prismDF, datDF))
    
  }
  
  
  # If the dataset contains at least five years of data, 
  # generate a five-year version too
  if (nrow(dailyDF) > 365 * 5) {
    
    statDF <- bind_rows(statDF,
                        generatePlotsAndTable(dailyDF, newDir, "5_yr",
                                              prismDF, datDF))
    
  }
  
  
  # If the dataset contains at least ten years of data, 
  # generate a ten-year version too
  if (nrow(dailyDF) > 365 * 10) {
    
    statDF <- bind_rows(statDF,
                        generatePlotsAndTable(dailyDF, newDir, "10_yr",
                                              prismDF, datDF))
    
  }
  
  
  # Write 'statDF' to 'newDir'
  statDF |>
    writeOutput(paste0(newDir, "/Stat_Metrics_", gageID, ".csv"))
  
  
  # Save both 'usgsDF' and 'gagDF' to 'newDir' as well
  usgsDF |>
    writeOutput(paste0(newDir, "/USGS_Gage_Streamflow_", gageID, ".csv"),
                quietly = TRUE)
  
  gagDF |>
    writeOutput(paste0(newDir, "/", 
                       gagPath |> str_remove("^.+[/\\\\]") |> 
                         str_replace("\\.gag$", "_Processed.csv")),
                quietly = TRUE)
  
  
  # Finally, make a decision based on the values in 'statDF'
  
  # If something is extremely problematic, do NOT proceed with the workflow
  
  
  # Checking the entire data range, 
  # if the monthly streamflow NSE value is below 0.5, 
  # stop the script and flag it as an error
  if (statDF$MONTHLY_RESULT[grepl("Nash", statDF$METRIC) & 
                            statDF$TIMESCALE == "All"] < 0.50) {
    
    paste0("Unexpectedly Low Nash-Sutcliffe Result for Monthly Streamflow\n\n",
           "In a comparison with USGS gage data (ID \"", gageID, "\"), the ",
           "modeled streamflow values appear to be excessively different. ",
           "On a monthly timescale, the calculated Nash-Sutcliffe Efficiency is ",
           statDF$MONTHLY_RESULT[grepl("Nash", statDF$METRIC) & 
                                   statDF$TIMESCALE == "All"] |> 
             round(digits = 3), ". Please investigate this issue.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



prepNewDirectory <- function (dirPath, gageID) {
  
  # Generate a new folder in the SRP "output" folder
  # It will contain data from this gage comparison
  
  
  # By default, the folder name will be "[GAGE_ID]_Comparison"
  newDir <- paste0(dirPath, "/SRP/output/", gageID, "_Comparison")
  
  
  # If the directory already exists, adjust the name to have a number at the end
  while (dir.exists(newDir)) {
    
    # If 'newDir' doesn't have any incrementing number in its name (e.g., "(#2)"),
    # add "_(#2)" to the directory name now
    if (!grepl("_\\(#[0-9]+\\)$", newDir)) {
      
      newDir <- paste0(newDir, "_(#2)")
      
      # (This situation happens only in the first iteration of this loop)
      
    } else {
      
      # If there's already an incrementing number in the folder name, 
      # extract it into 'dirNum'
      dirNum <- newDir |>
        str_extract("[0-9]+(?=\\)$)") |>
        as.numeric()
      
      
      # Increment the number
      dirNum <- dirNum + 1
      
      
      # Update the name in 'newDir' to have the new 'dirNum' instead
      newDir <- newDir |>
        str_replace("_\\(#[0-9]+\\)$",
                    paste0("_(#", dirNum, ")"))
      
    }
    
  } # End of loop to pick a name for the new gage folder
  
  
  # Create the new folder for the gage data comparisons
  dir.create(newDir)
  
  
  # Return the path 'newDir'
  return(newDir)
  
}



generatePlotsAndTable <- function (dailyDF, newDir, timescale, prismDF, datDF) {
  
  # For the input timescale, produce plots and a table
  
  # Save the plots to 'newDir' and return the table as a tibble
  
  # These actions will be performed for both daily and monthly streamflow datasets
  
  # Precipitation data is included in some versions of these plots
  # (It can come from two different sources: Either PRISM or the SRP DAT file)
  
  
  # Based on the value in 'timescale', apply a different filter to 'dailyDF'
  if (timescale == "1_yr") {
    
    # Keep only data from the past year
    cutoff <- max(dailyDF$DATE) - years(1)
    
    
    dailyDF <- dailyDF |>
      filter(DATE > cutoff)
    
  # This filter is for the last five years
  } else if (timescale == "5_yr") {
    
    cutoff <- max(dailyDF$DATE) - years(5)
    
    
    dailyDF <- dailyDF |>
      filter(DATE > cutoff)
    
  # This filter applies to the last ten years
  } else if (timescale == "10_yr") {
    
    cutoff <- max(dailyDF$DATE) - years(10)
    
    
    dailyDF <- dailyDF |>
      filter(DATE > cutoff)
    
  }
  
  
  # Next, create a monthly version of 'dailyDF' too
  # Use only complete months and rely on "YEAR_MONTH" to help group data

  # With data in acre-feet per day, summing the data by month will 
  # result in units of acre-feet per month
  monthlyDF <- dailyDF |>
    filter(IS_COMPLETE_MONTH) |>
    group_by(YEAR_MONTH) |>
    summarize(GAGE = sum(GAGE),
              MODEL = sum(MODEL)) |>
    mutate(YEAR_MONTH = as_date(YEAR_MONTH, format = "%Y-%m"))
  
  
  # After that, move on to the charts and statistics 
  
  # Start by generating plots
  # Use a separate function for that
  
  
  # Generate plots without any precipitation data
  dailyDF |>
    generateComparisonPlot(paste0(newDir, "/Daily_Comparison_", 
                                  timescale, ".png"),
                           isDaily = TRUE)
  
  
  monthlyDF |>
    generateComparisonPlot(paste0(newDir, "/Monthly_Comparison_", 
                                  timescale, ".png"),
                           isDaily = FALSE)
  
  
  # Then, use PRISM grid cell precipitation data
  dailyDF |>
    generateComparisonPlot(paste0(newDir, "/Daily_Comparison_", 
                                  timescale, "_PRISM_Precip.png"),
                           prismDF, isDaily = TRUE, precipType = "PRISM Avg")
  
  
  monthlyDF |>
    generateComparisonPlot(paste0(newDir, "/Monthly_Comparison_", 
                                  timescale, "_PRISM_Precip.png"),
                           prismDF, isDaily = FALSE, precipType = "PRISM Avg")
  
  
  # Try, precipitation data from the SRP DAT file next
  dailyDF |>
    generateComparisonPlot(paste0(newDir, "/Daily_Comparison_", 
                                  timescale, "_DAT_Precip.png"),
                           datDF, isDaily = TRUE, precipType = "DAT Avg")
  
  
  monthlyDF |>
    generateComparisonPlot(paste0(newDir, "/Monthly_Comparison_", 
                                  timescale, "_DAT_Precip.png"),
                           datDF, isDaily = FALSE, precipType = "DAT Avg")
  
  
  # After that, create a tibble that contains different statistical metrics
  statDF <- tibble("TIMESCALE" = timescale, 
                   "METRIC" = c("Nash-Sutcliffe Efficiency",
                                "P-Bias",
                                paste0("Root Mean Square Error to ",
                                       "Standard Deviation Ratio"),
                                "Modified Kling-Gupta Efficiency",
                                "R Squared"),
                   "DAILY_RESULT" = NA_real_,
                   "DAILY_NOTES" = "--",
                   "MONTHLY_RESULT" = NA_real_,
                   "MONTHLY_NOTES" = "--")
  
  
  statDF <- statDF |>
    mutate(DAILY_RESULT = 
             case_when(
               grepl("^Nash", METRIC) ~ calcNSE(dailyDF$GAGE, dailyDF$MODEL),
               grepl("Bias$", METRIC) ~ calcPBias(dailyDF$GAGE, dailyDF$MODEL),
               grepl("^Root", METRIC) ~ calcRSR(dailyDF$GAGE, dailyDF$MODEL),
               grepl("^Modif", METRIC) ~ calcMKGE(dailyDF$GAGE, dailyDF$MODEL),
               grepl("^R Sq", METRIC) ~ calcRSqrd(dailyDF$GAGE, dailyDF$MODEL)
             )) |>
    mutate(MONTHLY_RESULT = 
             case_when(
               grepl("^Nash", METRIC) ~ calcNSE(monthlyDF$GAGE, monthlyDF$MODEL),
               grepl("Bias$", METRIC) ~ calcPBias(monthlyDF$GAGE, monthlyDF$MODEL),
               grepl("^Root", METRIC) ~ calcRSR(monthlyDF$GAGE, monthlyDF$MODEL),
               grepl("^Modif", METRIC) ~ calcMKGE(monthlyDF$GAGE, monthlyDF$MODEL),
               grepl("^R Sq", METRIC) ~ calcRSqrd(monthlyDF$GAGE, monthlyDF$MODEL)
             ))
  
  
  # For P-Bias, add to the "NOTES" columns whether the result is an
  # overprediction or underprediction (this interpretation varies depending 
  # on the exact formula used)
  statDF$DAILY_NOTES[statDF$METRIC == "P-Bias"] <- 
    calcPBias(dailyDF$GAGE, dailyDF$MODEL) |> 
    attributes() |> pluck(1)
  
  
  statDF$MONTHLY_NOTES[statDF$METRIC == "P-Bias"] <- 
    calcPBias(monthlyDF$GAGE, monthlyDF$MODEL) |> 
    attributes() |> pluck(1)
  
  
  # Return 'statDF'
  return(statDF)
  
}



generateComparisonPlot <- function (streamDF, writePath, precipDF = NULL,
                                    isDaily = TRUE, volUnit = "AF", 
                                    precipType = "PRISM Avg") {
  
  # Generate a plot for 'streamDF' 
  # It can contain either daily or monthly streamflow data
  
  # 'precipDF', which generally contains precipitation data for the same period,
  # will be included as bars in the graph (if it isn't NULL)
  
  
  # If 'precipDF' was provided as input, adjust it to the bounds of 'streamDF'
  if (!is.null(precipDF)) {
    
    # For daily streamflow, filter 'precipDF' to the same range as 'streamDF'
    if (isDaily) {
      
      # Rename "Date" to "DATE" in order to match 'streamDF'
      precipDF <- precipDF |>
        filter(Date >= min(streamDF$DATE) & Date <= max(streamDF$DATE)) |>
        rename(DATE = Date)
      
      
      # Then, filter 'streamDF' to match the date range in 'precipDF'
      streamDF <- streamDF |>
        filter(DATE >= min(precipDF$DATE) & DATE <= max(precipDF$DATE))
      
      
      # Otherwise, for monthly streamflow, 
      # the procedure is a little more complicated
    } else {
      
      # Convert 'precipDF' into a monthly timescale using a "YEAR_MONTH" column
      precipDF <- precipDF |>
        mutate(YEAR_MONTH = paste0(year(Date), "-", month(Date)) |>
                 as_date(format = "%Y-%m")) |>
        filter(YEAR_MONTH >= min(streamDF$YEAR_MONTH) & 
                 YEAR_MONTH <= max(streamDF$YEAR_MONTH)) |>
        group_by(YEAR_MONTH) |>
        summarize(PRECIP = sum(PRECIP), .groups = "drop")
      
      
      # Then, filter 'streamDF' to match the date range in 'precipDF'
      streamDF <- streamDF |>
        filter(YEAR_MONTH >= min(precipDF$YEAR_MONTH) & 
                 YEAR_MONTH <= max(precipDF$YEAR_MONTH))
      
    }
    
  }
  
  
  # If daily streamflow will be plotted, the x-axis will be the "DATE" column
  # Otherwise, for monthly streamflow, it is the "YEAR_MONTH" column
  xCol <- if_else(isDaily, "DATE", "YEAR_MONTH")
  
  
  # Make sure this column exists in 'streamDF' too
  if (!(xCol %in% names(streamDF))) {
    
    paste0("Streamflow Dataset Missing Expected Column\n\n",
           "Because ", if_else(isDaily, "daily", "monthly"), " streamflow ",
           "will be plotted, this function expected the input data frame ",
           "to contain the column \"", xCol, "\". However, it was not found. ",
           "Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The selected 'xCol' column in 'streamDF' should be a "Date" type variable
  if (is.null(class(streamDF[[xCol]])) || class(streamDF[[xCol]]) != "Date") {
    
    paste0("Streamflow Dataset Column Type Issue\n\n",
           "To plot ", if_else(isDaily, "daily", "monthly"), " streamflow, ",
           "this function uses the column \"", xCol, "\". However, it is not ",
           "a \"Date\" type variable. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Get the limits for the y-axis (streamflow)
  yBounds <- c(streamDF$GAGE, streamDF$MODEL) |>
    range()
  
  
  # Prepare the label for the y-axis too
  yLabel <- paste0(if_else(isDaily, "Daily ", "Monthly "),
                 "Streamflow (", volUnit, "/",
                 if_else(isDaily, "Day", "Month"), ")")
  
  
  # If 'precipDF' was provided, a label will be needed for a secondary y-axis too
  if (!is.null(precipDF)) {
    
    yLabel2 <- paste0(if_else(isDaily, "Daily ", "Monthly "),
                      "Precipitation (in/",
                      if_else(isDaily, "Day", "Month"), ")")
    
  }
  
  
  # The graph can contain vertical bars at missing dates
  # Identify them using a new variable
  if (isDaily) {
    
    # For daily streamflow, get the dates that are missing in 'streamDF'
    missingDF <- tibble(DATE = seq(from = min(streamDF$DATE),
                                   to = max(streamDF$DATE),
                                   by = "days")) |>
      filter(!(DATE %in% streamDF$DATE))
    
  # For monthly streamflow charts, get the missing "YEAR_MONTH" pairs instead
  } else {
    
    missingDF <- tibble(DATE = seq(from = min(streamDF$YEAR_MONTH),
                                   to = max(streamDF$YEAR_MONTH),
                                   by = "days")) |>
      mutate(YEAR_MONTH = paste0(year(DATE), "-", month(DATE)) |>
               as_date(format = "%Y-%m")) |>
      filter(!(YEAR_MONTH %in% streamDF$YEAR_MONTH)) |>
      select(YEAR_MONTH) |> unique()
    
  }
  
  # Either way, 'missingDF' will be a single-column tibble containing a 
  # "Date" type variable ("DATE" or "YEAR_MONTH")
  
  
  # The next step is to design the plots
  
  # The setup differs depending on whether 'precipDF' is present
  
  
  # For 'precipDF', to have vertical bars coming down from the top, 
  # both y-axes must be reversed
  
  # That means that transformations are necessary to get the streamflow data
  # back into the correct position (i.e., back to having zero at the bottom)
  
  
  # Start by initializing 'streamPlot' with all customizations that are SHARED
  # between the two options
  streamPlot <- ggplot(streamDF) +
    
    xlab("Date") + ylab(yLabel) +
    # Axis labels
    
    guides(color = guide_legend(title = "Flow Type")) +
    scale_color_manual(values = c("Gage" = "blue", "Model" = "red")) + 
    # Set the colors of the streamflow lines (plus the name of their legend)
    
    scale_x_date(date_labels = if_else(nrow(streamDF) < 365 * 5, "%Y-%m", "%Y")) +
    # Set the appearance of the x-axis date labels (see more details below)
    
    coord_cartesian(ylim = yBounds) +
    # Limit the chart's y-axis to the values in 'yBounds'
    
    theme_gray(base_size = 20)
    # Set the default font size to "20" units instead of "11"
  
  
  # The x-axis labels use either "Year-Month" or "Year" depending on the size
  # of 'streamDF' 
  
  # For daily data, plots with at least 5 years worth of data use just
  # years in their labels; smaller plots use "Year-Month"
  
  # For monthly data, essentially all cases use "Year-Month" ('streamDF' 
  # would need at least 365 * 5 = 1825 months of data to switch its labels)
  
  
  # All of these settings are shared in both versions of 'streamPlot'
  
  # The next set of edits are dependent on 'precipDF' 
  
  
  # First handle the (simpler) case when 'precipDF' is NOT present
  if (is.null(precipDF)) {
    
    # In this case, the only components missing from 'streamPlot' 
    # are the actual streamflow lines themselves
    
    # Add lines for "GAGE" and "MODEL"
    streamPlot <- streamPlot +
      
      geom_line(mapping = aes(x = get(xCol), y = GAGE, color = "Gage"), 
                lwd = 0.8) +
      # Linewidth = 0.8 units
      
      geom_line(mapping = aes(x = get(xCol), y = MODEL, color = "Model"),
                lwd = 0.8, linetype = 2, alpha = 0.6)
      # Linewidth = 0.8 units, dashed linetype, partially transparent
      
    
    # Note: The colors assigned to each of these lines are strings
    #       ("Gage" and "Model")
    #       
    #       In the initial definition of 'streamPlot', both `guides` and 
    #       `scale_color_manual` were setup to use the streamflow lines' 
    #       colors as a legend (so the actual color assignments are in 
    #       `scale_color_manual`)
    
    
    # The alternative scenario occurs if 'precipDF' is present
  } else {
    
    # Prepare 'streamPlot' with a more complicated approach
    
    # We have to reverse the y-axes to make the precipitation columns come down
    # from the top
    
    # We can't just do this to one axis, and if we try to flip the precipitation
    # data from a regular set of axes, the columns will not draw correctly
    
    # So we have to reverse all the y-axes first, and it will be easier to 
    # reverse the streamflow lines back to a normal appearance
    
    
    # A requirement of this approach is that the primary y-axis breaks 
    # will have to be set manually
    
    # We want nice roundish numbers as the axis breaks
    # However, `ggplot` will default to nice breaks for the reversed primary axis
    
    # When we transform the primary y-axis back into a normal ordering (i.e.,
    # with zero at the bottom), the corresponding values at the axis breaks 
    # will not be nice numbers
    
    # This function will get us nice numbers on the post-transformation axis
    breakVals <- getNiceAxisBreaks(yBounds[2], yBounds[1])
    
    
    # After that, get the extreme values in 'precipDF'
    precipRange <- range(precipDF$PRECIP)
    
    
    # Prepare the chart next
    
    streamPlot <- streamPlot + 
      
      geom_line(mapping = aes(x = get(xCol), y = yBounds[2] + yBounds[1] - GAGE, 
                              color = "Gage"), 
                lwd = 0.8) +
      # The gage data will be coming from the top down, and this transformation
      # to the "y" variable will correct it to appear as if it came from the 
      # bottom up instead
      
      geom_line(mapping = aes(x = get(xCol), y = yBounds[2] + yBounds[1] - MODEL, 
                              color = "Model"), 
                lwd = 0.8, linetype = 2, alpha = 0.6) + 
      # The same transformation as above is applied to the modeled streamflow data
      
      geom_col(data = precipDF, 
               mapping = aes(x = get(xCol), 
                             y = PRECIP * diff(yBounds) / diff(precipRange), 
                             fill = precipType), 
               width = setPrecipColumnWidths(isDaily, nrow(precipDF)), 
               alpha = 0.35) +
      # Set precipitation values next--a transformation maps the precipitation
      # data to the same scale as the streamflow data (see more details below)
      # Its color is setup to appear in a legend, the width of each column is 
      # determined in a separate function, and the columns are set to be mostly
      # transparent
      
      guides(fill = guide_legend(title = "Precipitation")) + 
      scale_fill_manual(values = c("#0081FF") |> set_names(precipType)) + 
      # Set the colors of the precipitation columns (and the name of their legend)
      
      scale_y_reverse(breaks = breakVals, 
                      labels = ~ yBounds[2] + yBounds[1] - .,
                      sec.axis = 
                        sec_axis(~ . * diff(precipRange) / diff(yBounds), 
                                 name = yLabel2))
      # This is what actually flips the y-axis to come down from the top
      # 
      # The breaks are set using 'breakVals' (described earlier)
      # 
      # The labels have a transformation applied so that they reflect the
      # bottom-up streamflow data correctly (and their numbers are actually 
      # nice thanks to the efforts in creating 'breakVals')
      # 
      # The secondary y-axis for precipitation is also setup here
      # 
      # Its values *should* come from the top down, so the default axis values
      # will already be nice numbers
      # 
      # The only requirement is specifying the transformation correctly 
      # (since all secondary y-axes are purely decorative, and the data is 
      #  actually still plotted relative to the streamflow axis)
      # 
      # This is why a transformation was applied to the precipitation data in
      # the `geom_col` call
      # 
      # The data was rescaled to follow the reversed streamflow axis properly
      # 
      # The secondary axis has the opposite of this transformation so that 
      # the streamflow y-axis values can be rescaled in the secondary y-axis and 
      # properly reflect the original precipitation values
      # 
      
     
      # ...Who knew the plotting would get so complicated? (>.<)
      
      # To summarize, the streamflow and precipitation values are a lie
      # As are both y-axes' labels
    
      # The streamflow lines and precipitation columns get their values 
      # by assuming that y = 0 is at the top of the graph
    
      # This is still true
    
      # However, their values have been rescaled to create the illusion that: 
      # (1) the streamflow data is coming from the bottom
      # (2) the precipitation data is relative to the secondary axis
    
    
      # The formula applied to the streamflow data made it so that the values we 
      # want to show are indeed scaled correctly (and relative to the bottom of 
      # the graph--as if y = 0 was at the bottom of the plot!)
    
      # Meanwhile, the main y-axis labels are also reversing the y-axis reverse, 
      # with breaks in the graph set at "nice numbers" when considered from the 
      # bottom-up (i.e., y = 0 at the bottom of the plot)
    
      # These breaks are likely ugly if we consider their "true" top-down values
    
      # And the precipitation data is intended to be top-down, but it is plotted
      # against the streamflow data's y-axis, which has a different scaling
      
      # So the precipitation data is transformed (mapping its extremes to the 
      # extremes of the streamflow data)
    
      # Then, to support this illusion, the labels have the reverse of that 
      # transformation applied (scaling the streamflow y-axis values to the  
      # precipitation values' actual range)
    
      # In this case, since we are maintaining the top-down axis labeling, the
      # breaks set by `ggplot` end up being nice numbers for the precipitation
      # values
    
  }
  
  
  # Regardless of whether 'precipDF' is present, consider incorporating 
  # missing data into the plot (if there are any)
  if (nrow(missingDF) > 0) {
    
    streamPlot <- streamPlot +
      
      geom_col(data = missingDF, 
               mapping = aes(x = get(xCol), y = yBounds[2], alpha = "")) +
      # Use the dates in 'missingDF' and set the "y" values to the maximum 
      # possible streamflow values for the dataset 
      # (Regardless of whether the y-axis is reversed, these columns will 
      #  extend across the entirety of the graph)
      
      scale_alpha_manual(values = 0.2) + 
      guides(alpha = guide_legend(title = "Missing Data"))
      # Use the transparency "alpha" parameter and create a legend for missing data
    
  }
  
  
  # Next, save 'streamPlot' to a file
  
  # The size of the chart should partially depend on the number of records
  
  
  # If the dates in 'streamDF' cover a period of more than 5,000 days, 
  # a larger chart is needed
  if (difftime(max(streamDF[[xCol]]), min(streamDF[[xCol]]), 
               units = "days") > 5000) {
    
    widthFactor <- 10
    heightFactor <- 8
    
  # Otherwise, a smaller dataset can use a smaller chart area
  } else {
    
    widthFactor <- 8
    heightFactor <- 6
    
  }
  
  
  # Save 'streamPlot' to 'writePath'
  ggsave(writePath, streamPlot, units = "px", dpi = 600,
         width = 1080 * widthFactor, height = 720 * heightFactor)
  
  
  # If the file was written successfully, output a message
  if (file.exists(writePath)) {
    
    cat("\n\n")
    
    paste0("Saved plot to \"", writePath, "\" successfully!") |>
      errWrap() |> col_blue() |> cat()
    
    cat("\n\n")
    
  } else {
    
    paste0("Could Not Save Chart\n\n",
           "The script failed to save a plot to \"", writePath, "\" for an ",
           "unknown reason. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



getNiceAxisBreaks <- function (yMax, yMin = 0) {
  
  # Get a couple of nice numbers to use as breakpoints in the primary y-axis
  # (This is for the streamflow data)
  
  # Since the y-axis will start out reversed, the data will be transformed
  # to restore its appearance
  
  # We want nice values for the axis after that transformation is applied
  
  
  # The first step is to find a couple of nice breaks for the normal y-axis
  
  
  # Get the number of digits in the maximum possible y-axis value
  numDigits <- yMax |> round() |> nchar()
  
  
  # Get a nice number divisible by 10 that bounds 'yMax'
  maxNiceBound <- ceiling(yMax / 10^(numDigits - 1)) * 10^(numDigits - 1)
  
  # This formula rounds up to a nice round number
  
  # For example, "2100" and "2900" become "3000"
  # Or "156" and "104" become "200"
  
  # In the case of "2100" and "104", though, 'maxNiceBound' would be too high
  # of a value to use
  
  # Consider how "far along" 'yMax' is between 'maxNiceBound' and one increment
  # down (e.g., "1000" vs "2000" or "80" vs "90")
  
  # If 'yMax' is closer to a nearby nice breakpoint, set 'maxNiceBound' to
  # that other value instead
  
  # For example, "104" is closer to "100" than "200", so use that instead
  # Or, for "140", use "150" instead of "200"
  
  
  # If 'yMax' is less than 25% of the way to 'maxNiceBound', 
  if (yMax < maxNiceBound - 7.5 * 10^(numDigits - 2)) {
    
    # Use one increment lower for 'maxNiceBound'
    maxNiceBound <- maxNiceBound - 10^(numDigits - 1)
    
  # If 'yMax' is closer to the halfway point to 'maxNiceBound'
  } else if (yMax < maxNiceBound - 4 * 10^(numDigits - 2)) {
    
    # Use half an increment lower for 'maxNiceBound'
    maxNiceBound <- maxNiceBound - 5 * 10^(numDigits - 2)
    
  }
  
  
  # Look at 'yMin' next
  
  
  # Look at the difference between 'yMax' and 'yMin' 
  # Find a nearby "nice" starting point for the axis that respects the difference
  # in scale between these two values
  numDigits <- (yMax - yMin) |> round() |> nchar()
  
  
  # Find a number divisible by 10 that is close to (but less than) 'yMin'
  minNiceBound <- floor(yMin / 10^(numDigits - 1)) * 10^(numDigits - 1)
  
  
  # Based on the value of 'minNiceBound' and 'maxNiceBound', 
  # find a preferable number of breaks to include in the y-axis
  numBreaks <- 1:5 |>
    map_lgl(~ (maxNiceBound - minNiceBound) %% . == 0) |>
    which() |> max()
  
  # Considering the numbers 1 through 5, find which numbers divide cleanly with 
  # the chosen values for 'maxNiceBound' and 'minNiceBound'
  
  # After that, the maximum number among those options
  # That will be the number of axis breaks to include in the dataset
  
  
  # The actual final break values are determined here
  breakVals <- seq(from = minNiceBound, 
                   to = maxNiceBound, 
                   length.out = numBreaks)
  
  
  # The numbers in 'breakVals' should be nice round numbers
  
  # However, if these values were input directly into the chart,
  # the labels would not be clean still once the data is rescaled back into
  # a regular primary y-axis
  
  
  # Instead, pretend that 'breakVals' contains the post-transformation 
  # y-axis break values
  
  # To get the actual values to input into the chart, apply the reverse of 
  # normal transformation rescale (though its formulation is the same in both 
  # directions)
  revBreaks <- yMax + yMin - breakVals
  
  
  # Now, 'revBreaks' contains the "pre-transformation" breakpoints
  
  # If we applied these breaks without transforming the streamflow data back
  # from the reversed y-axis, the axis would have some not-so-pretty numbers
  
  # But, post-transformation, we would get the nice round numbers in 'breakVals'
  
  
  # Example: 
  
  # If 'breakVals' had nice numbers like "0", "50", and "100", these would be
  # the numbers we would see after flipping the reversed streamflow data back
  # to normal
  
  # But 'revBreaks' is what would go into the plot 
  
  # If the maximum and minimum streamflow values were "88" and "0", then
  # 'breakVals' would contain "88", "38", and "-12"
  
  # These values will be input into the plot, and after reversing the streamflow
  # data back to normal, we would also have to transform the axis  
  
  # The result of applying the transformation ('yMax' + 'yMin' - 'val') would
  # be "0", "50", and "100"--the nice numbers we wanted in the first place
  
  
  # Return these reversed breaks
  return(revBreaks)
  
}



setPrecipColumnWidths <- function (isDaily, numRecords) {
  
  # Depending on the size and type of the precipitation dataset,
  # use a different column width in the plots
  
  # It would be best to keep column widths at "1.0" (their true size),
  # but they become really hard to see in larger plots
  
  # For monthly data, the columns are more spaced out too
  # (since they are technically plotted on the first of each month)
  # Larger columns help with that
  
  
  # If daily precipitation data is plotted
  if (isDaily) {
    
    # 1 year or less of data
    if (numRecords <= 365) {
      
      return(1)
      
    # 5 years or less of data
    } else if (numRecords <= 365 * 5) {
      
      return(1.5)
      
    # 10 years or less of data
    } else if (numRecords <= 365 * 10) {
      
      return(2.8)
      
    # More than 10 years of data
    } else {
      
      return(3.8)
      
    }
    
  # Otherwise, for monthly precipitation,
  } else {
    
    # 2 years of data or less
    if (numRecords <= 12 * 2) {
      
      return(1.5)
      
    # 5 years of data or less
    } else if (numRecords <= 12 * 5) {
      
      return(3)
      
    # 10 years of data or less
    } else if (numRecords <= 12 * 10) {
      
      return(4.5)
      
    # More than 10 years of data
    } else {
      
      return(5.5)
      
    }
    
  }
  
}



#### Script Execution ####

# Call the procedure for multiple USGS gages
mainProcedure(gageID = "11466800")
mainProcedure(gageID = "11465500")
mainProcedure(gageID = "11465700")


# Clean up
base::remove(list = ls())

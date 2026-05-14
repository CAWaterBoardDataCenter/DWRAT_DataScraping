# Compare data in a SRP gag file to USGS gage data at the same location
# This script is specifically designed for comparing SRP gag files' values to 
# USGS gage 11446680 


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
  cat("Starting 'HLP_010_Compare_SRP_Output_to_USGS_Gage.R'!\n")
  
  
  # Notify the user which USGS gage is being assessed
  cat(paste0("\n\nRunning comparison for USGS Gage ", gageID, "!\n\n"))
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  cat("\n[1/3]\tGetting gag file...\n")
  
  
  # Confirm that the model hydrology folder exists and get its directory path
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Confirm that "SRP_inflow_6.gag" is present in the SRP "output" folder
  gagPath <- paste0(dirPath, "/SRP/output/SRP_inflow_", gageID, ".gag") |>
    checkForPreviousOutput()
  
  
  # Read in the gag file
  gagDF <- read_gag(gagPath)
  
  
  # Validate the contents of 'gagDF'
  # To do this, borrow the "validateGag" function from the Raw Flows script
  functionStealer("Scripts/RRW_016_Generate_Raw_Flows.R", "validateGag")
  functionStealer("Scripts/RRW_016_Generate_Raw_Flows.R", "getColsFromMetadata")
  
  
  gagDF <- gagDF |>
    validateGag(gagPath, dirPath)
  
  # NOTE: `validateGag` will also add a "DATE" column to the file
  
  
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
  
  
  compareGageAndModel(usgsDF, gagDF, dirPath, gageID, gagPath)
  
  
  cat("\tDone!\n\n")
  
  
  
  # Output a completion message
  "'HLP_010_Compare_SRP_Output_to_USGS_Gage.R' is complete!\n\n" |>
    col_green() |>
    cat()
  
  
  # Return nothing
  return(invisible(NULL))
  
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



compareGageAndModel <- function (usgsDF, gagDF, dirPath, gageID, gagPath) {
  
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
  statDF <- generatePlotsAndTable(dailyDF, newDir, "All")
  
  
  # If the dataset contains at least one year of data, 
  # generate a one-year version too
  if (nrow(dailyDF) > 365) {
    
    statDF <- bind_rows(statDF,
                        generatePlotsAndTable(dailyDF, newDir, "1_yr"))
    
  }
  
  
  # If the dataset contains at least five years of data, 
  # generate a five-year version too
  if (nrow(dailyDF) > 365 * 5) {
    
    statDF <- bind_rows(statDF,
                        generatePlotsAndTable(dailyDF, newDir, "5_yr"))
    
  }
  
  
  # If the dataset contains at least ten years of data, 
  # generate a ten-year version too
  if (nrow(dailyDF) > 365 * 10) {
    
    statDF <- bind_rows(statDF,
                        generatePlotsAndTable(dailyDF, newDir, "10_yr"))
    
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
                         str_replace("\\.gag$", "_Processed.gag")),
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



generatePlotsAndTable <- function (dailyDF, newDir, timescale) {
  
  # For the input timescale, produce plots and a table
  
  # Save the plots to 'newDir' and return the table as a tibble
  
  # These actions will be performed for both daily and monthly streamflow datasets
  
  
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
  dailyDF |>
    generateComparisonPlot(paste0(newDir, "/Daily_Comparison_", 
                                  timescale, ".png"),
                           isDaily = TRUE)
  
  
  monthlyDF |>
    generateComparisonPlot(paste0(newDir, "/Monthly_Comparison_", 
                                  timescale, ".png"),
                           isDaily = FALSE)
  
  
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



generateComparisonPlot <- function (streamDF, writePath, isDaily = TRUE,
                                    volUnit = "AF") {
  
  # Generate a plot for 'streamDF' 
  # It can contain either daily or monthly streamflow data
  
  
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
  
  
  # The graph will contain vertical bars at missing dates
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

  
  # Prepare the chart next
  streamPlot <- streamDF |>
    ggplot() +
    geom_line(mapping = aes(x = get(xCol), y = GAGE, color = "Gage"), 
              lwd = 0.8) +
    geom_line(mapping = aes(x = get(xCol), y = MODEL, color = "Model"),
              lwd = 0.8, linetype = 2, alpha = 0.6) + 
    xlab("Date") + ylab(yLabel) +
    guides(color = guide_legend(title = "Flow Type")) +
    scale_color_manual(values = c("Gage" = "blue", "Model" = "red")) + 
    scale_x_date(date_labels = if_else(nrow(streamDF) < 365 * 5, "%Y-%m", "%Y")) + 
    coord_cartesian(ylim = yBounds) +
    theme_gray(base_size = 20)
  
  # The x-axis labels use either "Year-Month" or "Year" depending on the size
  # of 'streamDF'
  
  
  # Add columns to the chart for missing data if 'missingDF' contains values
  if (nrow(missingDF) > 0) {
    
    streamPlot <- streamPlot +
      geom_col(data = missingDF, 
               mapping = aes(x = get(xCol), y = yBounds[2], alpha = "")) +
      scale_alpha_manual(values = 0.2) + 
      guides(alpha = guide_legend(title = "Missing Data"))
    
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



#### Script Execution ####

# Call the procedure for multiple USGS gages
mainProcedure(gageID = "11466800")
mainProcedure(gageID = "11465500")
mainProcedure(gageID = "11465700")


# Clean up
base::remove(list = ls())

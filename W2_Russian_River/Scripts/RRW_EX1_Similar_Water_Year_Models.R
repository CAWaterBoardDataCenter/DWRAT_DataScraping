# Forecasting water availability is a crucial part of SDA's work

# We need some method to predict future precipitation and temperature data
# for the rest of the water year

# These values will affect the streamflow values output by our hydrology models
# as well as the allocations made by DWRAT


# Our current methodology is split into two approaches

# Depending on the amount of data available for the current water year, we apply
# one of these methods


# First, in the beginning of the water year, we use the 
# Standard Precipitation Index (SPI)
# This helps identify the driest period on record for each month

# In a time series of precipitation data, we can extract the worst-case months
# and use these records as our forecast for the current water year

# For this analysis, we need data on a watershed scale--daily precipitation 
# values that represent conditions over the entire watershed

# In this case, we gather PRISM data from cells located entirely *within* the 
# model domain only

# We then calculate the average precipitation across these cells, giving us a 
# time series (from January 1981 onwards) with average precipitation data for the
# entire model domain

# 

# Note: Precipitation data from PRISM often differs from real gage data in terms of
#       magnitude, but since this comparison involves PRISM data vs PRISM data,
#       it is okay (i.e., the timing of precipitation in PRISM is not a problem)






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
  cat("Starting 'RRW_EX2_Outlier_Bounds.R'!\n")
  
  
  # In this script, 'startDate' and 'endDate' are hard-coded
  startDate <- "1990-01-01" |>
    as.Date(format = "%Y-%m-%d")
  
  endDate <- "2025-12-31" |>
    as.Date(format = "%Y-%m-%d")
  
  
  # Start by reading the gage data CSV
  cat("\n[1/3]\tGetting gage data...\n")
  
  
  # Get the path to the "Pre-PRISM" meteorological CSV
  meteorPath <- paste0("ProcessedData/PRMS_Pre-PRISM_Meteorological_",
                       startDate, "_", endDate, ".csv")
  
  
  # Make sure the file exists
  if (!file.exists(meteorPath)) {
    
    paste0("Missing Required Meteorological File\n\n",
           "Please obtain the \"Pre-PRISM\" meteorological file that ",
           "contains gage data from ", startDate, " to ", endDate, ". Place ",
           "it in the \"ProcessedData\" folder.\n\n",
           "(\"", normalizePath(meteorPath, mustWork = FALSE), "\" does ",
           "not exist)") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Read in 'meteorPath'
  meteorDF <- getFile(meteorPath)
  
  
  cat("\tDone!\n\n")
  
  
  # Use 'meteorDF' to determine upper limits for each precipitation gage and month
  cat(paste0("[2/3]\tCalculating outlier bounds...\n"))
  
  
  # Use another function to calculate each bound
  outlierDF <- calcOutlierBounds(meteorDF)
  
  
  cat("\tDone!\n\n")
  
  
  # Save the output to a file
  cat(paste0("[3/3]\tSaving results...\n"))
  
  
  outlierDF |>
    writeOutput("ProcessedData/PRMS_Gage_Outlier_Bounds.csv")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_EX2_Outlier_Bounds.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



calcOutlierBounds <- function (meteorDF) {
  
  # For each precipitation gage and month, set a threshold for outliers
  # Precipitation values are extremely high when the values are above:
  
  # Q3 + 3.5 * IQR
  
  # Note: The upper bounds will be using units of millimeters
  
  
  # First, identify all precipitation columns in 'meteorDF'
  precipCols <- names(meteorDF) |>
    str_subset("^PRECIP[0-9]+$")
  
  
  # Prepare the final output variable as well
  outlierDF <- tibble(GAGE = precipCols)
  
  # 'outlierDF' will have a row for each PRMS precipitation gage
  
  
  # There will be separate upper bound columns in 'outlierDF' for each month
  outlierColNames <- paste0(month.abb, "_OUTLIER_LIMIT_MM") |>
    toupper()
  
  
  outlierDF[outlierColNames] <- NA_real_
  
  
  # For tracking purposes too, record the number of records in 'meteorDF' that
  # would be labeled as outliers using these calculated upper bounds
  countColNames <- paste0(month.abb, "_NUM_OUTLIERS") |>
    toupper()
  
  
  outlierDF[countColNames] <- NA_real_
  
  
  # Iterate through each of the precipitation stations
  for (i in 1:length(precipCols)) {
    
    # Get a subset of the DAT file with just one precipitation column
    subsetDF <- meteorDF |>
      mutate(MONTH = month(DATE)) |> 
      select(DATE, MONTH, all_of(precipCols[i]))
    
    
    # For ease of use, the precipitation column will be renamed to "PRECIP"
    subsetDF <- subsetDF |>
      rename(PRECIP = all_of(precipCols[i]))
    
    
    # After that, filter out missing entries from 'subsetDF'
    # Also, keep precipitation values that are greater than one inch only
    # (1 in = 25.4 mm)
    subsetDF <- subsetDF |>
      filter(!is.na(PRECIP)) |>
      filter(PRECIP > 25.4)
    
    
    # Next, iterate through each month for this gage
    for (j in 1:length(month.abb)) {
      
      # For precipitation gage 'i', filter 'subsetDF' to month 'j'
      # Get Quartile 3 (the 75th percentile) and add 3.5 multiplied by the IQR
      # Then, extract the result and save it in 'outlierDF'
      outlierDF[[outlierColNames[j]]][i] <- subsetDF |>
        filter(MONTH == j) |>
        summarize(OUT_BOUND = quantile(PRECIP, 0.75) + 3.5 * IQR(PRECIP)) |>
        unlist(use.names = FALSE)
      
      
      # Using this newly calculated limit, count the number of precipitation
      # records that would be flagged as outliers
      outlierDF[[countColNames[j]]][i] <- subsetDF |>
        filter(MONTH == j) |>
        filter(PRECIP > outlierDF[[outlierColNames[j]]][i]) |>
        nrow()
      
    } # End of 'j' loop through months
    
  } # End of 'i' loop through gages
  
  
  # Return 'outlierDF'
  return(outlierDF)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

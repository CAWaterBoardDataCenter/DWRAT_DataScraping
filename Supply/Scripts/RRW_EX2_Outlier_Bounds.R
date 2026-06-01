# Currently, the Russian River workflow does not have any QA/QC procedures for 
# precipitation data

# We have only relied on the procedures applied by the original data sources 
# (i.e., NOAA and RAWS--CIMIS includes flags but the data is raw)

# This procedure attempts to develop upper bounds for extremely high values

# These bounds will be specific to each month

# This allows for the variability between dry and wet months to be factored in 
# when determining what counts as an outlier

# Therefore, each of the 15 precipitation gages will have 12 separate upper 
# outlier bounds


# The boundary will be calculated using the Interquartile Range (IQR)
# and this formula:

# Upper Bound = Quartile_3 + 3.5 * IQR

# This is a fairly lax boundary, but California can have some pretty extreme 
# precipitation events from time to time, so some apparent outliers may be
# cases of legitimate variability 


# The automated workflow for the Russian River will rely on the output of this
# procedure, but this script will not be run regularly 

# Its purpose is to document the process that determined these outlier bounds


# Gage data was downloaded on May 21, 2026 for the period between 
# January 1, 1990 and December 31, 2025

# The Pre-QAQC meteorological CSV will be required for this script
# ("PRMS_No-QAQC_Meteorological_1990-01-01_2025-12-31.csv")

# Obtain this file from the appropriate SDA staff and add it to the 
# "ProcessedData" folder 


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
  
  
  # Get the path to the "Pre-QAQC" meteorological CSV
  meteorPath <- paste0("ProcessedData/PRMS_No-QAQC_Meteorological_",
                       startDate, "_", endDate, ".csv")
  
  
  # Make sure the file exists
  if (!file.exists(meteorPath)) {
    
    paste0("Missing Required Meteorological File\n\n",
           "Please obtain the \"No-QAQC\" meteorological file that ",
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
    writeOutput("ProcessedData/RR_Workflow_PRMS_Gage_Outlier_Bounds.csv")
  
  
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
      # That is the outlier bound for this gage and month
      outlierCalc <- subsetDF |>
        filter(MONTH == j) |>
        summarize(OUT_BOUND = quantile(PRECIP, 0.75) + 3.5 * IQR(PRECIP)) |>
        unlist(use.names = FALSE)
      
      
      # However, if 'outlierCalc' is NA, replace it with 3.5 inches as an arbitrary limit
      if (is.na(outlierCalc)) {
        
        # 'outlierCalc' will be NA if it has no precipitation data available
        # for the month
        # Alternatively, it may not have any precipitation values greater
        # than one inch for this month
        
        
        # 3.5 in * 25.4 mm/in
        outlierCalc <- 3.5 * 25.4
        
      }
      
      
      # Then, save 'outlierCalc' to 'outlierDF'
      outlierDF[[outlierColNames[j]]][i] <- outlierCalc
      
      
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

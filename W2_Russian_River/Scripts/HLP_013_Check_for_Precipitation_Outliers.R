# Before running PRMS, check for outliers in the precipitation data

# Notify the user if recent data contains extreme values


# Check each gage station's non-zero dataset for unusual values

# Then, check each day (across stations) for more unusual values


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("W2_Russian_River/Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'HLP_013_Check_for_Precipitation_Outliers.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that a proper directory exists for model input and output files
  # The PRMS model outputs are stored there
  cat("\n[1/2]\tGetting DAT file...\n")
  
  
  # Get the path to that directory
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Get the PRMS DAT file and read it in
  datPath <- paste0(dirPath, "/PRMS/Input/DAT_PRMS_", startDate, "_", 
                    endDate, ".dat") |>
    checkForPreviousOutput()
  
  
  datDF <- datPath |>
    read_dat()
  
  
  # Validate 'datDF' as well
  
  # However, it will need some adjustments 
  # (all headers capitalized and "SEC" renamed to "SECOND")
  datDF <- datDF |>
    set_names(names(datDF) |> toupper()) |>
    rename(SECOND = SEC)
  
  
  # Next, apply the validation function
  # (This function adds a "DATE" column too)
  datDF <- datDF |>
    validateInputDAT(datPath, "PRMS", 
                     names(datDF) |> str_subset("((precip|tm))"),
                     startDate, endDate, "Final")
  
  
  cat("\tDone!\n\n")
  
  
  # The next step is to search for outliers in the precipitation data
  cat("[2/2]\tValidating precipitation data...\n")
  
  
  # Check each precipitation column in 'datDF'
  outlierDF <- precipOutlierCheck(datDF)
  
  
  outlierDF |>
    writeOutput("W2_Russian_River/Output/PRMS_Outlier_Bounds.csv")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("'HLP_013_Check_for_Precipitation_Outliers.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



precipOutlierCheck <- function (datDF) {
  
  # 
  
  
  # Get all precipitation columns in 'datDF'
  precipCols <- names(datDF) |>
    str_subset("^PRECIP[0-9]+$")
  
  
  outlierDF <- tibble(GAGE = precipCols)
  
  

  outlierDF[paste0(toupper(month.abb), "_OUTLIER_LIMIT")] <- NA_real_

  outlierDF[paste0(toupper(month.abb), "_NUM_OUTLIERS")] <- NA_real_

  # outlierDF |>
  #   select(GAGE,
  #          starts_with("JAN"), starts_with("FEB"), starts_with("MAR"),
  #          starts_with("APR"), starts_with("MAY"), starts_with("JUN"))
  # 
  
  for (i in 1:length(precipCols)) {
    
    
    # Get a subset of the DAT file with just one precipitation column
    subsetDF <- datDF |>
      select(DATE, MONTH, all_of(precipCols[i])) |>
      filter(get(precipCols[i]) > 25.4) |>
      rename(PRECIP = all_of(precipCols[i]))
    
    
    for (j in 1:length(month.abb)) {
      
      outlierDF[[paste0(toupper(month.abb[j]), "_OUTLIER_LIMIT")]][i] <- 
        subsetDF |>
        filter(MONTH == j) |>
        summarize(OUTLIER_LIMIT = quantile(PRECIP, 0.75) + 3.5 * IQR(PRECIP)) |>
        unlist(use.names = FALSE)
      
      
      outlierDF[[paste0(toupper(month.abb[j]), "_NUM_OUTLIERS")]][i] <- subsetDF |>
        filter(MONTH == j) |>
        filter(PRECIP > outlierDF[[paste0(toupper(month.abb[j]), "_OUTLIER_LIMIT")]][i]) |>
        nrow()
      
    }
    
  }
  
  
  # For each precipitation column, calculate overall and month-based average
  return(outlierDF)
  
  
  
  # 
  
  
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

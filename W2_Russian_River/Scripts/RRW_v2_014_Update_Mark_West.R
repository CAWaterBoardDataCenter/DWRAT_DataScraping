# This script sets up the Mark_West_inflow DAT file that is input into RRIHM's
# modflow component

# An output for Sub-Basin 24 from SRP ("SRP_inflow_6.gag") is used to update
# this file with new flow data

# Note: The SRP output uses cfs, while RRIHM requires m^3/s


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_v2_014_Update_Mark_West.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Check for the directory that contains metadata and model input/output files
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Start by reading in the required files
  cat("\n[1/3]\tGathering files...\n")
  
  
  # From the SRP model outputs, get the path to "SRP_inflow_6.gag"
  # Data from this file will be added to an RRIHM DAT file
  gagPath <- paste0(dirPath, "/SRPHM/Output/SRP_inflow_6.gag") |>
    checkForPreviousOutput()
  
  
  # Also confirm that the "RRIHM" folder was copied to "Output"
  # Get the path to that folder
  rrPath <- validate_model_copy("RRIHM")
  
  
  # Locate "Mark_West_inflow.dat" within the "RRIHM" folder
  # (This file was confirmed to exist via `validateModelCopy_RRIHM`)
  markPath <- paste0(rrPath, "/modflow/input/",
                     "Mark_West_inflow.dat") |>
    checkForPreviousOutput()
  
  
  # Read in and validate the two files next
  
  
  # Start with the gag file
  gagDF <- read_gag(gagPath)
  
  
  # Validate the contents of 'gagDF'
  # To do this, borrow the "validateGag" function from the Raw Flows script
  c("validateGag", "getColsFromMetadata") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_017_Generate_Raw_Flows.R", .))
  
  
  gagDF <- gagDF |>
    validateGag(gagPath, dirPath)
  
  # NOTE: `validateGag` will also add a "DATE" column to the file
  #       It relies on the metadata file's SRP model start date column for that
  
  
  # Read in the DAT file next
  # Its values are tab-separated
  markDF <- markPath |>
    read_dat()
  
  
  # Validate 'markDF' next
  markDF <- markDF |>
    validateMark()
  
  
  cat("\tDone!\n\n")
  
  
  # Convert the streamflow data in 'gagDF' into m^3/s
  # Then integrate this data into 'markDF'
  cat("[2/3]\tUpdating Mark West DAT file...\n")
  
  markDF <- markDF |>
    integrate_SRP_streamflow(gagDF)
  
  
  cat("\tDone!\n\n")
  
  
  # Finally, save the updated DAT file
  # (both locally and in the archive hydrology folder)
  cat("[3/3]\tSaving updates...\n")
  
  
  # Remove the "DATE" column from 'markDF'
  # (It is no longer needed)
  markDF <- markDF |>
    select(-DATE)
  
  
  # Write 'markDF' back to 'markPath' first
  markDF |>
    writeOutput(markPath, writeFunction = "write_tsv", col_names = FALSE)
  
  
  # Then, write 'markDF' to the archive folder ('dirPath')
  archivePath <- paste0(dirPath, "/RRIHM/Input", 
                        markPath |> extract_filename())
  
  
  markDF |>
    writeOutput(archivePath, writeFunction = "write_tsv", col_names = FALSE)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_014_Update_Mark_West.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateMark <- function (markDF, markPath) {
  
  # Validate a Mark West DAT file for RRIHM
  
  # The original file should contain three columns 
  # (And `read_dat` can add a "DATE" column)
  
  
  # Return 'markDF' with column name edits (and potentially a new "DATE" column)
  
  
  # These are the columns that should have been imported from 'markDF'
  # ("X1", "X2", and "X3" are the expected names)
  # (The other strings are what they would be renamed to)
  expectedCols <- c("Time" = "X1", "Flow" = "X2", "Date" = "X3")
  
  
  # Check if any columns are missing
  if (!all(expectedCols %in% names(markDF))) {
    
    paste0("DAT File - Column Issue\n\n",
           "The Mark West DAT file for RRIHM does not have all ", 
           length(expectedCols), " of the required columns (missing ",
           vec2QuotedStr(expectedCols[!(expectedCols %in% names(markDF))]), 
           "). Please investigate this file.\n\n", 
           "(This error occurred for \"", markPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Make sure no "NA" values are present in 'markDF'
  if (anyNA(markDF)) {
    
    # Output the indices of the rows and columns where "NA" values were detected
    cat("\n\n")
    cat("Missing Value Indices:\n")
    print(which(is.na(markDF), arr.ind = TRUE))
    cat("\n\n")
    
    
    paste0("DAT File - Missing Data Issue\n\n",
           "The Mark West DAT file for RRIHM has one or more entries with ",
           "\"NA\" values. Please check the entries noted above and investigate ",
           "the file.\n\n", 
           "(This error occurred for \"", markPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, update the column names in 'markDF'
  # (These renames are meant to match the naming conventions in GAG files)
  markDF <- markDF |>
    rename(all_of(expectedCols))
  
  
  # Check the "Time", "Flow", and "Date" columns for proper formatting
  
  
  # The "Time" column should contain integers that increment from 0 onwards
  # The final number should be one less than the total number of rows in 'markDF'
  if (!all(0:(nrow(markDF) - 1) == markDF[["Time"]]) ||
      !is.numeric(markDF[["Time"]])) {
    
    paste0("DAT File - Time Increment Issue\n\n",
           "The first column in the Mark West DAT file should contain integers ",
           "that start from 0 and end at ", nrow(markDF) - 1, ". Its type should ",
           "be numeric too. However, this was not the case. Please investigate ",
           "the file.\n\n", 
           "(This error occurred for \"", markPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The "Flow" column should contain non-negative numbers
  if (!is.numeric(markDF[["Flow"]]) || any(markDF[["Flow"]] < 0)) {
    
    paste0("DAT File - Flow Column Issue\n\n",
           "The second column in the Mark West DAT file should contain ",
           "non-negative flow volumes (in m^3/s). However, this was not the ",
           "case. Please investigate.\n\n", 
           "(This error occurred for \"", markPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there are no issues, return 'markDF'
  # (It now has updated column names)
  return(markDF)
  
}



integrate_SRP_streamflow <- function (markDF, gagDF) {
  
  # Convert the streamflow data in 'gagDF' from cfs to m^3/s
  
  # Then, for overlapping dates, replace the values in 'markDF' with
  # the "Flow" values in 'gagDF'
  
  
  # First, update the units in 'gagDF'
  # There are 35.3147 cubic feet in one cubic meter
  # ft^3 / s  *  1 m^3 / 35.3147 ft^3  =  m^3 / s
  gagDF <- gagDF |>
    mutate(Flow = Flow / 35.3147)
  
  
  # Before proceeding, 
  # ensure that both 'markDF' and 'gagDF' are sorted properly by date
  
  # In 'gagDF' too, only the "DATE" and "Flow" columns are required
  gagDF <- gagDF |>
    select(DATE, Flow) |>
    arrange(DATE)
  
  markDF <- markDF |>
    arrange(DATE)
  
  
  # Create a subset of 'markDF' with just the dates that appear in 'gagDF'
  # Then, remove the "Flow" column in 'markDF', and join it with 'gagDF'
  # to add a new "Flow" column that has the SRPHM values
  newRows <- markDF |>
    filter(DATE %in% gagDF$DATE) |>
    select(-Flow) |>
    left_join(gagDF, by = "DATE", relationship = "one-to-one")
  
  # (A join ensures that flow values from 'gagDF' are linked to the correct dates' 
  #  rows in 'markDF')
  
  
  # Finally, redefine 'markDF' using 'newRows'
  
  # Keep the rows in 'markDF' that have a date outside of the date range in 'gagDF'
  # But use 'newRows' to replace the overlapping dates
  markDF <- bind_rows(markDF |> filter(DATE %notin% gagDF$DATE),
                      newRows) |>
    arrange(DATE)
  
  # (Sorting by date is likely required afterwards too)
  
  
  # Return 'markDF'
  return(markDF)
  
}


#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())




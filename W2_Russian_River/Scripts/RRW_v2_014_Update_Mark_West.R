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
  markPath <- paste0(rrPath, "/modflow/input/",
                     "Mark_West_inflow.dat") |>
    checkForPreviousOutput()
  
  
  # Read in and validate the two files next
  
  
  # Start with the gag file
  gagDF <- read_gag(gagPath)
  
  
  # Validate the contents of 'gagDF'
  # To do this, borrow the "validateGag" function from the Raw Flows script
  functionStealer("W2_Russian_River/Scripts/RRW_017_Generate_Raw_Flows.R", "validateGag")
  
  
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
    validate_rrihm_dat()
  
  
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




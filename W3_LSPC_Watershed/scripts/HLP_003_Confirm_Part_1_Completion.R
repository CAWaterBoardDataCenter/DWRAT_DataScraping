# The "Part 2" workflow should only be executed if the "Part 1" portion has 
# been completed

# There are two indirect confirmations for this:
#  (1) There should be a text file that indicates the presence of an archive location
#  (2) Manual review files should exist for each watershed


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'HLP_003_Confirm_Part_1_Completion.R'!\n")
  
  
  # Import functions from other scripts
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R",
                  "read_all_lspc_project_control")
  
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_012_Archive_Raw_and_Staged_Files.R", 
                  "get_lspc_archive_folder")
  
  
  # Import the data scraping bounds next
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Run tests to confirm that the previous scripts were run
  cat("\n[1/2]\tChecking for the archive folder...\n")
  
  
  # Perform that check in another function
  check_for_archive_folder(startDate, endDate)
  
  
  cat("\tDone!\n\n")
  
  
  # Next, take a look at the QC spreadsheet directories for each watershed
  cat("[2/2]\tChecking for QC spreadsheets...\n")
  
  
  # Use a separate function again
  check_for_qc_dirs()
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'HLP_003_Confirm_Part_1_Completion.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



check_for_archive_folder <- function (startDate, endDate) {
  
  # Confirm that an archive folder was established for the workflow run
  
  
  # Use the user-specified start date and end dates and look for its directory
  tryRes <- catch_warnings_and_errors(
    get_lspc_archive_folder(startDate, endDate)
  )
  
  
  # If 'tryRes' contains an error, then the prior scripts were not executed
  error_if(caught_issue(tryRes),
           
           paste0("Previous Scripts Not Run\n\n",
                  "It does not appear that the \"Part 1\" scripts were fully ",
                  "executed before starting with the \"Part 2\" scripts. An ",
                  "archive text file could not be found for the user-specified ",
                  "data scraping bounds (", startDate, " and ", endDate, "). ",
                  "Please investigate."))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



check_for_qc_dirs <- function () {
  
  # Confirm that each project watershed has a QC file directory
  # Otherwise, output an error message
  
  
  # Read in the weather control spreadsheet
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validation function for weather control file 
  
  
  # Use the imported function to get storage information for every watershed
  wsDir <- controlDF |>
    read_all_lspc_project_control(worksheet = "Storage")
  
  # To Do: Validation of storage worksheets
  
  
  # Get the paths to the "QCSpreadsheets" directory for each watershed
  qcDir <- wsDir |>
    map_chr(~ paste0("W3_LSPC_Watershed/",
                     filter(., scope == "project" & level == "root") |>
                       select(path) |> unlist(use.names = FALSE),
                     "/",
                     filter(., scope == "project" & level == "candidate" & source == "gage") |>
                       select(path) |> unlist(use.names = FALSE),
                     "/QCSpreadsheets"))
  
  
  # Confirm that each QAQC directory exists
  error_if(!all(dir.exists(qcDir)),
           paste0("Could Not Locate QC Folders\n\n",
                  "Each watershed should have a directory within their ",
                  "project folder that stores manual review spreadsheets ",
                  "for \"candidate\" gage data. However, this is not the case for ", 
                  controlDF$project_name[!dir.exists(qcDir)] |> vec2QuotedStr(),
                  ". Please investigate. This suggests that the \"Part 1\" ",
                  "scripts were not executed."))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

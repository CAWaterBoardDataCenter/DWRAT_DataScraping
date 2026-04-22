# After a Paradigm DWRAT run has completed successfully, 
# perform final post-processing steps here

# The Anaconda environment used to run DWRAT is archived at this step

# Optionally, if "ADDITIONAL_ARCHIVE_LOCATION" has value in the RR control file,
# the contents of the hydrology folder are copied elsewhere too


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
  cat("Starting 'RRW_020_DWRAT_Cleanup.R'!\n\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that the model hydrology folder exists and get its path
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # After that, get the path to Anaconda's "activate.bat" script
  batPath <- detectAnacondaBat()
  
  
  # Check if a value has been supplied to the optional field 
  # "ADDITIONAL_ARCHIVE_LOCATION" in the workflow control file
  # If that's the case, the script will run an extra procedure
  # to copy the files from 'dirPath' to 'extraDir'
  extraDir <- checkForAdditionalArchive()
  
  # 'extraDir' will be "NA" if this extra procedure will NOT be run
  
  
  # Output a message about exporting the Anaconda environment to a file
  cat(paste0("[1/", 
             if_else(is.na(extraDir), 1, 2), 
             "]\tExporting Anaconda Environment to YAML File...\n"))
  
  
  # Use "conda export" to write an "environment.yml" file in the hydrology
  # output folder
  ymlRes <- system(paste0(batPath, " && ",
                          "conda export -n paradigm-dwrat -f ", 
                          paste0(dirPath, "/environment.yml") |> 
                            normalizePath(mustWork = FALSE) |> shQuote()), 
                   intern = TRUE)
  
  
  # 'ymlRes' should be empty
  # Otherwise, an error may have occurred
  if (length(ymlRes) > 0) {
    
    # Include the output message in the console 
    cat("\n\nOutput Message(s):\n\n")
    print(ymlRes)
    
    
    paste0("Anaconda Export Error\n\n",
           "An error was encountered while exporting the \"paradigm-dwrat\" ",
           "environment to a file. Please investigate the model's output ",
           "messages above.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The task is complete
  cat("\tDone!\n\n")
  
  
  # If 'extraDir' is not NA, there is an additional procedure to run
  # The files in 'dirPath' will be stored in this secondary folder location too
  if (!is.na(extraDir)) {
    
    cat("[2/2]\tCopying archived files to another folder...\n")
    
    dir_copy(path = dirPath,
             new_path = paste0(extraDir, "/RR_Workflow"), 
             overwrite = TRUE)
    
    cat("\tDone!\n\n")
    
  }
  
  
  # Output a completion message
  cat(col_green("\n'RRW_020_DWRAT_Cleanup.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



checkForAdditionalArchive <- function () {
  
  # Check if a user supplied a valid directory to "ADDITIONAL_ARCHIVE_LOCATION"
  
  # If so, a process will be run later to copy the archived files in the model
  # hydrology output folder into this additional location
  
  # This function will return either the location to this extra directory
  # or "NA" if this process will not be used
  
  
  # First, check if the user supplied a value in the control file
  extraDir <- getFromControl_RR("ADDITIONAL_ARCHIVE_LOCATION")
  
  
  # If 'extraDir' is "NA", return "NA" and do not use this procedure
  if (is.na(extraDir)) {
    
    return(extraDir)
    
  }
  
  
  # If 'extraDir' contains a value, make sure it points to a valid directory
  # If not, "NA" will still be returned
  
  
  # First check whether 'extraDir' is a SharePoint folder
  extraDir <- sharepointPathCheck(extraDir, isFolder = TRUE)
  
  
  # If the directory does not exist, notify the user and then return "NA"
  if (!dir.exists(extraDir)) {
    
    paste0("Additional Archive Location Does Not Exist\n\n",
           "The folder \"", extraDir, "\" does not appear to exist. ",
           "Therefore, no copies will be made of the files stored ", 
           "in the main hydrology output folder.\n\n",
           "Please correct the path in \"ADDITIONAL_ARCHIVE_LOCATION\" ",
           "of the control file to enable this part of the procedure.") |>
      errWrap() |>
      message()
    
    cat("\n\n")
    
    
    # Return "NA"
    return(NA)
    
  }
  
  
  # Otherwise, return 'extraDir' (as a normalized path)
  return(extraDir |> normalizePath())
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())



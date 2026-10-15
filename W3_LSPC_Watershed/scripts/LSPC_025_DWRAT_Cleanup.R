# Perform some final cleanup for the workflow

# Save Anaconda YML files to the archive folder

# Update metadata too for the procedure


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
  cat("Starting 'LSPC_025_DWRAT_Cleanup.R'!\n")
  
  
  # Import functions from other scripts
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_012_Archive_Raw_and_Staged_Files.R",
                  "get_lspc_archive_folder")
  
  c("check_for_additional_archive", "export_anaconda_env", 
    "update_metadata_compilation") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_021_DWRAT_Cleanup.R", .))
  
  
  # Import the user-specified start and end dates
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Get the location of the archive folder
  dirPath <- get_lspc_archive_folder(startDate, endDate)
  
  
  # After that, get the path to Anaconda's "activate.bat" script
  batPath <- detectAnacondaBat()
  
  
  # Check if a value has been supplied to the optional field 
  # "ADDITIONAL_ARCHIVE_LOCATION" in the workflow control file
  # If that's the case, the script will run an extra procedure
  # to copy the files from 'dirPath' to 'extraDir'
  extraDir <- get_from_lspc_master_control("ADDITIONAL_ARCHIVE_LOCATION") |>
    check_for_additional_archive()
  
  # 'extraDir' will be "NA" if this extra procedure will NOT be run
  
  
  # Output a message about exporting the Anaconda environments to a file
  cat(paste0("[1/", 
             if_else(is.na(extraDir), 2, 3), 
             "]\tExporting Anaconda Environments to YAML Files...\n"))
  
  
  # Make YAML files for both Python environments used by this workflow
  # (Paradigm DWRAT and LSPC Climate Processing Restructure)
  
  export_anaconda_env(batPath, dirPath, "paradigm-dwrat",
                      paste0("Paradigm_DWRAT_Environment.yml"))
  
  
  export_anaconda_env(batPath, dirPath, "lspc-climate-processing-restructure",
                      paste0("LSPC_Climate_Processing_Environment.yml"))
  
  
  cat("\tDone!\n\n")
  
  
  # If 'extraDir' is not NA, there is an additional procedure to run
  # The files in 'dirPath' will be stored in this secondary folder location too
  if (!is.na(extraDir)) {
    
    cat("[2/3]\tCopying archived files to another folder...\n")
    
    
    # The model hydrology folder will be stored under "LSPC_Workflow" in 'extraDir'
    newDir <- paste0(extraDir, "/LSPC_Workflow")
    
    
    # Delete the "LSPC_Workflow" folder if it already exists 
    if (dir.exists(newDir)) {
      
      dir_delete(newDir)
      
    }
    
    
    # Copy 'dirPath' to 'newDir'
    dir_copy(path = dirPath, new_path = newDir, overwrite = TRUE)
    
    
    cat("\tDone!\n\n")
    
  }
  
  
  # Finally, save metadata about the completed run
  cat(paste0("[", if_else(is.na(extraDir), "2/2", "3/3"), 
             "]\tAdding final metadata...\n"))
  
  
  # Add one more column of metadata
  updateMetadataCSV(dirPath,
                    newCols = list("APPROXIMATE_COMPLETION_TIME" = Sys.time()))
  
  
  # Then, if "LONG-RUNNING_METADATA_FILE_LOCATION" is not empty, 
  # update or create "RR_Workflow_Tracker.csv" with this run's metadata
  if (!is.na(get_from_lspc_master_control("LONG-RUNNING_METADATA_FILE_LOCATION"))) {
    
    update_metadata_compilation(dirPath,
                                get_from_lspc_master_control("LONG-RUNNING_METADATA_FILE_LOCATION"),
                                "LSPC_Workflow_Tracker.csv")
    
  }
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_025_DWRAT_Cleanup.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

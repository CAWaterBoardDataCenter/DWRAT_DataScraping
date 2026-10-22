# After a Paradigm DWRAT run has completed successfully, 
# perform final post-processing steps here

# The Anaconda environment used to run DWRAT is archived at this step

# Optionally, if "ADDITIONAL_ARCHIVE_LOCATION" has value in the RR control file,
# the contents of the hydrology folder are copied elsewhere too


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
  cat("Starting 'RRW_021_DWRAT_Cleanup.R'!\n\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that the model hydrology folder exists and get its path
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # After that, get the path to Anaconda's "activate.bat" script
  batPath <- detectAnacondaBat()
  
  
  # Check if a value has been supplied to the optional field 
  # "ADDITIONAL_ARCHIVE_LOCATION" in the workflow control file
  # If that's the case, the script will run an extra procedure
  # to copy the files from 'dirPath' to 'extraDir'
  extraDir <- getFromControl_RR("ADDITIONAL_ARCHIVE_LOCATION") |>
    check_for_additional_archive()
  
  # 'extraDir' will be "NA" if this extra procedure will NOT be run
  
  
  # Output a message about exporting the Anaconda environment to a file
  cat(paste0("[1/", 
             if_else(is.na(extraDir), 2, 3), 
             "]\tExporting Anaconda Environment to YAML File...\n"))
  
  
  # Use "conda export" to write an "environment.yml" file in the hydrology
  # output folder
  export_anaconda_env(batPath, dirPath, "paradigm-dwrat")
  
  
  # The task is complete
  cat("\tDone!\n\n")
  
  
  # If 'extraDir' is not NA, there is an additional procedure to run
  # The files in 'dirPath' will be stored in this secondary folder location too
  if (!is.na(extraDir)) {
    
    cat("[2/3]\tCopying archived files to another folder...\n")
    
    
    # The model hydrology folder will be stored under "RR_Workflow" in 'extraDir'
    newDir <- paste0(extraDir, "/RR_Workflow")
    
    
    # Delete the "RR_Workflow" folder if it already exists 
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
  if (!is.na(getFromControl_RR("LONG-RUNNING_METADATA_FILE_LOCATION"))) {
    
    update_metadata_compilation(dirPath,
                                getFromControl_RR("LONG-RUNNING_METADATA_FILE_LOCATION"))
    
  }
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_021_DWRAT_Cleanup.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



check_for_additional_archive <- function (extraDir) {
  
  # Check if a user supplied a valid directory to "ADDITIONAL_ARCHIVE_LOCATION"
  
  # If so, a process will be run later to copy the archived files in the model
  # hydrology output folder into this additional location
  
  # This function will return either the location to this extra directory
  # or "NA" if this process will not be used
  
  
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
    
    cat("\n\n")
    
    paste0("Additional Archive Location Does Not Exist\n\n",
           "The folder \"", extraDir, "\" does not appear to exist. ",
           "Therefore, no copies will be made of the files stored ", 
           "in the main hydrology archive folder.\n\n",
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



export_anaconda_env <- function (batPath, dirPath, envName, 
                                 outName = "environment.yml") {
  
  # Export an Anaconda environment to a YML file
  
  # The "conda export" command can help with this
  # https://docs.conda.io/projects/conda/en/latest/commands/export.html
  
  
  # The YML file will originally be written as "environment.yml"
  # Otherwise, there may be an "EnvironmentExporterNotDetected" error
  # It will be renamed afterwards if 'outName' is something different
  tempName <- "environment.yml"
  
  
  # Submit the command to export the environment to a file
  ymlRes <- system(paste0(batPath, " && ",
                          "conda export -n ", envName, " -f ", 
                          paste0(dirPath, "/", tempName) |> 
                            normalizePath(mustWork = FALSE) |> shQuote()), 
                   intern = TRUE)
  
  
  # 'ymlRes' should be empty or only contain warning messages
  # Otherwise, an error may have occurred
  if (length(ymlRes) > 0) {
    
    # Include the output message in the console 
    cat("\n\nOutput Message(s) from Export Operation:\n\n")
    print(ymlRes)
    
    
    # Check if the word "error" appears anywhere in 'ymlRes'
    error_if(any(grepl("Error", ymlRes, ignore.case = TRUE)),
             paste0("Anaconda Export Error\n\n",
                    "An error was encountered while exporting the \"", envName, 
                    "\" environment to a file. Please investigate the output ",
                    "messages above."))
    
  }
  
  
  # If 'outName' is not "environment.yml", rename the file to match 'outName'
  if (outName != tempName) {
    file.rename(paste0(dirPath, "/", tempName),
                paste0(dirPath, "/", outName))
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



update_metadata_compilation <- function (dirPath, fileLocation,
                                         fileName = "RR_Workflow_Tracker.csv") {
  
  # Add data from the current workflow run's "metadata.csv" file to
  # "RR_Workflow_Tracker.csv", which is stored in the folder
  # identified by "LONG-RUNNING_METADATA_FILE_LOCATION"
  
  
  # First, locate the folder that contains "RR_Workflow_Tracker.csv"
  trackerDir <- fileLocation |>
    sharepointPathCheck(isFolder = TRUE)
  
  
  # If the directory does not exist, notify the user and then end the procedure
  if (!dir.exists(trackerDir)) {
    
    paste0("Long-Running Metadata CSV Location Does Not Exist\n\n",
           "The folder \"", trackerDir, "\" does not appear to exist. ",
           "Therefore, metadata from this workflow run will remain in ",
           "\"metadata.csv\" in \"", dirPath, "\". That information will ",
           "NOT be copied into any long-running file.\n\n",
           "Please correct the path in \"LONG-RUNNING_METADATA_FILE_LOCATION\" ",
           "of the control file to enable this part of the procedure.") |>
      errWrap() |>
      message()
    
    cat("\n\n")
    
    
    # Return nothing
    return(invisible(NULL))
    
  }
  
  
  # Otherwise, read in "metadata.csv"
  metaDF <- paste0(dirPath, "/metadata.csv") |>
    normalizePath(mustWork = TRUE) |>
    getFile()
  
  
  # Then, check if 'fileName' already exists in 'trackerDir'
  compiledPath <- paste0(trackerDir, "/", fileName) |>
    normalizePath(mustWork = FALSE)
  
  
  # If the file already exists, read it in and append 'metaDF' to its rows
  if (file.exists(compiledPath)) {
    
    # Read in 'compiledDF'
    compiledDF <- compiledPath |>
      getFile()
    
    
    # Before performing the append operation,
    # check the columns shared between 'compiledDF' and 'metaDF'
    # Make sure they are the same type (otherwise `bind_rows` may fail)
    sharedCols <- base::intersect(names(compiledDF), names(metaDF))
    
    
    # Iterate through all shared columns, if there are any
    if (length(sharedCols) > 0) {
      
      for (i in 1:length(sharedCols)) {
        
        # If the types of the shared column match, skip to the next iteration
        if (typeof(compiledDF[[sharedCols[i]]]) == 
            typeof(metaDF[[sharedCols[i]]])) {
          next
        }
        
        
        # Otherwise, convert the column to "character" in both variables
        compiledDF[[sharedCols[i]]] <- compiledDF[[sharedCols[i]]] |>
          as.character()
        
        metaDF[[sharedCols[i]]] <- metaDF[[sharedCols[i]]] |>
          as.character()
        
      }
      
    }
    
    
    # Bind 'metaDF' to 'compiledDF'
    compiledDF <- compiledDF |>
      bind_rows(metaDF)
    
    
    # If the file does NOT exist yet, use 'metaDF' to establish it
  } else {
    
    compiledDF <- metaDF
    
  }
  
  
  # Write 'compiledDF' back to 'compiledPath'
  writeOutput(compiledDF, compiledPath)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

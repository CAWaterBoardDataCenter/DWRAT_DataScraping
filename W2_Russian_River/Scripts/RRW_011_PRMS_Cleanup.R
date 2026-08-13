# After the PRMS run has completed successfully, 
# copy key output files into the hydrology model input/output folder

# Then, delete the "RR_PRMS" folder from the "Output" folder


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
  cat("Starting 'RRW_011_PRMS_Cleanup.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that a proper directory exists for model input and output files
  # The actual PRMS model files should have been successfully copied to
  # the "Output" folder too
  cat("[1/3]\tChecking directories...\n")
  
  
  # Check for the directory that contains metadata and model input/output files
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Also confirm that the "RR_PRMS" folder was copied to "Output"
  prmsPath <- validate_model_copy("PRMS")
  
  
  cat("\tDone!\n\n")
  
  
  cat("[2/3]\tCopying output files...\n")
  
  
  # Copy output files into the hydrology folder
  copyOutputs(prmsPath, dirPath, startDate, endDate)
  
  
  cat("\tDone!\n\n")
  
  
  # The final step is to delete the "RR_PRMS" folder that was copied to 
  # the "Output" folder
  cat("[3/3]\tDeleting the model files...\n")
  
  
  deleteFiles(prmsPath, "PRMS")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_011_PRMS_Cleanup.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



copyOutputs <- function (prmsPath, dirPath, startDate, endDate) {
  
  # Copy several files from the PRMS "output" folder (plus the control file)
  # into the hydrology folder 
  
  
  # Confirm that they exist in the "output" folder first
  check_for_model_outputs("PRMS", prmsPath, modelOutput = NULL,
                          includeScriptGeneratedOutput = TRUE)
  
  
  # This vector contains the paths of the key output files 
  # (and the script-generated log)
  copyFiles <- list_model_outputs("PRMS", prmsPath, 
                                  includeScriptGeneratedOutput = TRUE)
  
  
  # Prepare vectors that contain the proper filepaths and the planned filepaths
  sourcePaths <- copyFiles |>
    normalizePath(mustWork = TRUE)
  
  
  # For the "sub_cfs" and "sub_inq" files, include the model scraping bounds 
  # in their archive filenames
  writePaths <- copyFiles |>
    str_replace("RR_PRMS_Output(?=_sub_((cfs)|(inq))\\.csv$)",
                paste0("RR_PRMS_Output_", startDate, "_", endDate))
  
  # The regular expression checks for "RR_PRMS_Output" in the file paths,
  # with a lookahead regex specifically matching "sub_cfs.csv" and "sub_inq.csv" 
  
  # (This is needed in case "RR_PRMS_Output_" would match with a portion of the
  #  path rather than the actual filename)
  
  
  # After that, replace the paths in 'writePaths' using 'dirPath'
  # The files will be written to the PRMS "Output" folder
  writePaths <- writePaths |>
    str_remove("^.+[/\\\\]") |>
    paste0(dirPath, "/PRMS/Output/", ... = _) |>
    normalizePath(mustWork = FALSE)
  
  
  # Add rows to 'sourcePaths' and 'writePaths' for "prms_rr.control"
  sourcePaths <- c(sourcePaths,
                   paste0(prmsPath, "/windows/prms_rr.control") |>
                     checkForPreviousOutput())
  
  
  writePaths <- c(writePaths,
                  paste0(dirPath, "/PRMS/Input/prms_rr.control"))
  
  
  # Copy the files using the `copyFile` function
  # If any of these actions fail, the function will trigger an error 
  map2(sourcePaths, writePaths, copyFile)
  
  
  # Return nothing if there were no issues
  return(invisible(NULL))
  
}



deleteFiles <- function (modelPath, model) {
  
  # Delete the model's directory in the "Output" folder
  
  # Start by deleting that folder
  dir_delete(modelPath)
  
  
  # Confirm that it was deleted
  if (dir.exists(modelPath)) {
    
    stop(paste0("Failed to Delete ", model, " Directory\n\n",
                "The script attempted to delete the ", model, " model files that ",
                "were located in the \"Output\" folder. However, it ",
                "was unsuccessful for an unknown reason. Please investigate.\n\n",
                "(This error occurred for \"", modelPath, "\")") |>
           errWrap())
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}


#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

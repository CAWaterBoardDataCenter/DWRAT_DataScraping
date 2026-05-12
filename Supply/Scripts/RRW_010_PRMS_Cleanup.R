# After the PRMS run has completed successfully, 
# copy key output files into the hydrology model input/output folder

# Then, delete the "RR_PRMS" folder from the "ProcessedData" folder


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
  cat("Starting 'RRW_010_PRMS_Cleanup.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that a proper directory exists for model input and output files
  # The actual PRMS model files should have been successfully copied to
  # the "ProcessedData" folder too
  cat("[1/3]\tChecking directories...\n")
  
  
  # Check for the directory that contains metadata and model input/output files
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Also confirm that the "RR_PRMS" folder was copied to "ProcessedData"
  prmsPath <- validateModelCopy_PRMS()
  
  
  cat("\tDone!\n\n")
  
  
  cat("[2/3]\tCopying output files...\n")
  
  
  # Copy output files into the hydrology folder
  copyOutputs(prmsPath, dirPath, startDate, endDate)
  
  
  cat("\tDone!\n\n")
  
  
  # The final step is to delete the "RR_PRMS" folder that was copied to 
  # the "ProcessedData" folder
  cat("[3/3]\tDeleting the model files...\n")
  
  
  deleteFiles(prmsPath)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_010_PRMS_Cleanup.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



copyOutputs <- function (prmsPath, dirPath, startDate, endDate) {
  
  # Copy several files from the PRMS "output" folder
  # into the hydrology folder 
  
  
  # This vector contains the names of the desired output files
  copyFiles <- c("rr_budget.out2", 
                 "gsflow.csv",
                 "RR_PRMS_Output_sub_cfs.csv",
                 "RR_PRMS_Output_sub_inq.csv",
                 "PRMS_Console_Output.txt")
  
  
  # Confirm that they exist in the "output" folder first
  checkForModelOutputs_PRMS(prmsPath, modelOutput = NULL,
                            includeScriptGeneratedOutput = TRUE)
  
  
  # Prepare vectors that contain the proper filepaths and the planned filepaths
  sourcePaths <- copyFiles |>
    paste0(prmsPath, "/PRMS/output/", 
           ... = _) |>
    normalizePath(mustWork = FALSE)
  
  
  # For the "sub_cfs" and "sub_inq" files, include today's date and the 
  # modeler's name in these filenames
  writePaths <- copyFiles |>
    str_replace("RR_PRMS_Output",
                paste0("RR_PRMS_Output_",
                       Sys.Date(),
                       "_", Sys.info()[["user"]],
                       "_", startDate,
                       "_", endDate)) |>
    paste0(dirPath, "/PRMS/Output/", 
           ... = _) |>
    normalizePath(mustWork = FALSE)
  
  
  # Copy the files
  # If any of these actions fail, 'copyRes' will contain FALSE in its vector
  copyRes <- file.copy(sourcePaths, writePaths, overwrite = TRUE)
  
  
  # Verify that the files copied successfully
  # If not, output an error message
  if (anyFalse(copyRes) || anyFalse(file.exists(writePaths))) {
    
    missingFiles <- which(!copyRes)
    
    
    stop(paste0("Could Not Copy File", 
                if_else(length(missingFiles) > 1, "s", ""), "\n\n",
                "The script attempted to copy PRMS output files to the ",
                "hydrology folder. However, the process was not successful ",
                "for ", vec2QuotedStr(copyFiles[missingFiles]), ".\n\n",
                "Perhaps there was a permission issue? Please investigate.\n\n",
                "The intended new file", 
                if_else(length(missingFiles) > 1, "s were", " was"), ": ",
                vec2QuotedStr(writePaths[missingFiles])) |>
           errWrap())
    
  } 
  
  
  # Return nothing if there were no issues
  return(invisible(NULL))
  
}



deleteFiles <- function (prmsPath) {
  
  # Delete the "RR_PRMS" directory in the "ProcessedData" folder
  
  # Start by deleting that folder
  dir_delete(prmsPath)
  
  
  # Confirm that it was deleted
  if (dir.exists(prmsPath)) {
    
    stop(paste0("Failed to Delete PRMS Directory\n\n",
                "The script attempted to delete the PRMS model files that ",
                "were located in the \"ProcessedData\" folder. However, it ",
                "was unsuccessful for an unknown reason. Please investigate.\n\n",
                "(This error occurred for \"", prmsPath, "\")") |>
           errWrap())
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}


#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

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
  copy_model_outputs("PRMS", prmsPath, dirPath, startDate, endDate,
                     includeScriptGeneratedOutput = TRUE)
  
  
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



copy_model_outputs <- function (model, modelPath, dirPath, startDate, endDate,
                                additionalInputFiles = NULL, 
                                includeScriptGeneratedOutput = FALSE,
                                archiveModelName = model) {
  
  # Copy several files from the model "output" folder (plus additional files)
  # into the hydrology folder 
  
  # By default, the control file and parameter file are archived too
  # (in the model's input folder)
  
  # If a "nam" file is present too in the model files, it will be saved as well
  
  # If more input files must be archived, their paths should be included in 
  # 'additionalInputFiles' as a character vector containing relative paths 
  # (relative to the root model directory)
  
  # (If more output files should be archived, please update `list_model_outputs`)
  
  # 'archiveModelName' is reserved for models whose archive folder names are 
  # different from their actual model names
  # (For example, if the SRPHM model has its archive folder labeled as "SRP")
  
  
  # Confirm that they exist in the model's "output" folder first
  check_for_model_outputs(model, modelPath, modelOutput = NULL,
                          includeScriptGeneratedOutput = includeScriptGeneratedOutput)
  
  
  # This vector contains the paths of the key output files 
  # (and the script-generated log)
  copyFiles <- list_model_outputs(model, modelPath, 
                                  includeScriptGeneratedOutput = includeScriptGeneratedOutput)
  
  
  # Prepare vectors that contain the proper filepaths in 'copyFiles'
  sourcePaths <- copyFiles |>
    normalizePath(mustWork = TRUE)
  
  
  # Prepare the output filenames next
  # In most cases, the names will be the exact same as 'copyFiles'
  # However, some adjustments may be applied for different models
  if (model %in% c("PRMS")) {
    
    # For the "sub_cfs" and "sub_inq" files, include the model scraping bounds 
    # in their archive filenames
    writePaths <- copyFiles |>
      str_replace(paste0("RR_", model, "_Output(?=_sub_((cfs)|(inq))\\.csv$)"),
                  paste0("RR_", model, "_Output_", startDate, "_", endDate))
    
    # The regular expression checks for "RR_PRMS_Output" in the file paths,
    # with a lookahead regex specifically matching "sub_cfs.csv" and "sub_inq.csv" 
    
    # (This is needed in case "RR_PRMS_Output_" would match with a portion of the
    #  path rather than the actual filename)
    
  } else {
    
    writePaths <- copyFiles
    
  }
  
  
  # After that, replace the folder paths in 'writePaths' using 'dirPath'
  # The files will be written to the model's "Output" folder
  writePaths <- writePaths |>
    extract_filename() |>
    paste0(dirPath, "/", archiveModelName, "/Output/", ... = _) |>
    normalizePath(mustWork = FALSE)
  
  
  # Archive the control and parameter files as well
  # (The nam file too, if it exists)
  modelFiles <- list_model_components(model)
  
  modelFiles <- modelFiles[names(modelFiles) %in% c("CONTROL", "PARAM", "NAM")]
  
  
  # Convert 'modelFiles' into a vector
  modelFiles <- modelFiles |> unlist(use.names = FALSE)
  
  
  # If 'additionalInputFiles' is not NULL, add those paths to 'modelFiles' too
  if (!is.null(additionalInputFiles)) {
    
    modelFiles <- c(modelFiles, 
                    additionalInputFiles)
    
  }
  
  
  # Add these paths to 'sourcePaths' and 'writePaths'
  sourcePaths <- c(sourcePaths,
                   modelFiles |>
                     paste0(modelPath, "/", ... = _) |>
                     checkForPreviousOutput())
  
  
  # These files will be saved in the archive "Input" folder
  writePaths <- c(writePaths,
                  modelFiles |>
                    extract_filename() |>
                    paste0(dirPath, "/", archiveModelName, "/Input/", ... = _) |>
                    normalizePath(mustWork = FALSE))
  
  
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

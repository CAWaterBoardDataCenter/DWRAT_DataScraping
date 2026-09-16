# Prepare the PRMS files for a model run

# The model files will be copied from another location
# to the "Output" folder

# The source location is specified in the field "RR_PRMS_SOURCE_LOCATION" 
# in "RR_Supply_Control_File.xlsx"



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
  cat("Starting 'RRW_008_Setup_PRMS_Model.R'!\n\n")
  
  
  # Perform the model copying procedure in a generic function
  copy_model_files("PRMS", "RR_PRMS_SOURCE_LOCATION")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_008_Setup_PRMS_Model.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



copy_model_files <- function (model, sourceField) {
  
  # For a given model, copy its files from a source directory
  # into a newly created folder within the workflow's "Output" folder
  
  # (This folder is temporary and will be deleted after the model run finishes)
  
  
  # Get the location of the model files
  sourceDir <- getFromControl_RR(sourceField)
  
  
  # Validate the user's input and ensure that this directory contains
  # all required components
  # (Also, if the directory is on SharePoint, 'sourceDir' will be adjusted
  #  to reflect that)
  sourceDir <- validateSourceModelDirectory(sourceDir, sourceField, model)
  
  
  cat(paste0("[1/1]\tCopying the ", model, " folder to \"W2_Russian_River/Output/",
             get_model_dir_name(model), "\"...\n"))
  
  
  # Copy the contents from 'sourceDir' to a new folder in "Output"
  copy_contents(sourceDir, 
                paste0("W2_Russian_River/Output/", get_model_dir_name(model)))
  
  
  cat("\tDone!\n\n")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



copy_contents <- function (sourceDir, newDir) {
  
  # Copy the files from 'sourceDir' into a newly created folder called 'newDir'
  
  
  # If the folder already exists, delete it and its contents
  if (dir.exists(newDir)) {
    
    unlink(newDir, recursive = TRUE)
    
  }
  
  
  # Next, create the 'newDir' folder
  dir.create(newDir)
  
  
  # Copy the entire contents of 'sourceDir' into this new folder
  tryRes <- try(dir_copy(sourceDir, newDir, overwrite = TRUE),
                silent = TRUE)
  
  
  # If copying the files fails for some reason, try a second time before quitting
  if ("try-error" %in% class(tryRes)) {
    
    # Output the error message that was received
    cat("\n\n")
    print(tryRes)
    cat("\n\n")
    
    
    # Notify the user that the script will try again
    cat("Copying the files failed!\n")
    cat("(Reason shown above)")
    cat("\n\n")
    cat("Trying one more time!")
    cat("\n\n")
    
    
    # Wait a bit before retrying
    Sys.sleep(runif(1, min = 2, max = 5))
    
    
    # Try again to copy the directory over
    dir_copy(sourceDir, newDir, overwrite = TRUE)
    
  }
  
  
  # If the process is successful, return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

# Prepare the PRMS files for a model run

# The model files will be copied from another location
# to the "Output" folder

# The source location is specified in the field "RR_PRMS_SOURCE_LOCATION" 
# in "RR_Supply_Control_File.xlsx"



#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source(Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_008_Setup_PRMS_Model.R'!\n\n")
  
  
  # Get the location of the PRMS model files
  sourceDir <- getFromControl_RR("RR_PRMS_SOURCE_LOCATION")
  
  
  # Validate the user's input and ensure that this directory contains
  # all required components
  # (Also, if the directory is on SharePoint, 'sourceDir' will be adjusted
  #  to reflect that)
  sourceDir <- validateSourceModelDirectory(sourceDir, "RR_PRMS_SOURCE_LOCATION",
                                            "PRMS", 
                                            c("bin", "PRMS", "windows", 
                                              "PRMS/input/climate_scenarios"),
                                            c("bin/gsflow.exe", 
                                              "windows/prms_rr.control",
                                              "windows/run.bat"))
  
  
  cat("[1/1]\tCopying the PRMS folder to \"W2_Russian_River/Output/RR_PRMS\"...\n")
  
  
  # Copy the contents from 'sourceDir' to a new "RR_PRMS" folder in "Output"
  copyModel(sourceDir, "W2_Russian_River/Output/RR_PRMS")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_008_Setup_PRMS_Model.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



copyModel <- function (sourceDir, newDir) {
  
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
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

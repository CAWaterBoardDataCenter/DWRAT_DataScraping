# Prepare the PRMS files for a model run

# The model files will be copied from another location
# to the "ProcessedData" folder

# The source location is specified in the field "RR_PRMS_SOURCE_LOCATION" 
# in "RR_Supply_Control_File.xlsx"



#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Supply_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRS_007_Setup_PRMS_Model.R'!\n\n")
  
  
  # Get the location of the PRMS model files
  sourceDir <- getFromSupplyControl_RR("RR_PRMS_SOURCE_LOCATION")
  
  
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
  
  
  cat("[1/1]\tCopying the PRMS folder to \"ProcessedData/RR_PRMS\"...\n")
  
  
  # Copy the contents from 'sourceDir' to a new "RR_PRMS" folder in "ProcessedData"
  copyModel(sourceDir)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRS_007_Setup_PRMS_Model.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



copyModel <- function (sourceDir) {
  
  # Copy the files from 'sourceDir' into a newly created "RR_PRMS" folder in
  # the "ProcessedData" folder
  
  
  # 'newDir' will contain the new folder location relative to the working directory
  newDir <- "ProcessedData/RR_PRMS"
  
  
  # If the folder already exists, delete it and its contents
  if (dir.exists(newDir)) {
    
    unlink(newDir, recursive = TRUE)
    
  }
  
  
  # Next, create the "RR_PRMS" folder
  dir.create(newDir)
  
  
  # Copy the entire contents of 'sourceDir' into this new folder
  dir_copy(sourceDir, newDir, overwrite = TRUE)
  
  
  # Side note: It doesn't matter if the source folder has a name that's 
  # different from "RR_PRMS"
  # In "ProcessedData", the folder will still be called "RR_PRMS"
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

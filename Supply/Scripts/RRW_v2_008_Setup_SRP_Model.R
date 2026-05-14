# Prepare the SRP files for a model run

# The model files will be copied from another location
# to the "ProcessedData" folder

# The source location is specified in the field "SRPHM_SIR2024_SOURCE_LOCATION" 
# in "RR_Workflow_Control_File.xlsx"


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
  cat("Starting 'RRW_v2_008_Setup_SRP_Model.R'!\n\n")
  
  
  # Get the location of the SRP model files
  sourceDir <- getFromControl_RR("SRPHM_SIR2024_SOURCE_LOCATION")
  
  
  # Validate the user's input and ensure that this directory contains
  # all required components
  # (Also, if the directory is on SharePoint, 'sourceDir' will be adjusted
  #  to reflect that)
  sourceDir <- validateSourceModelDirectory(
    sourceDir, 
    "SRPHM_SIR2024_SOURCE_LOCATION",
    "SRP", 
    c("bin", "model", "output", 
      "model/external_files", 
      "model/model1", "model/model2",
      "model/model1/SRPHM_2000_2026", 
      "output/output.model1_full",
      "output/output.model1_2000_2026",
      "output/output.model2_dpl"),
    c("bin/gsflow.exe",
      "model/model1/SRPHM_2000_2026/SRPHM_spinup.control"))
  
  
  cat("[1/1]\tCopying the SRP folder to \"ProcessedData/SIR2024-5121_update\"...\n")
  
  
  # Copy the contents from 'sourceDir' to a new SRP folder in "ProcessedData"
  copyModel(sourceDir)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_008_Setup_SRP_Model.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



copyModel <- function (sourceDir) {
  
  # Copy the files from 'sourceDir' 
  # into a newly created "SIR2024-5121_update" folder
  # in the "ProcessedData" folder
  
  
  # 'newDir' will contain the new folder location 
  # relative to the working directory
  newDir <- "ProcessedData/SIR2024-5121_update"
  
  
  # If the folder already exists, delete it and its contents
  if (dir.exists(newDir)) {
    
    unlink(newDir, recursive = TRUE)
    
  }
  
  
  # Next, create the "SIR2024-5121_update" folder
  dir.create(newDir)
  
  
  # Copy the entire contents of 'sourceDir' into this new folder
  dir_copy(sourceDir, newDir, overwrite = TRUE)
  
  
  # Side note: It doesn't matter if the source folder has a name that's 
  # different from "SIR2024-5121_update"
  # In "ProcessedData", the folder will still be called "SIR2024-5121_update"
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

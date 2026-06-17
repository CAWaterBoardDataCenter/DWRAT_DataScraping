# Prepare the SRP files for a model run

# The model files will be copied from another location
# to the "Output" folder

# The source location is specified in the field "SRPHM_SOURCE_LOCATION" 
# in "RR_Workflow_Control_File.xlsx"


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("W2_Russian_River/Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_v2_008_Setup_SRP_Model.R'!\n\n")
  
  
  # Get the location of the SRP model files
  sourceDir <- getFromControl_RR("SRPHM_SOURCE_LOCATION")
  
  
  # Validate the user's input and ensure that this directory contains
  # all required components
  # (Also, if the directory is on SharePoint, 'sourceDir' will be adjusted
  #  to reflect that)
  sourceDir <- validateSourceModelDirectory(
    sourceDir, 
    "SRPHM_SOURCE_LOCATION",
    "SRP", 
    c("external_files", "model1", "model1/SRPHM_post_spinup_WY2021",
      "model1/SRPHM_post_spinup_WY2021/bin", 
      "model1/SRPHM_post_spinup_WY2021/output"),
    c("model1/SRPHM_post_spinup_WY2021/bin/gsflow.exe",
      "model1/SRPHM_post_spinup_WY2021/SRPHM_spinup.control",
      "model1/SRPHM_post_spinup_WY2021/SRPHM_spinup.nam",
      "external_files/restartdata_2020.out"))
  
  
  cat("[1/1]\tCopying the SRP folder to \"W2_Russian_River/Output/SRPHM\"...\n")
  
  
  # Borrow a function from the PRMS model setup script
  functionStealer("W2_Russian_River/Scripts/RRW_007_Setup_PRMS_Model.R", "copyModel")
  
  
  # Copy the contents from 'sourceDir' to a new SRP folder in "Output"
  copyModel(sourceDir, "W2_Russian_River/Output/SRPHM")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_008_Setup_SRP_Model.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

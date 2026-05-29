# Prepare the RRIHM files for a model run

# The model files will be copied from another location
# to the "ProcessedData" folder

# The source location is specified in the field "RRIHM_SOURCE_LOCATION" 
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
  cat("Starting 'RRW_v2_012_Setup_RRIHM_Model.R'!\n\n")
  
  
  # Get the location of the RRIHM model files
  sourceDir <- getFromControl_RR("RRIHM_SOURCE_LOCATION")
  
  
  # Validate the user's input and ensure that this directory contains
  # all required components
  # (Also, if the directory is on SharePoint, 'sourceDir' will be adjusted
  #  to reflect that)
  sourceDir <- validateSourceModelDirectory(
    sourceDir, 
    "RRIHM_SOURCE_LOCATION",
    "RRIHM", 
    c("RRIHM_post_spinup_WY2021", "RRIHM_post_spinup_WY2021/modflow",
      "RRIHM_post_spinup_WY2021/prms", "RRIHM_post_spinup_WY2021/windows",
      "RRIHM_post_spinup_WY2021/modflow/input", 
      "RRIHM_post_spinup_WY2021/modflow/output",
      "RRIHM_post_spinup_WY2021/prms/input", 
      "RRIHM_post_spinup_WY2021/prms/output",
      "RRIHM_post_spinup_WY2021/windows/bin"),
    c("RRIHM_post_spinup_WY2021/modflow/input/Mark_West_inflow.dat",
      "RRIHM_post_spinup_WY2021/modflow/input/restartdata_2020.out",
      "RRIHM_post_spinup_WY2021/windows/bin/gsflow.exe"))
  
  
  cat("[1/1]\tCopying the RRIHM folder to \"ProcessedData/RRIHM\"...\n")
  
  
  # Import a function from the v1 workflow's PRMS model setup script
  functionStealer("Scripts/RRW_007_Setup_PRMS_Model.R", "copyModel")
  
  
  # Copy the contents from 'sourceDir' to a new folder in "ProcessedData"
  copyModel(sourceDir, "ProcessedData/RRIHM")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_012_Setup_RRIHM_Model.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

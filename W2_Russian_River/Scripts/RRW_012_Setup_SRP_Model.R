# Prepare the SRP files for a model run

# The model files will be copied from another location
# to the "ProcessedData" folder

# The source location is specified in the field "RR_SRP_SOURCE_LOCATION" 
# in "RR_Workflow_Control_File.xlsx"



#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_012_Setup_SRP_Model.R'!\n\n")
  
  
  # Get the location of the SRP model files
  sourceDir <- getFromControl_RR("RR_SRP_SOURCE_LOCATION")
  
  
  # Validate the user's input and ensure that this directory contains
  # all required components
  # (Also, if the directory is on SharePoint, 'sourceDir' will be adjusted
  #  to reflect that)
  sourceDir <- validateSourceModelDirectory(sourceDir, "RR_SRP_SOURCE_LOCATION",
                                            "SRP", 
                                            c("basin", "External Files", 
                                              "input", "nsub", "output"),
                                            c("SRPHM_update.control", 
                                              "Run_updated_Model.bat",
                                              "gsflow_ag.exe"))
  
  
  cat("[1/1]\tCopying the SRP folder to \"ProcessedData/SRPHM_update_ag\"...\n")
  
  
  # Borrow a function from the PRMS model setup script
  functionStealer("Scripts/RRW_007_Setup_PRMS_Model.R", "copyModel")
  
  
  # Copy the contents from 'sourceDir' to a new SRP folder in "ProcessedData"
  copyModel(sourceDir, "ProcessedData/SRPHM_update_ag")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_012_Setup_SRP_Model.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

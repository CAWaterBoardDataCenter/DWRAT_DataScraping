# Prepare the SRP files for a model run

# The model files will be copied from another location
# to the "Output" folder

# The source location is specified in the field "SRPHM_SOURCE_LOCATION" 
# in "RR_Workflow_Control_File.xlsx"


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
  cat("Starting 'RRW_v2_008_Setup_SRP_Model.R'!\n\n")
  
  
  # Rely on the functions from the PRMS model setup script to perform these actions
  c("copy_model_files", "copy_contents") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_008_Setup_PRMS_Model.R", .))
  
  
  # Copy the SRPHM model files to the "Output" folder
  copy_model_files("SRPHM", "SRPHM_SOURCE_LOCATION")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_008_Setup_SRP_Model.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

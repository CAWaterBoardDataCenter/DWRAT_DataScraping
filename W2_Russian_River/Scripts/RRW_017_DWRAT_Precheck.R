# Confirm that an installation of Anaconda exists on the user's device

# Then, check for an environment called "paradigm-dwrat"
# If the environment does not exist, install it

#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_017_DWRAT_Precheck.R'!\n")
  
  
  # Check for an installation of Anaconda
  cat("\n[1/2]\tChecking for Anaconda...\n")
  
  
  # If it exists, `detectAnacondaBat` will be able to retrieve a path to
  # Anaconda's "activate.bat" script
  batPath <- detectAnacondaBat()
  
  
  cat("\tDone!\n\n")
  
  
  # Next, check for the "paradigm-dwrat" environment
  cat("[2/2]\tChecking for \"paradigm-dwrat\" environment...\n")
  
  
  # Get a list of environments and check for "paradigm-dwrat"
  envList <- system(paste0(batPath, " && conda env list"), intern = TRUE)
  
  
  envDetected <- envList |>
    str_subset("^paradigm-dwrat\\s+")
  
  
  # If "paradigm-dwrat" is NOT detected, it must be installed
  if (length(envDetected) == 0) {
    
    # Notify the user
    paste0("The \"paradigm-dwrat\" Anaconda environment will now be ",
           "installed! This will take a few minutes!") |>
      errWrap() |>
      message()
    
    
    installDWRAT(batPath)
    
  }
  
  
  # Once "paradigm-dwrat" is confirmed or installed, conclude the script
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'RRW_017_DWRAT_Precheck.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



installDWRAT <- function (batPath) {
  
  # Establish a new Anaconda environment for DWRAT
  
  # This function relies on the "environment.yml" file located in 
  # the "Paradigm_DWRAT" sub-folder of the SDA "DWRAT_DataScraping" repository
  
  
  # First, look for "environment.yml" and confirm its existence
  envPath <- "../Paradigm_DWRAT/environment.yml" |>
    normalizePath(mustWork = FALSE)
  
  
  # Throw an error if it is not found
  if (!file.exists(envPath)) {
    
    paste0("Environment.yml Not Found\n\n", 
           "The Paradigm DWRAT scripts should have an accompanying ",
           "\"environment.yml\" file in their folder in the ",
           "\"DWRAT_DataScraping\" repository. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there are no issues, install the environment
  installAnacondaEnv(batPath, envPath)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

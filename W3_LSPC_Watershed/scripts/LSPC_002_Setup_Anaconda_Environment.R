# The LSPC climate scripts that download and process weather data are primarily 
# written in Python

# Before initiating that process, confirm that an installation of Anaconda 
# exists on the user's device

# Then, check for an environment called "lspc-climate-processing-restructure"
# If the environment does not exist, install it


# To Do: This script has a lot of overlap with "RRW_018_DWRAT_Precheck.R"
# Their processes can be consolidated into generic functions


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'LSPC_002_Setup_Anaconda_Environment.R'!\n")
  
  
  # Check for an installation of Anaconda
  cat("\n[1/2]\tChecking for Anaconda...\n")
  
  
  # If it exists, `detectAnacondaBat` will be able to retrieve a path to
  # Anaconda's "activate.bat" script
  batPath <- detectAnacondaBat()
  
  
  cat("\tDone!\n\n")
  
  
  # Next, check for the "lspc-climate-processing-restructure" environment
  cat("[2/2]\tChecking for \"lspc-climate-processing-restructure\" environment...\n")
  
  
  # Get a list of environments and check for "lspc-climate-processing-restructure"
  envList <- system(paste0(batPath, " && conda env list"), intern = TRUE)
  
  
  envDetected <- envList |>
    str_subset("^lspc-climate-processing-restructure\\s+")
  
  
  # If "lspc-climate-processing-restructure" is NOT detected, it must be installed
  if (length(envDetected) == 0) {
    
    # Notify the user
    paste0("The \"lspc-climate-processing-restructure\" Anaconda environment ",
           "will now be installed! This will take a few minutes!") |>
      errWrap() |>
      message()
    
    
    installEnv(batPath, "W3_LSPC_Watershed/LSPC_Climate_Environment.yml")
    
  }
  
  
  # Once "paradigm-dwrat" is confirmed or installed, conclude the script
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_002_Setup_Anaconda_Environment.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



installEnv <- function (batPath, ymlPath) {
  
  # Establish a new Anaconda environment for Python scripts
  
  # This function relies on the environment YML file given in 'ymlPath'
  
  
  # Throw an error if 'ymlPath' is not found
  if (!file.exists(ymlPath)) {
    
    paste0("Environment YML Not Found\n\n", 
           "The YAML file \"", ymlPath, "\" could not be found. ",
           "Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there are no issues, install the environment
  installAnacondaEnv(batPath, ymlPath)
  
  
  # After that, configure the environment to allow web requests to function properly
  # (This will address potential issues with SSL certificates on corporate networks)
  sslConfig <- system(paste0(batPath, 
                             " && conda activate lspc-climate-processing-restructure",
                             " && conda config --set ssl_verify truststore"), 
                      intern = TRUE)
  
  
  if (any(grepl("Error", sslConfig, ignore.case = TRUE))) {
    
    print(sslConfig)
    
    stop_script("An error occurred when re-configuring the Anaconda environment.")
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

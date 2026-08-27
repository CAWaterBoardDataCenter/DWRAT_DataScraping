# Confirm that directories exist for each watershed in the weather control file

# These folders must be in place before the weather data download can begin


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
  cat("Starting 'LSPC_003_Setup_Project_Directories.R'!\n")
  
  
  # Import the weather control file to get a list of watersheds 
  cat("\n[1/2]\tGetting list of watersheds...\n")
  
  
  controlDF <- read_lspc_weather_control()
  
  
  # To Do: Validation function for weather control file 
  
  
  cat("\tDone!\n\n")
  
  
  # Next, confirm that a directory exists for each watershed under "projects" 
  # in the "data" subfolder of the workflow folder
  cat("[2/2]\tChecking directories...\n")
  
  
  # Create a vector of paths pointing to each watershed folder in "projects"
  dirPaths <- paste0("W3_LSPC_Watershed/data/projects/", controlDF$project_name)
  
  
  # Create these directories if they do not already exist
  dirPaths |> dir_create(recurse = FALSE)
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_003_Setup_Project_Directories.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

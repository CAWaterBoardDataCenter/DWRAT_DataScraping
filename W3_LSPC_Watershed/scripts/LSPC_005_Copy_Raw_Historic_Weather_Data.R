# Copy historic weather data that is shared between all watersheds
# (from PRISM, NLDAS, and CIMIS)

# The field "HISTORIC_SHARED_WEATHER_DATA_LOCATION" should contain a path to a folder

# Within that folder should be data source folders for shared weather files

# These folder names should match the names present in "W3_LSPC_Watershed\data\shared\raw"


# However, only data from before the user-specified start date will be saved in the "raw" folder
# Files that were just downloaded by prior scripts will not be overwritten


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
  cat("Starting 'LSPC_005_Copy_Raw_Historic_Weather_Data.R'!\n")
  
  
  # Import the data scraping bounds
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  cat("\n[1/2]\tChecking directories...\n")
  
  
  # Define the target weather location
  # This is the local directory in the repository where shared raw data will be saved
  targetPath <- "W3_LSPC_Watershed/data/shared/raw"
  
  # To Do: This path can be defined generically in a function so that LSPC_003 and this script
  # can rely on the same information
  
  
  # Get the list of data source folders in 'targetPath' (e.g., "prism", "nldas", "cimis")
  targetFolders <- list.dirs(targetPath, full.names = FALSE, recursive = FALSE)
  
  
  # Get the path to the historic weather data next
  rawPath <- get_from_lspc_master_control("HISTORIC_SHARED_WEATHER_DATA_LOCATION") |>
    sharepointPathCheck(isFolder = TRUE)
  
  
  # To Do: Validate this directory
  # It should contain the folders that appear in "data\shared\raw"
  stopifnot(all(targetFolders %in% list.files(rawPath)))
  
  
  cat("\tDone!\n\n")
  
  
  # After that, for each data source folder in 'targetPath',
  # copy its corresponding folder from 'rawPath'
  cat("[2/2]\tCopying files...\n")
  
  
  # Copy the contents of the directories in 'rawPath' over to the repository's 
  # local "raw" folder ('targetPath')
  try(dir_copy(paste0(rawPath, "/", targetFolders),
               paste0(targetPath, "/", targetFolders),
               overwrite = FALSE), 
      silent = TRUE)
  
  # With 'overwrite' set to FALSE, files downloaded by prior scripts
  # will NOT be overwritten
  
  # Ideally, these files would be more "fresh" 
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_005_Copy_Raw_Historic_Weather_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

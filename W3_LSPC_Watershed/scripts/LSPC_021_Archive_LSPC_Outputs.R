# Archive the model outputs from LSPC 

# The main file of interest is "stream.csv" for each watershed


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
  cat("Starting 'LSPC_021_Archive_LSPC_Outputs.R'!\n")
  
  
  # Import functions from other scripts
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_012_Archive_Raw_and_Staged_Files.R",
                  "get_lspc_archive_folder")
  
  c("get_lspc_model_directories", "validate_lspc_model_folder") |>
    map(~ functionStealer("W3_LSPC_Watershed/scripts/LSPC_017_Check_LSPC_Model_Directory.R", .))
  
  
  # Import the data scraping bounds next
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Read in the LSPC weather control file too
  # (A list of watersheds is needed)
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validation of control file
  
  
  # Get the location of the archive folder
  dirPath <- get_lspc_archive_folder(startDate, endDate)
  
  
  # To Do: Use a generic function for this procedure and LSPC_012
  
  
  # Finally, get the directories for each watershed model folder
  modelFolder <- controlDF |>
    get_lspc_model_directories()
  
  
  modelFolder |>
    map(validate_lspc_model_folder)
  
  
  # Confirm that the model output folders have all desired outputs
  cat("\n[1/2]\tChecking for model outputs...\n")
  
  
  # Define a vector of files to archive
  expectedFiles <- c("stream.csv", "stream.out")
  
  
  # Confirm that each watershed's model sub-directory has these files 
  # among their outputs
  outputPaths <- modelFolder |>
    map(~ paste0(., "/Output/", expectedFiles)) |>
    unlist()
  
  
  outputPaths |> 
    check_if_missing_file()
  
  # To Do: Checking for model outputs should be a function also used by LSPC_020
  
  # Model input and output directories should be set in a single function too
  # so that they're easier to adjust
  
  
  cat("\tDone!\n\n")
  
  
  # The next step is to copy these outputs to the archive folder
  cat("[2/2]\tCopying files...\n")
  
  
  # These model outputs will be saved in the archive folder's watershed-specific
  # sub-folders (under "Output")
  writePaths <- controlDF$project_name |>
    map(~ paste0(dirPath, "/", ., "/Output/", expectedFiles)) |>
    unlist()
  
  
  # Copy the files
  # If any issues are encountered, the `copyFile` function will output an error
  map2(outputPaths, writePaths, copyFile)
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_021_Archive_LSPC_Outputs.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

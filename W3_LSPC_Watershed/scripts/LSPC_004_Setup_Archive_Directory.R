# Setup the hydrology folder that will store the model input and output files

# Metadata will be generated as well for this procedure run


# This script only requires "HYDROLOGY_OUTPUT_LOCATION" to be filled in
# with a path in the control file


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
  cat("Starting 'LSPC_004_Setup_Archive_Directory.R'!\n")
  
  
  # Import the data scraping bounds
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Read in the LSPC weather control file too
  # (A list of watersheds is needed)
  controlDF <- read_lspc_weather_control()
  
  
  # Rely on functions developed for the Russian River workflow as well
  c("validateInput", "generateFolders", "chooseFolderName", "addFiles") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_006_Setup_Output_Directory.R", .))
  
  # To Do: Make the error messages and such more generic in these RR functions
  
  
  # Prepare the new directory
  cat("[1/3]\tCreating new folders...\n")
  
  
  # Get the location where a new folder will be created
  saveDirectory <- get_from_lspc_master_control("HYDROLOGY_OUTPUT_LOCATION")
  
  
  # Confirm that the user's specification is valid
  saveDirectory <- validateInput(saveDirectory, "HYDROLOGY_OUTPUT_LOCATION")
  
  
  # Next, generate the directory and its sub-folders
  outputDirectory <- generateFolders(saveDirectory, models = controlDF$project_name, 
                                     isRussianRiver = FALSE)
  
  
  cat("\tDone!\n\n")
  
  
  cat("[2/3]\tGenerating metadata...\n")
  
  
  # Add metadata and the project lockfile to this new location
  # (The workflow version number will be saved here too)
  addFiles(outputDirectory, startDate, endDate, "LSPC_v1")
  
  
  cat("\tDone!\n\n")
  
  
  cat("[3/3]\tSaving new folder path to a text file for easy access...\n")
  
  
  # Save 'outputDirectory' to a text file in the "shared" folder under "data"
  # This will make it easier to reference in later scripts
  outPath <- paste0("W3_LSPC_Watershed/data/shared/Archive_Location_", 
                    startDate, "_", endDate, ".txt")
  
  outputDirectory |>
    writeOutput(outPath)
  
  
  # Save that file to 'outputDirectory' too
  
  # Edit 'outPath' to point to 'outputDirectory' instead of "Output"
  outPath <- outPath |>
    extract_filename() |>
    paste0(outputDirectory, "/", ... = _) |> 
    normalizePath(mustWork = FALSE)
  
  
  # Then save the txt file there too
  outputDirectory |>
    writeOutput(outPath, quietly = TRUE)
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_004_Setup_Archive_Directory.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

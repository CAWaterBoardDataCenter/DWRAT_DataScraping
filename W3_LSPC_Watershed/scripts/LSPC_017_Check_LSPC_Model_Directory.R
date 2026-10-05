# Each of the project watersheds should have a corresponding folder in "Models/LSPC"

# This script will ensure that these directories exist and are valid 


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
  cat("Starting 'LSPC_017_Check_LSPC_Model_Directory.R'!\n")
  
  
  # Read in the LSPC weather control file too
  # (A list of watersheds is needed)
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validation of control file
  
  
  # Confirm that each project watershed has a folder under the LSPC model directory
  cat("\n[1/2]\tChecking for LSPC model folders...\n")
  
  
  # Get a vector containing the relative paths to each model folder
  pathVec <- controlDF |> 
    get_lspc_model_directories()
  
  
  # Confirm that each path exists
  error_if(!all(dir_exists(pathVec)),
           paste0("Missing Model Directory\n\n",
                  "Each watershed should have its own LSPC model folder. However, ",
                  "no directory was found for ",
                  pathVec[!dir_exists(pathVec)] |> vec2QuotedStr(), ". ",
                  "Please investigate."))
  
  
  cat("\tDone!\n\n")
  
  
  cat("[2/2]\tValidating LSPC model folder contents...\n")
  
  
  # Check each folder for the same required structure and contents
  for (i in 1:length(pathVec)) {
    
    pathVec[i] |>
      validate_lspc_model_folder()
    
  }
  
    
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_017_Check_LSPC_Model_Directory.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



get_lspc_model_directories <- function (controlDF) {
  
  # Create a vector of folder paths
  
  # These will be the expected locations of LSPC model files for each watershed
  # that appears in 'controlDF'
  
  
  # All watershed model files should appear under the same parent directory
  return(paste0("Models/LSPC/",
                controlDF$project_name))
  
}



validate_lspc_model_folder <- function (modelFolderPath) {
  
  # Given the path to an LSPC model folder, check its contents
  
  # Make sure all required folders and files are present
  
  
  # To Do: This can be combined with the Russian River functions that inspect
  # a model source directory
  
  
  # First double-check that 'modelFolderPath' is the expected type of variable
  # It should be a single string that contains a folder path
  if (!is.vector(modelFolderPath) || length(modelFolderPath) != 1 ||
      !is.character(modelFolderPath[1]) || !dir.exists(modelFolderPath[1])) {
    
    paste0("Input Not a Folder Path\n\n",
           "Please adjust the script. A single path to a folder is the expected ",
           "input for this function.") |>
      stop_script()
    
  }
  
  
  # Start by checking for model folders
  # Every watershed should have at least three key folders
  modelFolders <- c("Input", "Output", "Input/Weather") |>
    paste0(modelFolderPath, "/", ... = _)
  
  
  error_if(!all(dir_exists(modelFolders)),
           paste0("Incomplete Model Folder\n\n",
                  "Several folders are expected to be in each LSPC model folder. ",
                  "However, ", 
                  modelFolders[!dir_exists(modelFolders)] |> vec2QuotedStr(),
                  " could not be found. Please investigate \"",
                  modelFolderPath, "\"."))
  
  
  # Check for the LSPC executable file next
  exeFile <- list.files(modelFolderPath, pattern = "^LSPC.+\\.exe$")
  
  # This file search applies to the root path of the watershed's model folder
  # There should be an executable file that starts with "LSPC" 
  
  
  # To Do: Make getting the EXE file a function so that it's not hard-coded
  # again later when running the model
  
  
  # Output an error message if 'exeFile' has zero matches
  error_if(length(exeFile) == 0,
           paste0("Could Not Find Executable File\n\n",
                  "Each watershed's model folder should contain an exe file ",
                  "that runs LSPC. However, no match was found.\n\n",
                  "Please investigate \"", modelFolderPath, "\""))
  
  
  # There should only be one match too
  error_if(length(exeFile) > 1,
           paste0("Could Not Identify Executable File\n\n",
                  "Each watershed's model folder should contain an exe file ",
                  "that runs LSPC. However, ", length(exeFile), " potential ",
                  "matches were found. The script cannot determine which file ",
                  "should be used.\n\n",
                  "Please investigate \"", modelFolderPath, "\""))
  
  
  # Finally, check for an LSPC model input file in the watershed "Input" folder
  inpFile <- modelFolders[1] |>
    list.files(pattern = "\\.inp$")
  
  
  # Just like the executable file, there should be exactly one match
  error_if(length(inpFile) == 0,
           paste0("Could Not Find LSPC Input File\n\n",
                  "Each watershed's model folder should contain an inp file ",
                  "to configure LSPC. However, no match was found.\n\n",
                  "Please investigate \"", modelFolderPath, "\""))
  
  
  # There should only be one inp file in the folder
  error_if(length(inpFile) > 1,
           paste0("Could Not Identify LSPC Input File\n\n",
                  "Each watershed's model folder should contain an inp file ",
                  "that configures LSPC. However, ", length(inpFile), " potential ",
                  "matches were found. The script cannot determine which file ",
                  "should be used.\n\n",
                  "Please investigate \"", modelFolderPath, "\""))
  
  
  # If there are no issues, return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

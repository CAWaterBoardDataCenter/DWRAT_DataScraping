# Setup the hydrology folder that will store the model input and output files
# Metadata will be generated as well for this procedure run


# This script only requires "HYDROLOGY_OUTPUT_LOCATION" to be filled in
# with a path in "RR_Workflow_Control_File.xlsx"

# A new folder will be created there with sub-folders for the inputs and outputs 
# of SRP, RRIHM, and DWRAT

# A CSV file will also be generated that contains information about the procedure


# After that, one additional output will be added to the "Output" folder

# It will be a text file containing a single line that specifies 
# the path to the newly generated directory

# Its filename will be "Archive_Location_[startDate]_[endDate].txt"


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
  cat("Starting 'RRW_v2_006_Setup_Output_Directory.R'!\n")
  
  
  # Import functions from the v1 workflow's corresponding script
  c("validateInput", "generateFolders", "chooseFolderName", "addFiles") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_006_Setup_Output_Directory.R", .))
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  cat("[1/3]\tCreating new folders...\n")
  
  
  # Get the location where a new folder will be created
  saveDirectory <- getFromControl_RR("HYDROLOGY_OUTPUT_LOCATION")
  
  
  # Confirm that the user's specification is valid
  saveDirectory <- validateInput(saveDirectory, "HYDROLOGY_OUTPUT_LOCATION")
  
  
  # Next, generate the directory and its sub-folders
  outputDirectory <- generateFolders(saveDirectory)
  
  
  cat("\tDone!\n\n")
  
  
  cat("[2/3]\tGenerating metadata...\n")
  
  
  # Add metadata and the project lockfile to this new location
  # (The workflow version number will be saved here too)
  addFiles(outputDirectory, startDate, endDate, "RRW_v2")
  
  
  cat("\tDone!\n\n")
  
  
  cat("[3/3]\tSaving new folder path to a text file for easy access...\n")
  
  
  # Save 'outputDirectory' to a text file in the "Output" folder
  # This will make it easier to reference in later scripts
  outPath <- paste0("W2_Russian_River/Output/Archive_Location_", startDate,
                    "_", endDate, ".txt")
  
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
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_006_Setup_Output_Directory.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

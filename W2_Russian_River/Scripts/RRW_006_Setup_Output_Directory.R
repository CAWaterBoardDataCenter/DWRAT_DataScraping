# Setup the hydrology folder that will store the model input and output files
# Metadata will be generated as well for this procedure run


# This script only requires "HYDROLOGY_OUTPUT_LOCATION" to be filled in
# with a path in "RR_Workflow_Control_File.xlsx"

# A new folder will be created there with sub-folders for the inputs and outputs 
# of PRMS, SRP, and DWRAT

# A CSV file will also be generated that contains information about the procedure


# After that, one additional file will be added to the "Output" folder

# It will be a text file containing a single line that specifies 
# the path to the newly generated directory

# Its filename will be "Hydrology_Output_Folder_[startDate]_[endDate].txt"
# (This file will be archived too)


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
  cat("Starting 'RRW_006_Setup_Output_Directory.R'!\n")
  
  
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
  addFiles(outputDirectory, startDate, endDate, "RRW_v1")
  
  
  cat("\tDone!\n\n")
  
  
  cat("[3/3]\tSaving new folder path to a text file for easy access...\n")
  
  
  # Save 'outputDirectory' to a text file in the "Output" folder
  # This will make it easier to reference in later scripts
  outPath <- paste0("W2_Russian_River/Output/Hydrology_Output_Location_", startDate,
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
  cat(col_green("\n'RRW_006_Setup_Output_Directory.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateInput <- function (saveDirectory, sourceField) {
  
  # Ensure that the directory provided by the user is valid
  
  # Also, 'saveDirectory' could contain either a local path or 
  # a partial SharePoint path
  # This function will help clarify which type of path it is
  
  
  # Start by checking if the directory is a SharePoint fragment
  # If it exists on SharePoint, convert 'saveDirectory' into a SharePoint path
  saveDirectory <- saveDirectory |>
    sharepointPathCheck(isFolder = TRUE)
  
  
  # If the directory cannot be found, notify the user
  if (!dir.exists(saveDirectory)) {
    
    stop(paste0("Cannot Find the Specified Directory\n\n",
                "In the RR Workflow Control File, the desired location ",
                "to store the model outputs was specified to be \"",
                saveDirectory, "\"\n\n",
                "However, this location does not appear to exist. ",
                if_else(grepl("\\.", saveDirectory), 
                        paste0("A folder directory (not a filename) should ",
                               "be the input. "), 
                        ""),
                "Please update the value specified for \"", sourceField,
                "\" in the control spreadsheet.") |>
           errWrap())
    
  }
  
  
  # Return 'saveDirectory' if there are no issues
  # (If 'saveDirectory' points to a SharePoint location, it has been updated 
  #  in this function to reflect that)
  return(saveDirectory)
  
}



generateFolders <- function (saveDirectory,
                             models = c("PRMS", "SRP")) {
  
  # In 'saveDirectory', a new folder will be created for the imminent 
  # model runs of PRMS, SRP, and DWRAT
  
  # By default, the folder's name will be the current date
  
  
  # However, checks will be necessary to ensure that 
  # this name is not already in use
  mainName <- chooseFolderName(saveDirectory)
  
  
  # Create the directory 'mainName'
  
  # In addition, create sub-folders for "PRMS", "SRP", and "DWRAT"
  # In each of these folders, create "Input" and "Output" folders
  newDirectories <- c(paste0(saveDirectory, "/", mainName, "/", models[1], "/Input"),
                      paste0(saveDirectory, "/", mainName, "/", models[1], "/Output"),
                      paste0(saveDirectory, "/", mainName, "/", models[2], "/Input"),
                      paste0(saveDirectory, "/", mainName, "/", models[2], "/Output"),
                      paste0(saveDirectory, "/", mainName, "/DWRAT/Input"),
                      paste0(saveDirectory, "/", mainName, "/DWRAT/Output"),
                      paste0(saveDirectory, "/", mainName, "/DWRAT/Output/LRR_Connected"),
                      paste0(saveDirectory, "/", mainName, "/DWRAT/Output/URR_Connected")) |>
    normalizePath(mustWork = FALSE)
  
  
  # Create the folders
  newDirectories |>
    map_lgl(~ dir.create(., recursive = TRUE))
    
  
  # Ensure that all folders were created successfully
  # If not, output an error
  if (!all(dir.exists(newDirectories))) {
    
    missingDirectories <- which(!dir.exists(newDirectories))
    
    
    stop(paste0("Could Not Create Folder",
                if_else(length(missingDirectories) > 1, "s", ""),
                "\n\n",
                "The script failed to create ", length(missingDirectories),
                " folder", if_else(length(missingDirectories) > 1, "s", ""),
                ". The cause of this issue is unknown (maybe a permission ",
                "issue). Please investigate.\n\n",
                "The missing folder",
                if_else(length(missingDirectories) > 1, "s are:", " is:"),
                "\n\n",
                vec2QuotedStr(newDirectories[missingDirectories]) |>
                  paste0(collapse = "\n\n")) |>
           errWrap())
    
  }
  
  
  # Return the normalized path to 'mainName'
  return(paste0(saveDirectory, "/", mainName) |>
           normalizePath())
  
}



chooseFolderName <- function (saveDirectory) {
  
  # Decide on the primary folder name that will be added to 'saveDirectory'
  # It should not conflict with existing folders
  
  
  # The default preferred name is the run date (today)
  folderName <- Sys.Date() |> as.character()
  
  
  # Get the current contents of 'saveDirectory'
  dirContents <- list.files(saveDirectory)
  
  
  # If 'folderName' does NOT appear in 'dirContents', return it without any edits
  if (!(folderName %in% dirContents)) {
    return(folderName)
  }
  
  
  # However, if that name is already in use, 'folderName' must be modified
  # A suffix will be appended to the name: "_(#[INDEX])"
  
  # The value of "INDEX" will depend on how many existing folders there are 
  # with this suffix attached
  if (sum(grepl(paste0(folderName, "_\\("), dirContents)) > 0) {
    
    # If this code block is executed, there are at least two folders  
    # that have been created today in this directory
    
    # One would have the standard name (e.g., "2026-03-04"), and the second 
    # folder (as well as any subsequent folders) would have the suffix attached
    # (e.g., "2026-03-04_(#2)")
    
    # Among these options, find the maximum number in 
    # their names in 'dirContents'
    # (Note: This is specifically among the folders that contain today's date)
    index <- dirContents |>
      str_subset(paste0(folderName, "_\\(")) |>
      str_extract("(?<=\\(#)[0-9]+") |> 
      as.numeric() |>
      max()
    
    # The regex in str_extract() looks for "(#" before the number, but  
    # does not include "(#" in the actual extracted string (that will only
    # contain the numeric digits)
    
    
    # This new folder that will be created will contain the next number up
    index <- index + 1
    
    
  # If this is the first folder with a suffix, append the number 2 to its name
  # (not 1 because the folder that lacks this suffix is the first instance)
  } else {
    
    index <- 2
    
  }
  
  
  # Return 'folderName' with the suffix attached
  return(paste0(folderName, "_(#", index, ")"))
  
}



addFiles <- function (outputDirectory, startDate, endDate, workflowVersion) {
  
  # Create metadata about the process in 'outputDirectory'
  # Also, copy the "renv" lock file there
  
  
  # Gather various information about the process into one data frame
  metaDF <- tibble(MODEL_RUN_DATE = Sys.Date(),
                   WORKFLOW_VERSION = workflowVersion,
                   R_VERSION = sessionInfo()[["R.version"]][["version.string"]],
                   MODELER_NAME = Sys.info()[["user"]],
                   LATEST_GIT_HASH = getGitHash(),
                   METEOROLOGICAL_START = startDate,
                   METEOROLOGICAL_END = endDate,
                   METADATA_DF_FIRST_DEFINED = Sys.time(),
                   CURRENT_WATER_YEAR = if_else(month(Sys.Date()) < 10,
                                                year(Sys.Date()),
                                                year(Sys.Date()) + 1))
  
  
  # The initial version of 'metaDF' contains information about:
  #   (*) The person running the scripts
  #   (*) 'startDate' and 'endDate'
  #   (*) The approximate creation datetime of the metadata dataframe
  #   (*) The current water year
  
  
  # Write 'metaDF' to a file
  metaDF |>
    writeOutput(paste0(outputDirectory, "/metadata.csv"))
  
  
  # Finally, copy the "renv.lock" file located in the root directory
  # Store it in the same location as the metadata file
  copyFile(from = "renv.lock",
           to = paste0(outputDirectory, "/renv.lock"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

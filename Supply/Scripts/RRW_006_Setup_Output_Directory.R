# Setup the hydrology folder that will store the model input and output files
# Metadata will be generated as well for this procedure run


# This script only requires "HYDROLOGY_OUTPUT_LOCATION" to be filled in
# with a path in "RR_Workflow_Control_File.xlsx"

# A new folder will be created there with sub-folders for the inputs and outputs 
# of PRMS, SRP, and DWRAT

# A CSV file will also be generated that contains information about the procedure

# The meteorological CSV file from the previous script will be 
# copied there as well
# ("ProcessedData/PRMS_Meteorological_[startDate]_[endDate].csv")

# Its pre-PRISM version will be included too
# ("ProcessedData/PRMS_Pre-PRISM_Meteorological_[startDate]_[endDate].csv")

# The weather station input files will be archived in this folder as well


# After that, one additional output will be added to the "ProcessedData" folder

# It will be a text file containing a single line that specifies 
# the path to the newly generated directory

# Its filename will be "Hydrology_Output_Folder_[startDate]_[endDate].txt"



#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_006_Setup_Output_Directory.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Verify that the meteorological CSV file exists
  # (This is a sign that the previous script completed its procedure)
  meteorPath <- paste0("ProcessedData/PRMS_Meteorological_", startDate,
                       "_", endDate, ".csv") |>
    checkForPreviousOutput()
  
  
  # Check for the "Pre-PRISM" version of this file as well
  prePrismMeteor <- paste0("ProcessedData/PRMS_Meteorological_", startDate,
                           "_", endDate, ".csv") |>
    checkForPreviousOutput()
  
  
  cat("[1/3]\tCreating new folders...\n")
  
  
  # Get the location where a new folder will be created
  saveDirectory <- getFromControl_RR("HYDROLOGY_OUTPUT_LOCATION")
  
  
  # Confirm that the user's specification is valid
  saveDirectory <- validateInput(saveDirectory, "HYDROLOGY_OUTPUT_LOCATION")
  
  
  # Next, generate the directory and its sub-folders
  outputDirectory <- generateFolders(saveDirectory)
  
  
  cat("\tDone!\n\n")
  
  
  cat("[2/3]\tGenerating metadata and copying meteorological file...\n")
  
  
  # Add metadata and the meteorological CSV to this new location
  addFiles(outputDirectory, meteorPath, prePrismMeteor, startDate, endDate)
  
  
  cat("\tDone!\n\n")
  
  
  cat("[3/3]\tSaving new folder path to a text file for easy access...\n")
  
  
  # Save 'outputDirectory' to a text file in the "ProcessedData" folder
  # This will make it easier to reference in later scripts
  outPath <- paste0("ProcessedData/Hydrology_Output_Location_", startDate,
                    "_", endDate, ".txt")
  
  outputDirectory |>
    writeOutput(outPath)
  
  
  # Save that file to 'outputDirectory' too
  
  # Edit 'outPath' to point to 'outputDirectory' instead of "ProcessedData"
  outPath <- outPath |>
    str_remove("^.+[/\\\\]") |>
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
  if (dir.exists(makeSharePointPath(saveDirectory))) {
    
    saveDirectory <- makeSharePointPath(saveDirectory)
    
  }
  
  
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



generateFolders <- function (saveDirectory) {
  
  # In 'saveDirectory', a new folder will be created for the imminent 
  # model runs of PRMS, SRP, and DWRAT
  
  # By default, the folder's name will be the current date
  
  
  # However, checks will be necessary to ensure that 
  # this name is not already in use
  mainName <- chooseFolderName(saveDirectory)
  
  
  # Create the directory 'mainName'
  
  # In addition, create sub-folders for "PRMS", "SRP", and "DWRAT"
  # In each of these folders, create "Input" and "Output" folders
  newDirectories <- c(paste0(saveDirectory, "/", mainName, "/PRMS/Input"),
                      paste0(saveDirectory, "/", mainName, "/PRMS/Output"),
                      paste0(saveDirectory, "/", mainName, "/SRP/Input"),
                      paste0(saveDirectory, "/", mainName, "/SRP/Output"),
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
  if (anyFalse(dir.exists(newDirectories))) {
    
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



addFiles <- function (outputDirectory, meteorPath, prePrismMeteor, 
                      startDate, endDate) {
  
  # Create metadata about the process in 'outputDirectory'
  # Also, copy meteorological files and the "renv" lock file there
  
  
  # Gather various information about the process into one data frame
  metaDF <- tibble(MODEL_RUN_DATE = Sys.Date(),
                   WORKFLOW_VERSION = "RRW",
                   MODELER_NAME = Sys.info()[["user"]],
                   LATEST_GIT_HASH = getGitHash(),
                   METEOROLOGICAL_START = startDate,
                   METEOROLOGICAL_END = endDate,
                   PRMS_METEOROLOGICAL_FILE_CREATED = 
                     file.info(meteorPath)[["ctime"]],
                   METADATA_DF_FIRST_DEFINED = Sys.time(),
                   CURRENT_WATER_YEAR = if_else(month(Sys.Date()) < 10,
                                                year(Sys.Date()),
                                                year(Sys.Date()) + 1))
  
  
  # The initial version of 'metaDF' contains information about:
  #   (*) The person running the scripts
  #   (*) 'startDate' and 'endDate'
  #   (*) The creation datetime of the meteorological CSV
  #   (*) The approximate creation datetime of the metadata dataframe
  #   (*) The current water year
  
  
  # Write 'metaDF' to a file
  metaDF |>
    writeOutput(paste0(outputDirectory, "/metadata.csv"))
  
  
  # After that, copy 'meteorDF' to 'outputDirectory'
  # (Place it in the "Input" folder under "PRMS")
  newMeteorPath <- paste0(outputDirectory, "/PRMS/Input/", 
                          meteorPath |> str_remove("^.+[/\\\\]")) |>
    normalizePath(mustWork = FALSE)
  
  
  # Copy the file
  copyFile(from = meteorPath, to = newMeteorPath)
  
  
  # Attempt the same copy process with the "Pre-PRISM" version of 
  # the meteorological CSV file
  copyFile(from = prePrismMeteor, 
           to = newMeteorPath |> 
             str_replace("^(.+[/\\\\])PRMS_Meteorological_", 
                         "\\1PRMS_Pre-PRISM_Meteorological_"), 
           quietly = TRUE)
  
  
  # Save the PRISM grid-cell-averaged precipitation data too
  # There is one file each for the PRMS and SRP model domains
  prmsGridPath <- paste0("WebData/PRISM_PRMS_Domain_Data_", 
                         getModeledWY(endDate)[1], "_", 
                         endDate, ".csv")
  
  
  srpGridPath <- paste0("WebData/PRISM_SRP_Domain_Data_", 
                        getModeledWY(endDate)[1], "_", 
                        endDate, ".csv")
  
  
  copyFile(prmsGridPath, paste0(outputDirectory, "/PRMS/Input/",
                                prmsGridPath |> str_remove("^.+/")), 
           quietly = TRUE)
  
  
  copyFile(srpGridPath, paste0(outputDirectory, "/SRP/Input/",
                               srpGridPath |> str_remove("^.+/")), 
           quietly = TRUE)
  
  
  # After that, save the outlier bounds and regression data for precipitation gages
  outlierPath <- getFromControl_RR("PRMS_PRECIP_GAGE_OUTLIER_BOUNDS") |>
    sharepointPathCheck(isFolder = FALSE)
  
  regressionPath <- getFromControl_RR("PRMS_PRECIP_GAGE_CORRELATION_TABLE") |>
    sharepointPathCheck(isFolder = FALSE)
  
  
  # Copy both files to the "PRMS" folder
  copyFile(outlierPath, paste0(outputDirectory, "/PRMS/Input/",
                               outlierPath |> str_remove("^.+[/\\\\]")))
  
  copyFile(regressionPath, paste0(outputDirectory, "/PRMS/Input/",
                                  regressionPath |> str_remove("^.+[/\\\\]")))
  
  
  # Each of the weather station input files will be archived as well
  copyStationInputFile("PRISM_PRMS_STATIONS_CSV", outputDirectory, "PRMS")
  copyStationInputFile("NOAA_STATIONS_CSV", outputDirectory, "PRMS")
  copyStationInputFile("RAWS_STATIONS_CSV", outputDirectory, "PRMS")
  copyStationInputFile("CIMIS_STATIONS_CSV", outputDirectory, "PRMS")
  copyStationInputFile("PRISM_PRMS_GRID_CELLS_CSV", outputDirectory, "PRMS")
  
  copyStationInputFile("PRISM_SRP_STATIONS_CSV", outputDirectory, "SRP")
  copyStationInputFile("PRISM_SRP_GRID_CELLS_CSV", outputDirectory, "SRP")
  
  
  # Finally, copy the "renv.lock" file located in the root "Supply" directory
  # Store it in the same location as the metadata file
  copyFile(from = "renv.lock",
           to = paste0(outputDirectory, "/renv.lock"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



copyStationInputFile <- function (sourceField, outputDirectory, model = "PRMS") {
  
  # Get the station input file's path from the control file
  
  # Then copy it into the new hydrology folder
  
  
  # Read in the path from the control file
  inputPath <- getFromControl_RR(sourceField) |>
    sharepointPathCheck(isFolder = FALSE)
  
  
  # Set the output path next
  # The filename will be the same as in 'inputPath'
  # (But any earlier folders in the path are replaced)
  outputPath <- paste0(outputDirectory, "/", model, "/Input/",
                       inputPath |> str_remove("^.+[/\\\\]"))
  
  
  # Copy the file
  copyFile(inputPath, outputPath, quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

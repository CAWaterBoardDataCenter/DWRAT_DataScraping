# Setup the hydrology folder that will store the model input and output files
# Metadata will be generated as well for this procedure run


# This script only requires "HYDROLOGY_OUTPUT_LOCATION" to be filled in
# with a path in "RR_Workflow_Control_File.xlsx"

# A new folder will be created there with sub-folders for the inputs and outputs 
# of SRP, RRIHM, and DWRAT

# A CSV file will also be generated that contains information about the procedure

# The meteorological CSV file from the previous script will be 
# copied there as well
# ("W2_Russian_River/Output/SRP_Meteorological_[startDate]_[endDate].csv")

# The weather station input files will be archived in this folder as well


# After that, one additional output will be added to the "Output" folder

# It will be a text file containing a single line that specifies 
# the path to the newly generated directory

# Its filename will be "Hydrology_Output_Folder_[startDate]_[endDate].txt"


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("W2_Russian_River/Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_v2_007_Setup_Output_Directory.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Verify that the SRP meteorological CSV file exists
  # (This is a sign that the previous script completed its procedure)
  meteorPath <- paste0("W2_Russian_River/Output/SRP_Meteorological_", startDate,
                       "_", endDate, ".csv") |>
    checkForPreviousOutput()
  
  
  # Import functions from the v1 workflow's corresponding script
  c("validateInput", "generateFolders", "chooseFolderName",
    "copyStationInputFile") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_007_Setup_Output_Directory.R", .))
  
  
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
  
  
  # Save 'outputDirectory' to a text file in the "Output" folder
  # This will make it easier to reference in later scripts
  outputDirectory |>
    writeOutput(paste0("W2_Russian_River/Output/Hydrology_Output_Location_", startDate,
                       "_", endDate, ".txt"))
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_v2_007_Setup_Output_Directory.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



addFiles <- function (outputDirectory, meteorPath, prePrismMeteor, 
                      startDate, endDate) {
  
  # Create metadata about the process in 'outputDirectory'
  # Also, copy meteorological files and the "renv" lock file there
  
  
  # Gather various information about the process into one data frame
  metaDF <- tibble(MODEL_RUN_DATE = Sys.Date(),
                   WORKFLOW_VERSION = "RRW_v2",
                   MODELER_NAME = Sys.info()[["user"]],
                   LATEST_GIT_HASH = getGitHash(),
                   METEOROLOGICAL_START = startDate,
                   METEOROLOGICAL_END = endDate,
                   SRP_MODEL_REVISION = "REV1", 
                   SRP_METEOROLOGICAL_FILE_CREATED = 
                     file.info(meteorPath)[["ctime"]],
                   METADATA_DF_FIRST_DEFINED = Sys.time(),
                   CURRENT_WATER_YEAR = if_else(month(Sys.Date()) < 10,
                                                year(Sys.Date()),
                                                year(Sys.Date()) + 1))
  
  
  # The initial version of 'metaDF' contains information about:
  #   (*) The person running the scripts
  #   (*) 'startDate' and 'endDate'
  #   (*) The creation datetime of the SRP meteorological CSV
  #   (*) The approximate creation datetime of the metadata dataframe
  #   (*) The current water year
  
  
  # Write 'metaDF' to a file
  metaDF |>
    writeOutput(paste0(outputDirectory, "/metadata.csv"))
  
  
  # After that, copy 'meteorDF' to 'outputDirectory'
  # (Place it in the "Input" folder under "SRP")
  newMeteorPath <- paste0(outputDirectory, "/SRP/Input/", 
                          meteorPath |> str_remove("^.+[/\\\\]")) |>
    normalizePath(mustWork = FALSE)
  
  
  # Copy the file
  copyFile(from = meteorPath, to = newMeteorPath)
  
  
  # Save the PRISM grid-cell-averaged precipitation data too
  # There is one file each for the RRIHM and SRP model domains
  prmsGridPath <- paste0("W2_Russian_River/Intermediate/PRISM_PRMS_Domain_Data_", 
                         getModeledWY(endDate)[1], "_", 
                         endDate, ".csv")
  
  
  srpGridPath <- paste0("W2_Russian_River/Intermediate/PRISM_SRP_Domain_Data_", 
                        getModeledWY(endDate)[1], "_", 
                        endDate, ".csv")
  
  
  copyFile(prmsGridPath, paste0(outputDirectory, "/RRIHM/Input/",
                                prmsGridPath |> str_remove("^.+/")), 
           quietly = TRUE)
  
  
  copyFile(srpGridPath, paste0(outputDirectory, "/SRP/Input/",
                               srpGridPath |> str_remove("^.+/")), 
           quietly = TRUE)
  
  
  # Each of the weather station input files will be archived as well
  copyStationInputFile("PRISM_PRMS_STATIONS_CSV", outputDirectory, "RRIHM")
  copyStationInputFile("NOAA_STATIONS_CSV", outputDirectory, "RRIHM")
  copyStationInputFile("RAWS_STATIONS_CSV", outputDirectory, "RRIHM")
  copyStationInputFile("CIMIS_STATIONS_CSV", outputDirectory, "RRIHM")
  copyStationInputFile("CDEC_STATIONS_CSV", outputDirectory, "RRIHM")
  copyStationInputFile("PRISM_PRMS_GRID_CELLS_CSV", outputDirectory, "RRIHM")
  
  copyStationInputFile("PRISM_SRP_STATIONS_CSV", outputDirectory, "SRP")
  copyStationInputFile("PRISM_SRP_GRID_CELLS_CSV", outputDirectory, "SRP")
  
  
  # Finally, copy the "renv.lock" file located in the root "Supply" directory
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

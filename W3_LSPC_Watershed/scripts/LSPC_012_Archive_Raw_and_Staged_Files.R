# Archive the raw and staged data in the previously established hydrology folder

# The entirety of the "shared" folder will be archived
# (as no further changes will occur to it)

# For watershed-specific folders, only the "raw" and "staged" folders 
# will be copied over

# The "candidate" and "curated" folders will still be adjusted in later steps
# of the workflow, so they will be archived later


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
  cat("Starting 'LSPC_012_Archive_Raw_and_Staged_Files.R'!\n")
  
  
  # Import a function from another script
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R",
                  "read_all_lspc_project_control")
  
  
  # Import the data scraping bounds next
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Read in the LSPC weather control file too
  # (A list of watersheds is needed)
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validation of control file
  
  
  # Use the imported function to get storage information for every watershed
  wsDir <- controlDF |>
    read_all_lspc_project_control(worksheet = "Storage")
  
  # To Do: Validation of storage worksheets
  
  
  # Finally, get the location of the archive folder
  dirPath <- get_lspc_archive_folder(startDate, endDate)
  
  
  # Define a vector of paths to the folders that will be copied
  cat("\n[1/2]\tIdentifying folders to archive...\n")
  
  
  # Start with getting the shared folder's path
  # Every control file has this path specified (and ideally, they should all be identical)
  # Confirm that here
  
  fromPath <- wsDir |>
    map_chr(~ paste0("W3_LSPC_Watershed/",
                     filter(., scope == "shared" & level == "root") |>
                       select(path) |> unlist(use.names = FALSE))) |>
    unique()
  
  stopifnot(length(fromPath) == 1)
  
  # To Do: Have a function for building paths 
  # (and checking if all project files have the same paths)
  
  
  # The write path for the shared folder will use the same folder name
  # (but will extend from 'dirPath' instead)
  toPath <- paste0(dirPath, "/",
                   fromPath |> extract_filename())
  
  
  # At this point, 'fromPath' and 'toPath' only contain the directory names 
  # for the shared folder
  
  # The next step is to add watershed folders' paths to them
  
  
  # At this stage (for this script), only the "raw" and "staged" folders are 
  # required from each watershed's project folder
  targetFolders <- c("raw", "staged")
  
  
  # Start with getting the root paths for each watershed project
  # Append the target folders' names to those files
  # Then, add these new paths to 'fromPath'
  newFromPaths <- wsDir |>
    map_chr(~ paste0("W3_LSPC_Watershed/", 
                     filter(., scope == "project" & level == "root") |>
                       select(path) |> unlist(use.names = FALSE)))
  
  # Right now, this vector contains the paths to the root watershed folders
  # Adding "/raw" or "/staged" to these root paths will give the paths to those
  # target folders
  
  # But before doing that, get started on the output folder paths
  # They will share the same root folder names for each watershed
  newToPaths <- paste0(dirPath, "/",
                       newFromPaths |> extract_filename(), "/Input")
  
  
  # Next, for both variables, append the 'targetFolders' to them
  # Then, include them in 'fromPath' and 'toPath'
  fromPath <- c(fromPath,
                newFromPaths |>
                  map(~ paste0(., "/", targetFolders)) |>
                  unlist(use.names = FALSE))
  
  toPath <- c(toPath,
              newToPaths |>
                map(~ paste0(., "/", targetFolders)) |>
                unlist(use.names = FALSE))
  
  
  # To Do: Validate that all 'fromPath' directories exist
  
  
  # Finally, make sure all paths are absolute
  fromPath <- fromPath |>
    normalizePath(mustWork = TRUE)
  
  toPath <- toPath |>
    normalizePath(mustWork = FALSE)
  
  
  cat("\tDone!\n\n")
  
  
  # Copy the folders over next
  cat("[2/2]\tCopying folders...\n")
  
  
  # Use `dir_copy` to copy them over
  dir_copy(fromPath, toPath, overwrite = TRUE)
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_012_Archive_Raw_and_Staged_Files.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



get_lspc_archive_folder <- function (startDate, endDate) {
  
  # Verify that previous scripts were run successfully and that a hydrology 
  # folder was created to store metadata and model files
  
  # This function can also return the directory path
  
  
  return(get_archive_path(startDate, endDate, "W3_LSPC_Watershed/data/shared", 
                          nameStr = "Archive_Location_"))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

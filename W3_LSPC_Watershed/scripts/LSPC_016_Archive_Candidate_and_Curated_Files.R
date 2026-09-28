# Archive the candidate and curated data into the previously established archive folder

# (No further changes will be made to these files)


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
  cat("Starting 'LSPC_016_Archive_Candidate_and_Curated_Files.R'!\n")
  
  
  # Import function from other scripts
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R",
                  "read_all_lspc_project_control")
  
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_012_Archive_Raw_and_Staged_Files.R",
                  "get_lspc_archive_folder")
  
  
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
  
  
  # To Do: Use a generic function for this procedure and LSPC_012
  
  
  # Define a vector of paths to the folders that will be copied
  cat("\n[1/2]\tIdentifying folders to archive...\n")
  
  
  # Start with getting the shared folder's path
  # Every control file has this path specified (and ideally, they should all be identical)
  # Confirm that here
  
  
  # The folders to copy are explicitly stated here
  targetFolders <- c("candidate", "curated")
  
  
  # Start with getting the root paths for each watershed project
  # Append the target folders' names to those files
  fromPath <- wsDir |>
    map_chr(~ paste0("W3_LSPC_Watershed/", 
                     filter(., scope == "project" & level == "root") |>
                       select(path) |> unlist(use.names = FALSE)))
  
  # Right now, this vector contains the paths to the root watershed folders
  # Adding "/candidate" or "/curated" to these root paths will give the paths to those
  # target folders
  
  # But before doing that, get started on the output folder paths
  # They will share the same root folder names for each watershed
  toPath <- paste0(dirPath, "/",
                       fromPath |> extract_filename(), "/Input")
  
  
  # Next, for both variables, append the 'targetFolders' to them
  fromPath <- fromPath |>
    map(~ paste0(., "/", targetFolders)) |>
    unlist(use.names = FALSE)
  
  toPath <- toPath |>
    map(~ paste0(., "/", targetFolders)) |>
    unlist(use.names = FALSE)
  
  
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
  
  # To Do: Use zip?
  #zip(flags = "-r9")
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_016_Archive_Candidate_and_Curated_Files.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

# Confirm that directories exist for each watershed in the weather control file

# These folders must be in place before the weather data download can begin

# If files from a previous run are present, clear them out

# The "shared" folder must be cleared as well


# This script will also generate the sub-folders required for each watershed
# (both shared and project-specific folders)


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
  cat("Starting 'LSPC_003_Setup_Project_Directories.R'!\n")
  
  
  # Import the weather control file to get a list of watersheds 
  cat("\n[1/2]\tGetting list of watersheds...\n")
  
  
  controlDF <- read_lspc_weather_control()
  
  
  # To Do: Validation function for weather control file 
  
  
  cat("\tDone!\n\n")
  
  
  # Next, confirm that a directory exists for each watershed under "projects" 
  # in the "data" subfolder of the workflow folder
  cat("[2/2]\tPrepping directories...\n")
  
  
  # Create a vector of paths pointing to each watershed folder in "projects"
  dirPaths <- paste0("W3_LSPC_Watershed/data/projects/", controlDF$project_name)
  
  
  # Include the "shared" folder in this vector as well
  dirPaths <- c(dirPaths, "W3_LSPC_Watershed/data/shared")
  
  
  # If these directories already exist, delete them
  try(dir_delete(dirPaths), silent = TRUE)
  
  
  # Then, create the folders for each watershed
  dirPaths |> dir_create(recurse = FALSE)
  
  # 'recurse' is FALSE, meaning that the workflow, "data", and "projects" folders 
  # must already exist; otherwise, an error will occur
  
  
  # Finally, prepare the sub-folders under these directories
  generate_subfolders(controlDF)
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_003_Setup_Project_Directories.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



generate_subfolders <- function (controlDF) {
  
  # Each watershed's individual project control file contains the relative paths 
  # to shared data folders
  
  # These folders will be generated now to help with a later step in the workflow
  # (copying previously downloaded raw weather data)
  
  
  # Start by reading in all watershed control files
  # Get a list of data frames containing these files
  projectList <- read_all_lspc_project_control(controlDF, worksheet = "Storage")
  
  
  # To Do: Validation for the "Storage" worksheet of these control files
  
  
  # Get a list of shared folder paths from each watershed's file
  # Ensure that all watersheds have the exact same shared paths specified
  for (i in 1:length(projectList)) {
    
    # Extract the shared paths from the watershed's project control file
    
    # First get the "root" path for shared folders
    # All other shared paths will be sub-folders of this "root" path
    
    sharedRoot <- projectList[[i]] |>
      filter(scope == "shared") |>
      filter(level == "root") |> 
      select(path) |>
      unlist(use.names = FALSE)
    
    
    sharedPaths <- projectList[[i]] |>
      filter(scope == "shared") |> 
      filter(level != "root") |>
      select(path) |>
      unlist(use.names = FALSE)
    
    
    # Append these paths to 'sharedRoot'
    # (The workflow folder name is also required to ensure that the paths are correct)
    sharedPaths <- paste0("W3_LSPC_Watershed/", 
                          sharedRoot, "/",
                          sharedPaths)
    
    
    # Normalize the paths to ensure that the formatting is consistent when 
    # comparing each watershed's specified shared paths
    sharedPaths <- sharedPaths |>
      normalizePath(mustWork = FALSE)
    
    
    # If this is the first iteration, 
    # use these folder paths as a basis for comparisons
    if (i == 1) {
      
      comparisonPaths <- sharedPaths
      
    } else {
      
      # To Do: Have specific error messages for the `stopifnot` commands in this script
      
      stopifnot(length(comparisonPaths) == length(sharedPaths))
      
      stopifnot(all(comparisonPaths %in% sharedPaths))
      
      
    }
    
  }
  
  
  # Once the shared folder paths have been confirmed to be consistent among
  # every watershed control file, generate them 
  sharedPaths |> dir_create(recurse = TRUE)
    
  
  # Next, focus on generating the watershed-specific paths
  
  
  # Iterate through each watershed-specific file
  for (i in 1:nrow(controlDF)) {
    
    # Temporarily extract the storage data frame for this iteration's watershed
    projectDF <- projectList[[i]]
    
    
    stopifnot(controlDF$project_name[i] == 
                extract_filename(projectDF$path[projectDF$scope == "project" & 
                                                  projectDF$level == "root"]))
    
    
    # Prepare a vector of watershed-specific folder paths to generate
    pathVec <- paste0("W3_LSPC_Watershed/",
             projectDF$path[projectDF$scope != "shared" & projectDF$level == "root"], "/",
             projectDF$path[projectDF$scope != "shared" & projectDF$level != "root"])
      
    
    # In the watershed-specific control files, a "root" row contains the basic path 
    # to the project-specific folder
    # Then, the non-root entries in the table specify the child folders of that "root"
    
    
    # The next step is to generate these folders
    pathVec |> dir_create(recurse = TRUE)
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



read_all_lspc_project_control <- function (controlDF, worksheet) {
  
  # Given a master control file for the LSPC weather procedure,
  # read in every project-specific control file
  
  # Return a list of data frames
  
  
  # Start by allocating space for the list
  projectList <- vector(mode = "list", length = nrow(controlDF))
  
  
  # Iterate through 'controlDF'
  # Each row corresponds to a different watershed (and control file)
  for (i in 1:nrow(controlDF)) {
    
    # Read in the watershed-specific control file
    projectDF <- paste0("W3_LSPC_Watershed/",
                        controlDF$project_control_file[i]) |>
      getXLSX(worksheet = worksheet)
    
    
    # Save this data frame in 'projectList'
    projectList[[i]] <- projectDF
    
  }
  
  
  # Return 'projectList'
  return(projectList)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

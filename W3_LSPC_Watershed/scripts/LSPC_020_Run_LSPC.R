# Run an LSPC model for each watershed

# To assist with this, temporary batch files are generated for each watershed


# To Do: Run different watersheds' models in parallel?


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
  cat("Starting 'LSPC_020_Run_LSPC.R'!\n")
  
  
  # Import functions from other scripts
  c("get_lspc_model_directories", "validate_lspc_model_folder") |>
    map(~ functionStealer("W3_LSPC_Watershed/scripts/LSPC_017_Check_LSPC_Model_Directory.R", .))
  
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_018_Migrate_Weather_Files.R",
                  "get_lspc_inp_paths")
  
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_004a_Download_Shared_Climate_Data.R",
                  "run_temp_bat")
  
  
  # Read in the LSPC weather control file
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validation of control file
  
  
  # Get the directories for each watershed model folder too
  modelFolder <- controlDF |>
    get_lspc_model_directories()
  
  
  modelFolder |>
    map(validate_lspc_model_folder)
  
  
  # After that, get the paths to the LSPC executables and the input files 
  # for each watershed
  exePaths <- modelFolder |>
    get_lspc_exe_paths()
  
  
  inpPaths <- modelFolder |>
    get_lspc_inp_paths()
  
  
  # First clear out the output directories for each watershed
  cat("\n[1/2]\tClearing models' output directories...\n")
  
  
  # Note the models' output locations in a vector
  modelOutput <- paste0(modelFolder, "/Output") |>
    normalizePath(mustWork = TRUE)
  
  
  # Make sure the model folders do not contain any files
  modelOutput |> 
    list.files(full.names = TRUE) |>
    unlink()
  
  
  cat("\tDone!\n\n")
  
  
  # Notify the user of the impending model run
  cat("[2/2]\tStarting up models...\n")
  
  
  # Get the current time (for time tracking purposes)
  startTime <- Sys.time()
  
  
  # Iterate through each watershed
  for (i in 1:nrow(controlDF)) {
    
    cat("\n\n")
    paste0("\t[", i, "/", nrow(controlDF), "] ", controlDF$project_name[i]) |>
      cat()
    cat("\n\n")
    
    
    # Each batch file will contain two commands:
    #   (1) Change the working directory to the root of the watershed's LSPC model folder
    #   (2) Call the LSPC executable with the inp file as input 
    #       (and "_run" appended to the end of the inp path)
    
    c(paste0("cd ", normalizePath(modelFolder[i])),
      paste0(normalizePath(exePaths[i]), " ", normalizePath(inpPaths[i]), "_run")) |>
      run_temp_bat()
    
    # Note: "_run" causes the model to begin running instantly without using the GUI
    
  }
  
  # To Do: Run multiple watersheds' models at once 
  
  # Use furrr package to run a parallel loop? 
  
  
  
  # Get the current time (for time tracking purposes)
  endTime <- Sys.time()
  
  
  # To Do: Check for errors
  # There should be several output files
  
  
  # Output a completion message
  cat("\tDone!\n\n")
  
  
  # After that, tell the user how long the model run took
  cat(paste0("\n\nThe models ran in ", 
             difftime(endTime, startTime, units = "mins") |> round(),
             " minutes!\n"))
  
  
  # Output a completion message
  cat(col_green("\n'LSPC_020_Run_LSPC.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



get_lspc_exe_paths <- function (modelPath) {
  
  # Given the path to one or more watershed's LSPC model directory, 
  # get the path to the LSPC executable file
  
  # There should be only one exe file in each directory
  # (this is previously confirmed in validation for the model directories,
  #  but it will be double-checked here as well)
  
  
  # The file should be located in the root model folder
  exePath <- modelPath |>
    map(~ list.files(., pattern = "\\.exe$", full.names = TRUE)) |> 
    unlist() 
  
  # Even if 'modelPath' contains multiple paths, `map` will ensure that 
  # each directory will be examined for input files
  
  
  # Confirm that the number of inp paths in 'exePath' match the number of model paths
  error_if(length(modelPath) != length(exePath),
           paste0("Mismatch of LSPC Exe Files\n\n",
                  "The expected number of .exe files that should have been ",
                  "detcted is ", length(modelPath), ". However, the number of ",
                  "files found is ", length(exePath), ". Please investigate."))
  
  
  # If there are no issues, return 'exePath'
  return(exePath)
  
}



wait_for_lspc_completion <- function (outputFolder) {
  
  # Check for the completion of an LSPC run using 
  
  
  
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

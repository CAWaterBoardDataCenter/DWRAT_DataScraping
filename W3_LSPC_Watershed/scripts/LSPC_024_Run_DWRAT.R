# Run Paradigm DWRAT for LSPC watersheds
# The outputs will be written to the hydrology archive folder

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
  cat("Starting 'LSPC_024_Run_DWRAT.R'!\n")
  
  
  # Import functions from other scripts
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_012_Archive_Raw_and_Staged_Files.R",
                  "get_lspc_archive_folder")
  
  functionStealer("W2_Russian_River/scripts/HLP_003_RR_Workflow_Validation_Functions.R",
                  "checkForPreviousOutput")
  
  c("basin_end_marker", "ensure_correct_typing") |>
    map(~ functionStealer("W3_LSPC_Watershed/scripts/LSPC_023_Finalize_DWRAT_Inputs.R", .))
  
  
  # Import the user-specified start and end dates
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  cat("\n[1/2]\tPreparing files...\n")
  
  
  # Read in the LSPC weather control file
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validation of control file
  
  
  # Get the location of the archive folder
  dirPath <- get_lspc_archive_folder(startDate, endDate)
  
  
  # Confirm that every folder has the three required DWRAT input files
  dwratFiles <- c("DEMAND" = "formatted_demand.csv", 
                  "SUPPLY" = "formatted_supply.csv", 
                  "BASINS" = "generated_basins.csv")
  
  # Note: This is a named vector (which allows the values to be referenced by name)
  
  
  # Generate paths for each watershed project
  # These files should be stored under "DWRAT/Input" 
  inputPaths <- paste0(dirPath, "/", controlDF$project_name, "/DWRAT/Input") |>
    map(~ paste0(., "/", dwratFiles) |> 
          set_names(names(dwratFiles)))
  
  # (The names used in 'dwratFiles' should be applied to the paths as well)
  
  
  # Test each path
  inputPaths |> unlist() |> checkForPreviousOutput()
  
  
  # After that, get a path to Anaconda's "activate.bat" script
  # This will be needed to run the DWRAT Python scripts
  batPath <- detectAnacondaBat()
  
  
  # Get Paradigm DWRAT's LSPC DWRAT script
  scriptPath <- "Models/Paradigm_DWRAT/LSPC_DWRAT.py" |>
    normalizePath(mustWork = FALSE)
  
  
  # Confirm that the path exists
  error_if(!file.exists(scriptPath),
           paste0("DWRAT Script Not Found\n\n", 
                  "In DWRAT_DataScraping, the \"Paradigm DWRAT\" folder is located ", 
                  "in the \"Models\" folder. It should contain a key script called \"",
                  "LSPC_DWRAT.py\". However, it could not be found. Please ",
                  "investigate."))
  
  
  cat("\tDone!\n\n")
  
  
  # Next, try to run DWRAT for each watershed
  cat("[2/2]\tRunning DWRAT...\n")
  
  
  # Run DWRAT for each watershed, one at a time
  for (i in 1:nrow(controlDF)) {
    
    cat("\n\n")
    paste0("\t[", i, "/", nrow(controlDF), "] ", controlDF$project_name[i]) |>
      cat()
    cat("\n\n")
    
    
    # Get the path to the storage location for the DWRAT output files 
    writePath <- paste0(dirPath, "/", controlDF$project_name[i], "/DWRAT/Output") |>
      normalizePath(mustWork = TRUE)
    
    
    # After that, start by checking if multiple DWRAT runs are needed
    # (One DWRAT run is required for each distinct flow path)
    
    # To do that, read in the basins CSV and check for the string that 
    # marks the end of a flow path ("000")
    # Note: That string was also modified to avoid type parsing issues
    
    # First read in the file
    basinsDF <- inputPaths[[i]][["BASINS"]] |>
      getFile()
    
    
    # Then, get the outlet subwatersheds in 'basinsDF'
    # (Each of these subwatersheds have a "FLOWS_TO" value equal to the end string)
    flowOutlets <- basinsDF |> 
      get_flow_paths(inputPaths[[i]][["BASINS"]], controlDF$project_name[i])
    
    
    # Notify the user about the number of unique flow paths
    cat("\n\n")
    paste0("\tNumber of distinct flow paths: ", length(flowOutlets)) |>
      cat()
    cat("\n\n")
    
    
    # If there are multiple flow paths, create subfolders in 'writePath'
    # Some outputs share the same exact names, 
    # so this will prevent accidental overwrites during the DWRAT runs
    if (length(flowOutlets) > 1) {
      
      catch_warnings_and_errors({
        paste0(writePath, "/", 1:length(flowOutlets)) |>
          dir_create()
      })
      
    }
    
    
    # Prepare to run DWRAT for each unique path
    for (j in 1:length(flowOutlets)) {
      
      cat("\n\n")
      paste0("\t\t[", j, "/", length(flowOutlets), "] Outlet: ", flowOutlets[j]) |>
      cat("\n\n")
      
      
      # Before each DWRAT run, adjust the LSPC DWRAT Python script
      
      # Several values are needed for this task:
      #  (*) The name of the flow path's outlet
      #  (*) The path to the demand input file
      #  (*) The path to the supply input file
      #  (*) The path to the basins input file
      #  (*) The planned model name for this run
      #  (*) The output location for the model files
      
      # Aside from the model name and sorta the output path, 
      # all of these values are already known
      
      # For the model name, construct a name based on the watershed name
      # and the outlet subwatershed name
      modelName <- paste0(controlDF$project_name[i], "_",
                          flowOutlets[j])
      
      
      # For the output path, if there are multiple flow paths, 
      # it will be slightly different from 'writePath'
      if (length(flowOutlets) > 1) {
        
        # When there are multiple flow paths, subfolders were created earlier
        # So in this situation, we would use those subfolders to store the outputs
        outputPath <- paste0(writePath, "/", j)
        
      } else {
        
        # Otherwise, when there's only one flow path for the watershed, 
        # 'writePath' and 'outputPath' can be identical
        outputPath <- writePath
        
      }
      
      
      # Next, update the LSPC DWRAT script with these values
      edit_script(scriptPath, 
                  outletVal = flowOutlets[j], 
                  supplyVal = inputPaths[[i]][["SUPPLY"]], 
                  demandVal = inputPaths[[i]][["DEMAND"]], 
                  basinsVal = inputPaths[[i]][["BASINS"]],
                  modelVal = modelName, 
                  directoryVal = outputPath)
      
      
      # Once the script has been updated, it's time to run DWRAT
      dwratRes <- system(paste0(batPath, " && ",
                                "conda activate paradigm-dwrat && ",
                                "python ", shQuote(scriptPath)), 
                         intern = TRUE)
      
      
      # Confirm that DWRAT ran successfully
      check_for_errors(outputPath, modelName, scriptPath, dwratRes)
      
      # 'dwratRes' is already present in a log file among the DWRAT outputs,
      # so it does not need to be written to an output file
      
      
    } # End of 'j' loop through flow paths
    
    
    # If there are multiple flow paths in a watershed, 
    # some of the outputs can be combined and stored in the root of the DWRAT Output folder
    if (length(flowOutlets) > 1) {
      
      combine_dwrat_outputs(writePath, length(flowOutlets))
      
    }
    
    
  } # End of 'i' loop through watersheds
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_024_Run_DWRAT.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



get_flow_paths <- function (basinsDF, basinPath, wsName) {
  
  # Use this function to get the number of unique flow paths 
  
  # This function will identify rows in 'basinsDF' that contain 
  # the subwatershed outlet strings 
  
  # The most downstream subwatershed in a flow path will have that outlet string
  # in its "FLOWS_TO" column
  
  
  # First, get the outlet string as it would appear in 'basinsDF'
  endString <- make_basin_end_string()
  
  
  # Next, get the subwatersheds in 'basinsDF' that use this marker
  flowPaths <- basinsDF$BASIN[basinsDF$FLOWS_TO == endString]
  
  
  # Make sure there's at least one match
  error_if(length(flowPaths) == 0,
           paste0("No Outlet Found in Basin Connectivity File\n\n",
                  "The file \"", basinPath, "\" should ",
                  "contain the immediate downstream subwatershed of each ",
                  "subwatershed in ", wsName, ". The outlet ",
                  "subwatershed should be a unique value (\"", endString, "\"). ",
                  "However, it could not be found in the file. Please investigate."))
  
  
  # Return 'flowPaths'
  return(flowPaths)
  
}



make_basin_end_string <- function () {
  
  # In the connectivity tables, this string appears in the "FLOWS_TO" column
  # to mark the end of a flow path
  
  # (i.e., the subwatershed that appears in "BASIN" does not have any downstream
  #  subwatersheds)
  
  return(basin_end_marker() |>
           ensure_correct_typing())
  
  # `ensure_correct_typing` was used in the demand and basin tables to prevent
  # type parsing errors (where the values are read in as numbers instead of strings)
  
  # Thus, to inspect the connectivity tables, it must be applied here too
  
}



edit_script <- function (scriptPath, outletVal, supplyVal, demandVal, basinsVal,
                         modelVal, directoryVal) {
  
  # Edit the filepaths and parameters that appear in the main DWRAT script
  
  # The following variables and parameters will be updated: 
  #  (*) outlet - Identifies this model run's flow path of interest
  #  (*) supply_file - Path to supply input
  #  (*) demand_file - Path to demand input
  #  (*) basin_file - Path to basins input
  #  (*) modelName - Identifier for this model run
  #  (*) directoryPath - Output directory for model files
  
  
  # Before proceeding, make sure the input file paths use only forward slashes ("/")
  demandVal <- demandVal |>
    normalizePath(winslash = "/", mustWork = TRUE)
  
  supplyVal <- supplyVal |>
    normalizePath(winslash = "/", mustWork = TRUE)
  
  basinsVal <- basinsVal |>
    normalizePath(winslash = "/", mustWork = TRUE)
  
  directoryVal <- directoryVal |>
    normalizePath(winslash = "/", mustWork = TRUE)
  
  
  # After that, begin editing the file 
  
  # Read in the lines of code from this script
  dwratScript <- getFile(scriptPath)
  
  
  # Modify the paths in 'dwratScript'
  dwratScript <- dwratScript |>
    locate_and_replace("^outlet", outletVal) |>
    locate_and_replace("^demand_file", demandVal) |>
    locate_and_replace("^supply_file", supplyVal) |>
    locate_and_replace("^basin_file", basinsVal) |>
    locate_and_replace("modelName=", modelVal) |>
    locate_and_replace("directoryPath=", directoryVal)
  
  
  # Update the Python script with these changes
  dwratScript |>
    writeOutput(scriptPath, quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



locate_and_replace <- function (dwratScript, matchPattern, newPath) {
  
  # In the script text of 'dwratScript', locate a specific line of code
  # using the regex pattern in 'matchPattern'
  
  # Then, on that line, replace the filepath (encased in single quotes) 
  # with the path in 'newPath'
  
  
  # Try to find 'matchPattern' in 'dwratScript'
  # (There should be exactly one match)
  matchIndex <- find_matches(dwratScript, matchPattern)
  
  
  # Replace the filepath contained within single quotes on this line of code
  # Use 'newPath' as the replacement
  dwratScript[matchIndex] <- dwratScript[matchIndex] %>%
    str_replace("'.+'\\s*",
                paste0("'", newPath, "'"))
  
  
  # Return the updated 'dwratScript'
  return(dwratScript)
  
}



check_for_errors <- function (outPath, projectName, dwratRes, scriptPath) {
  
  # Confirm that DWRAT ran successfully
  
  
  # Look for error messages in the console output of 'dwratRes'
  if (any(grepl("Error", dwratRes, ignore.case = TRUE))) {
    
    # Include the model run outputs in the console 
    cat("\n\nModel Output Message(s):\n\n")
    print(dwratRes)
    
    
    # Save 'dwratRes' to a file too
    writeOutput(dwratRes, "W3_LSPC_Watershed/DWRAT_Output_Messages.txt")
    
    
    paste0("Paradigm DWRAT Error\n\n",
           "An error was encountered while running DWRAT. Please investigate ",
           "the model's output messages (included above and in a file).\n\n", 
           "(This error occurred for \"", scriptPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Confirm that all required outputs were generated
  outFiles <- c(paste0("_", projectName, ".log"),
                "_preferred_output.csv", 
                "appropriative_demand.csv", 
                "appropriative_user_connectivity_matrix.csv",
                "appropriative_user_matrix.csv",
                paste0("basin_appropriative_output_", projectName, ".csv"),
                "basin_connectivity_matrix.csv",
                paste0("basin_riparian_output_", projectName, ".csv"),
                "flows.csv", 
                "riparian_demand.csv",
                "riparian_user_connectivity_matrix.csv",
                "riparian_user_matrix.csv", 
                paste0("user_appropriative_output_", projectName, ".csv"),
                paste0("user_riparian_output_", projectName, ".csv")) |>
    paste0(outPath, "/", ... = _) |>
    normalizePath(mustWork = FALSE)
  
  
  # Check if any files are missing
  missingFiles <- which(!file.exists(outFiles))
  
  
  if (length(missingFiles) > 0) {
    
    # Include the model run outputs in the console 
    cat("\n\nModel Output Message(s):\n\n")
    print(dwratRes)
    
    
    # Save 'dwratRes' to a file too
    writeOutput(dwratRes, "W3_LSPC_Watershed/DWRAT_Output_Messages.txt")
    
    
    paste0("Missing DWRAT Output File", 
           if_else(length(missingFiles) > 1, "s", ""), "\n\n",
           "The Paradigm DWRAT model run did not generate all of the expected ",
           "files (missing ", vec2QuotedStr(outFiles[missingFiles]),
           "). Please investigate the model's output messages (included ",
           "above and in a file).\n\n", 
           "(This error occurred for \"", scriptPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



combine_dwrat_outputs <- function (writePath, numFlowPaths) {
  
  # This function is called for watersheds that have multiple flow paths
  # DWRAT was run more than once, and there are multiple sets of outputs
  
  # Many of these files can be combined into a single table for ease of use
  # That is the objective of this function
  
  
  # Notify the user about the intended operation
  cat("\n\n")
  cat("Combining DWRAT output files!")
  cat("\n\n")
  
  
  # This tibble contains a set of DWRAT outputs that will be combined
  targetFiles <- c("_preferred_output.csv",           "^_preferred_output\\.csv$",
                   "appropriative_demand.csv",        "^appropriative_demand\\.csv$",
                   "basin_appropriative_output.csv",  "^basin_appropriative_output_.+\\.csv$",
                   "basin_riparian_output.csv",       "^basin_riparian_output_.+\\.csv$", 
                   "flows.csv",                       "^flows\\.csv$",
                   "riparian_demand.csv",             "^riparian_demand\\.csv$", 
                   "user_appropriative_output.csv",   "^user_appropriative_output_.+\\.csv$", 
                   "user_riparian_output.csv",        "^user_riparian_output_.+\\.csv$") |>
    matrix(ncol = 2, byrow = TRUE) |>
    data.frame() |> set_names(c("FILENAME", "REGEX"))
  
  # One column shows the planned filename, and the second column contains a 
  # regular expression to help locate each file within the separate output folders
  
  
  # Iterate through each file and try to combine them into one
  for (i in 1:nrow(targetFiles)) {
    
    # For this output file, 
    # get a list of all DWRAT output files that match the regular expression
    componentFiles <- list.files(writePath, pattern = targetFiles$REGEX[i],
                                 recursive = TRUE, full.names = TRUE)
    
    
    # The number of files found should match 'numFlowPaths'
    error_if(length(componentFiles) != numFlowPaths,
             paste0("Issue Gathering DWRAT Output Files\n\n",
                    "The script attempted to gather files from each of the ",
                    numFlowPaths, " DWRAT runs and combine them. But, while ",
                    "attempting to locate the DWRAT outputs related to \"",
                    targetFiles$FILENAME[i], "\", ", length(componentFiles),
                    " match(es) were found instead. Please investigate.\n\n",
                    "(This error occurred in \"", writePath, "\")"))
    
    
    # Read in each file iteratively and combine them
    for (j in 1:length(componentFiles)) {
      
      # Read in one of the individual DWRAT runs' version of this file
      tempRead <- componentFiles[j] |> getFile()
      
      
      # Start binding these results together into one variable ('combinedDF')
      if (j == 1) {
        
        combinedDF <- tempRead
        
      } else {
        
        combinedDF <- bind_rows(combinedDF, tempRead)
        
      }
      
    } # End of 'j' loop through component files
    
    
    # Write 'combinedDF' to a single file in the root location of 'writePath'
    combinedDF |>
      writeOutput(paste0(writePath, "/", targetFiles$FILENAME[i]))
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

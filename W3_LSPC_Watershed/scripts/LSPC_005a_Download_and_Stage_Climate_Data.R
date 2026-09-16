# Run Python scripts to download weather data for each watershed

# This script prepares a temporary Python script that has key information
# (path to Weather Control file and NLDAS Earth Data credentials)

# Then, using Anaconda, this script executes its Python counterpart (LSPC_005b),
# which imports values from the temporary script and executes other Python scripts 
# to download and process weather data for each watershed

# Those scripts generate manual review spreadsheets as well

# Note: The temporary Python script should never be committed
#       The 005b script should have a line of code that deletes it too


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
  cat("Starting 'LSPC_005a_Download_and_Stage_Climate_Data.R'!\n")
  
  
  # Start by preparing a temporary file
  cat("[1/2]\tGenerating temporary Python script...\n")
  
  
  # 'temp.py' will be written the workflow's "scripts" folder
  # It should contain information on the location of the weather control file
  # and Earth Data login credentials (if provided)
  generate_temp_LSPC_script()
  
  
  cat("\tDone!\n\n")
  
  
  # Call the 005b Python script to obtain weather data
  cat("[2/2]\tInitiating climate download and processing scripts...\n")
  
  
  # Get a path to Anaconda's "activate.bat" script
  batPath <- detectAnacondaBat()
  
  
  # Get the path to the Python script next
  scriptPath <- "W3_LSPC_Watershed/scripts/LSPC_005b_Download_and_Stage_Climate_Data.py" |>
    normalizePath(mustWork = FALSE)
  
  
  # Double-check that the path exists
  if (!file.exists(scriptPath)) {
    
    paste0("LSPC Climate Script Not Found\n\n", 
           "The LSPC workflow's \"scripts\" folder should contain a key script ",
           "called \"", scriptPath, "\". However, it could not be found. Please ",
           "investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Prepare a batch file to execute the Python script
  tempBat <- "temp.bat"
  
  
  c("cd W3_LSPC_Watershed",
    paste0(batPath, " && ",
           "conda activate lspc-climate-processing-restructure && ",
           "python ", shQuote(scriptPath))) |>
    writeOutput(tempBat, writeFunction = "write_lines", quietly = TRUE)
  
  
  climateRes <- system(tempBat, intern = TRUE)
  
  
  # Remove the temporary batch file
  unlink(tempBat)
  
  
  # To Do: Check for outputs and errors
  
  
  print(climateRes)
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_005a_Download_and_Stage_Climate_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



generate_temp_LSPC_script <- function () {
  
  # Generate a temporary script for the LSPC climate process
  
  # This file will contain the path to the weather control file
  
  # Similarly, if a user has NASA Earth Data login credentials, 
  # they will be stored as environment variables for use in the NLDAS download script
  
  
  # First check if the user has provided Earth Data credentials in a file
  nldasLogin <- get_from_lspc_master_control("EARTHDATA_LOGIN_CREDENTIALS")
  
  
  # If the control file field contains a file path, read it in
  if (!is.na(nldasLogin)) {
    nldasLogin <- getFile(nldasLogin)
  }
  
  
  # To Do: Validate login credentials file
  # (Adjust `validateLogin` in the RRW CIMIS script for that)
  
  
  # Next, prepare the script contents
  
  # It will be three lines that define three different variables
  # ('master_control_file', 'username', and 'password')
  
  # Start with a vector containing the planned Python code
  pyVec <- c(
    # Path to LSPC Weather Control File
    # (Absolute path that uses forward slashes and quotes)
    paste0("master_control_file = '", 
           lspc_weather_control_path() |> 
             normalizePath(mustWork = TRUE, winslash = "/"), "'"),
    
    # Earth Data Username
    # (Either an empty string or the first line of 'nldasLogin')
    paste0("username = '",
           if_else(is.na(nldasLogin[1]), "", nldasLogin[1]),
           "'"),
    
    # Earth Data Password
    # (Either an empty string or the second line of 'nldasLogin')
    paste0("password = '",
           if_else(is.na(nldasLogin[2]), "", nldasLogin[2]),
           "'"))
  
  
  # Prepare the output path of 'pyVec' next
  tempPath <- "W3_LSPC_Watershed/scripts/temp.py"
  
  
  # Write 'pyVec' to 'tempPath'
  pyVec |>
    writeOutput(tempPath, writeFunction = "write_lines")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



checkForErrors <- function (dirPath, scriptPath, climateRes) {
  
  # Confirm that the LSPC Part 1 climate script ran successfully
  
  
  # Look for error messages in the console output of 'climateRes'
  if (any(grepl("Error", climateRes, ignore.case = TRUE))) {
    
    # Include the model run outputs in the console 
    cat("\n\nModel Output Message(s):\n\n")
    print(climateRes)
    
    
    # Save 'climateRes' to a file too
    writeOutput(climateRes, "W3_LSPC_Watershed/data/Climate_Download_Output_Messages.txt")
    
    
    paste0("Climate Download and Staging Error\n\n",
           "An error was encountered while running the LSPC Python script. Please ",
           "investigate the model's output messages (included above and in a file).\n\n", 
           "(This error occurred for \"", scriptPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Confirm that all required outputs were generated
  # outFiles <- c("_preferred_output_Output.csv",
  #               "basin_appropriative_output_Output.csv",
  #               "basin_riparian_output_Output.csv",
  #               "user_appropriative_output_Output.csv",
  #               "user_riparian_output_Output.csv",
  #               "URR_Connected/_preferred_output.csv",
  #               "LRR_Connected/_LRR_Connected.log",
  #               "LRR_Connected/appropriative_demand.csv",
  #               "LRR_Connected/appropriative_user_connectivity_matrix.csv",
  #               "LRR_Connected/appropriative_user_matrix.csv",
  #               "LRR_Connected/basin_appropriative_output_LRR_Connected.csv",
  #               "LRR_Connected/basin_connectivity_matrix.csv",
  #               "LRR_Connected/basin_riparian_output_LRR_Connected.csv",
  #               "LRR_Connected/flows.csv",
  #               "LRR_Connected/riparian_demand.csv",
  #               "LRR_Connected/riparian_user_connectivity_matrix.csv",
  #               "LRR_Connected/riparian_user_matrix.csv",
  #               "LRR_Connected/user_appropriative_output_LRR_Connected.csv",
  #               "LRR_Connected/user_riparian_output_LRR_Connected.csv",
  #               "URR_Connected/_preferred_output.csv",
  #               "URR_Connected/_URR_Connected.log",
  #               "URR_Connected/appropriative_demand.csv",
  #               "URR_Connected/appropriative_user_connectivity_matrix.csv",
  #               "URR_Connected/appropriative_user_matrix.csv",
  #               "URR_Connected/basin_appropriative_output_URR_Connected.csv",
  #               "URR_Connected/basin_connectivity_matrix.csv",
  #               "URR_Connected/basin_riparian_output_URR_Connected.csv",
  #               "URR_Connected/flows.csv",
  #               "URR_Connected/riparian_demand.csv",
  #               "URR_Connected/riparian_user_connectivity_matrix.csv",
  #               "URR_Connected/riparian_user_matrix.csv",
  #               "URR_Connected/user_appropriative_output_URR_Connected.csv",
  #               "URR_Connected/user_riparian_output_URR_Connected.csv") |>
  #   paste0(dirPath, "/DWRAT/Output/", ... = _) |>
  #   normalizePath(mustWork = FALSE)
  
  # 
  # # Check if any files are missing
  # missingFiles <- which(!file.exists(outFiles))
  # 
  # 
  # if (length(missingFiles) > 0) {
  #   
  #   # Include the model run outputs in the console 
  #   cat("\n\nModel Output Message(s):\n\n")
  #   print(dwratRes)
  #   
  #   
  #   # Save 'dwratRes' to a file too
  #   writeOutput(dwratRes, "W2_Russian_River/Output/DWRAT_Output_Messages.txt")
  #   
  #   
  #   paste0("Missing DWRAT Output File", 
  #          if_else(length(missingFiles) > 1, "s", ""), "\n\n",
  #          "The Paradigm DWRAT model run did not generate all of the expected ",
  #          "files (missing ", vec2QuotedStr(outFiles[missingFiles]),
  #          "). Please investigate the model's output messages (included ",
  #          "above and in a file).\n\n", 
  #          "(This error occurred for \"", scriptPath, "\")") |>
  #     errWrap() |>
  #     stop()
  #   
  # }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

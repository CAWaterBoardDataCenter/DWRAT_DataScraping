# Run the Paradigm Connected DWRAT
# The outputs will be written to the hydrology folder

#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_019_Run_DWRAT.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that the model hydrology folder exists and get its path
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Next, get a path to Anaconda's "activate.bat" script
  batPath <- detectAnacondaBat()
  
  
  # Get Paradigm DWRAT's Russian River Connected DWRAT script
  scriptPath <- "../Paradigm_DWRAT/RR_Connected.py" |>
    normalizePath(mustWork = FALSE)
  
  
  # Double-check that the path exists
  if (!file.exists(scriptPath)) {
    
    paste0("DWRAT Script Not Found\n\n", 
           "In DWRAT_DataScraping, the Paradigm DWRAT folder is adjacent to ", 
           "the Supply folder. It should contain a key script called \"",
           "RR_Connected.py\". However, it could not be found. Please ",
           "investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Notify the user of the imminent model run
  cat("[1/1]\tStarting up model...\n")
  
  
  # Run Paradigm DWRAT
  dwratRes <- system(paste0(batPath, " && ",
                            "conda activate paradigm-dwrat && ",
                            "python ", shQuote(scriptPath)), 
                     intern = TRUE)
  
  
  # Check for errors
  checkForErrors(dirPath, scriptPath, dwratRes)
  
  
  # Output a completion message
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_019_Run_DWRAT.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



checkForErrors <- function (dirPath, scriptPath, dwratRes) {
  
  # Confirm that DWRAT ran successfully
  
  
  # Look for error messages in the console output of 'dwratRes'
  if (any(grepl("Error", dwratRes, ignore.case = TRUE))) {
    
    # Include the model run outputs in the console 
    cat("\n\nModel Output Message(s):\n\n")
    print(dwratRes)
    
    
    # Save 'dwratRes' to a file too
    writeOutput(dwratRes, "ProcessedData/DWRAT_Output_Messages.txt")
    
    
    paste0("Paradigm DWRAT Error\n\n",
           "An error was encountered while running DWRAT. Please investigate ",
           "the model's output messages (included above and in a file).\n\n", 
           "(This error occurred for \"", scriptPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Confirm that all required outputs were generated
  outFiles <- c("_preferred_output_Output.csv",
                "basin_appropriative_output_Output.csv",
                "basin_riparian_output_Output.csv",
                "user_appropriative_output_Output.csv",
                "user_riparian_output_Output.csv",
                "URR_Connected/_preferred_output.csv",
                "LRR_Connected/_LRR_Connected.log",
                "LRR_Connected/appropriative_demand.csv",
                "LRR_Connected/appropriative_user_connectivity_matrix.csv",
                "LRR_Connected/appropriative_user_matrix.csv",
                "LRR_Connected/basin_appropriative_output_LRR_Connected.csv",
                "LRR_Connected/basin_connectivity_matrix.csv",
                "LRR_Connected/basin_riparian_output_LRR_Connected.csv",
                "LRR_Connected/flows.csv",
                "LRR_Connected/riparian_demand.csv",
                "LRR_Connected/riparian_user_connectivity_matrix.csv",
                "LRR_Connected/riparian_user_matrix.csv",
                "LRR_Connected/user_appropriative_output_LRR_Connected.csv",
                "LRR_Connected/user_riparian_output_LRR_Connected.csv",
                "URR_Connected/_preferred_output.csv",
                "URR_Connected/_URR_Connected.log",
                "URR_Connected/appropriative_demand.csv",
                "URR_Connected/appropriative_user_connectivity_matrix.csv",
                "URR_Connected/appropriative_user_matrix.csv",
                "URR_Connected/basin_appropriative_output_URR_Connected.csv",
                "URR_Connected/basin_connectivity_matrix.csv",
                "URR_Connected/basin_riparian_output_URR_Connected.csv",
                "URR_Connected/flows.csv",
                "URR_Connected/riparian_demand.csv",
                "URR_Connected/riparian_user_connectivity_matrix.csv",
                "URR_Connected/riparian_user_matrix.csv",
                "URR_Connected/user_appropriative_output_URR_Connected.csv",
                "URR_Connected/user_riparian_output_URR_Connected.csv") |>
    paste0(dirPath, "/DWRAT/Output/", ... = _) |>
    normalizePath(mustWork = FALSE)
  
  
  # Check if any files are missing
  missingFiles <- which(!file.exists(outFiles))
  
  
  if (length(missingFiles) > 0) {
    
    # Include the model run outputs in the console 
    cat("\n\nModel Output Message(s):\n\n")
    print(dwratRes)
    
    
    # Save 'dwratRes' to a file too
    writeOutput(dwratRes, "ProcessedData/DWRAT_Output_Messages.txt")
    
    
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



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

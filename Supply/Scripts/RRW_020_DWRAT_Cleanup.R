# After a Paradigm DWRAT run has completed successfully, 
# perform final post-processing steps here

# 

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
  cat("Starting 'RRW_020_DWRAT_Cleanup.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that the model hydrology folder exists and get its path
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # After that, get the path to Anaconda's "activate.bat" script
  batPath <- detectAnacondaBat()
  
  
  # Output a message about exporting the Anaconda environment to a file
  cat("[1/1]\tExporting Anaconda Environment to YAML File...\n")
  
  
  # Use "conda export" to write an "environment.yml" file in the hydrology
  # output folder
  ymlRes <- system(paste0(batPath, " && ",
                          "conda export -n paradigm-dwrat -f ", 
                          paste0(dirPath, "/environment.yml") |> 
                            normalizePath(mustWork = FALSE) |> shQuote()), 
                   intern = TRUE)
  
  
  # 'ymlRes' should be empty
  # Otherwise, an error may have occurred
  if (length(ymlRes) > 0) {
    
    # Include the output message in the console 
    cat("\n\nOutput Message(s):\n\n")
    print(ymlRes)
    
    
    paste0("Anaconda Export Error\n\n",
           "An error was encountered while exporting the \"paradigm-dwrat\" ",
           "environment to a file. Please investigate the model's output ",
           "messages above.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The task is complete
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_020_DWRAT_Cleanup.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())



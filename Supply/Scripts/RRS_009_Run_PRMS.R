# Run the PRMS model
# Use the copy of the model files in the "ProcessedData" folder 


#### Setup ####

# Clear the environment
#remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRS_009_Run_PRMS.R'!\n")
  
  
  # Confirm that the "RR_PRMS" folder was copied to "ProcessedData"
  prmsPath <- checkForPRMS()
  
  
  # Get the path to the batch file stored in the "windows" folder
  batPath <- paste0(prmsPath, "/windows/run.bat") |>
    normalizePath(mustWork = TRUE)
  
  
  # Notify the user of the impending model run
  cat("[1/1]\tStarting up model...\n")
  
  
  # Get the current time (for time tracking purposes)
  startTime <- Sys.time()
  
  
  # Run the batch file
  # All output in the Command Prompt window will be saved to 'modelOutput'
  modelOutput <- system(batPath, intern = TRUE)
  
  
  # Get the current time (for time tracking purposes)
  endTime <- Sys.time()
  
  
  # Check for errors
  # There should be several output files
  checkForOutputFiles(prmsPath, modelOutput)
  
  
  # Output a completion message
  cat("\tDone!\n\n")
  
  
  # Save the model output to a file
  modelOutput |>
    writeOutput(paste0(prmsPath, "/PRMS/output/PRMS_Console_Output.txt"),
                "write_lines", quietly = TRUE)
  
  
  # After that, tell the user how long the model run took
  cat(paste0("\n\nThe model ran in ", 
            difftime(endTime, startTime, units = "mins") |> round(),
            " minutes!\n"))
  
  
  # Output a completion message
  cat(col_green("\n'RRS_009_Run_PRMS.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



checkForPRMS <- function () {
  
  # In a prior script, PRMS model files were copied to the "ProcessedData" folder
  # Verify that it exists
  
  
  # The expected path of the "RR_PRMS" folder
  prmsPath <- "ProcessedData/RR_PRMS" |> normalizePath(mustWork = FALSE)
  
  
  # Make sure that that folder exists 
  if (!dir.exists(prmsPath)) {
    
    stop(paste0("PRMS Folder Not Found\n\n",
                "A copy of the PRMS model files should have been added ",
                "to the \"ProcessedData\" folder in an earlier script. ",
                "However, it was not found. ",
                "Please run the previous scripts before running this one.\n\n",
                "The expected directory was \"", prmsPath, "\"") |>
           errWrap())
    
  }
  
  
  # Also confirm that the control file and batch file for PRMS exist
  controlPath <- paste0(prmsPath, "/windows/prms_rr.control") |> 
    normalizePath(mustWork = FALSE)
  
  
  if (!file.exists(controlPath)) {
    
    stop(paste0("Missing PRMS Control File\n\n",
                "When the PRMS folder was copied into the \"ProcessedData\" ", 
                "folder, a control file was present in the \"windows\" folder. ",
                "However, it cannot be found now. Please investigate.\n\n",
                "(This error occurred for \"", controlPath, "\")") |>
           errWrap())
    
  }
  
  
  batPath <- paste0(prmsPath, "/windows/run.bat") |>
    normalizePath(mustWork = FALSE)
  
  
  if (!file.exists(batPath)) {
    
    stop(paste0("Missing PRMS Batch File\n\n",
                "In an earlier script, a batch file was added to the model ", 
                "files that are stored in the  \"ProcessedData\" folder. ", 
                "However, it cannot be found now. Please investigate.\n\n",
                "(This error occurred for \"", batPath, "\")") |>
           errWrap())
    
  }
  
  
  # Return 'prmsPath'
  return(prmsPath)
  
}



checkForOutputFiles <- function (prmsPath, modelOutput) {
  
  # Double-check that the model ran successfully
  
  # There should be several key files in the "output" folder
  
  outFiles <- c("gsflow.csv", 
                "rr_budget.out2",
                "RR_PRMS_Output_sub_cfs.csv",
                "RR_PRMS_Output_sub_inq.csv")
  
  
  # Check if any files are missing
  missingFiles <- which(!file.exists(outFiles |>
                                       paste0(prmsPath, "/PRMS/output/", 
                                              ... = _) |>
                                       normalizePath(mustWork = FALSE)))
  
  
  if (length(missingFiles) > 0) {
    
    # Include the model run outputs in the console
    cat("\n\nModel Output Message(s):\n\n")
    print(modelOutput)
    
    stop(paste0("Missing PRMS Output File", 
                if_else(length(missingFiles) > 1, "s", ""), "\n\n",
                "The PRMS model run did not generate all of the expected ",
                "files (missing ", vec2QuotedStr(outFiles[missingFiles]),
                "). Please investigate the model's output messages (included ",
                "above).\n\n",
                "(This error occurred when running \"", prmsPath, "\")") |>
           errWrap())
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}


#### Script Execution ####

mainProcedure()


# Clean up
#remove(list = ls())

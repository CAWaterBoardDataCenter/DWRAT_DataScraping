# Create a batch file for the Russian River Workflow in the "Workflows" folder

# This file updates the repository using git and then runs through the 
# entire RR Workflow procedure using "RRW_000A_Run_RR_Process_Today.R"

# All outputs are saved in a log file

# A final script copies that log file to the main hydrology output folder

#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'HLP_006_Generate_RR_Workflow_Bat.R'!\n")
  
  
  cat("[1/1]\tGenerating batch file...\n")
  
  
  # This batch file requires a path to "Rscript.exe"
  rPath <- detectRScriptExe()
  
  
  # Customize the call to R with these options
  # https://rstudio.github.io/r-manuals/r-intro/Invoking-R.html
  rOpts <- " --no-save --no-environ --no-init-file --no-restore --no-Rconsole "
  
  
  # Define the output location for a log
  # By default, store this file in the DWRAT_DataScraping "Workflows" folder
  logPath <- "..\\Workflows\\RR_Workflow.log"
  
  
  # Write several commands to this bat file:
  
  c(# Switch the working directory to the "Supply" folder of DWRAT_DataScraping
    # (Note: This is an absolute path)
    paste0("cd ", getwd() |> normalizePath() |> shQuote()),
    
    paste0("(",
           
           # Run "HLP_005_Git_Update_Main.R" to ensure the scripts are up-to-date
           shQuote(rPath), rOpts, shQuote("Scripts\\HLP_005_Git_Update_Main.R"),
           
           " && ",
           
           # Execute "RRW_000A_Run_RR_Process_Today.R" to run the 
           # entire Russian River process
           shQuote(rPath), rOpts, shQuote("Scripts\\RRW_000A_Run_RR_Process_Today.R"),
           
           # All output is stored in a new file established at 'logPath'
           ") > ", shQuote(logPath), 
           
           " && ",
           
           # Finally, copy the log file to the model archive folder
           shQuote(rPath), rOpts, shQuote("Scripts\\HLP_007_Archive_Log_File.R")),
    
    # Call "exit" to ensure that the batch file closes properly
    "exit") |>
    writeOutput("../Workflows/RR_Workflow.bat", writeFunction = "write_lines")
  
  
  # If there are no issues, conclude the script
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'HLP_006_Generate_RR_Workflow_Bat.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

# Create batch files for the LSPC Workflow in the "Workflows" folder

# The first batch file updates the repository using git and then runs
# through the "Part 1" portion of the workflow using "MSTR_v1_LSPC_AUTO.R"

# The second batch file runs through the "Part 2" portion of the workflow
# using "MSTR_v1_LSPC_Part_2.R"

# All outputs are saved in log files

# A final script copies those log files to the main hydrology archive folder

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
  cat("Starting 'HLP_004_Generate_LSPC_Workflow_Bat.R'!\n")
  
  
  cat("[1/1]\tGenerating batch file...\n")
  
  
  # This batch file requires a path to "Rscript.exe"
  rPath <- detectRScriptExe()
  
  
  # Customize the call to R with these options
  # https://rstudio.github.io/r-manuals/r-intro/Invoking-R.html
  rOpts <- " --no-save --no-restore --no-Rconsole "
  
  
  # Define the output location for log files
  # By default, store these files in the LSPC "Workflow_Bin" folder
  logPath <- paste0("W3_LSPC_Watershed\\Workflow_Bin\\LSPC_Workflow_", 1:2, ".log")
  
  
  # Next, write several commands to these batch files
  
  
  # Start with the Part 1 batch file:
  
  c(# Switch the working directory to the root directory of DWRAT_DataScraping
    # (Note: This is an absolute path)
    paste0("cd ", getwd() |> normalizePath() |> shQuote()),
    
    paste0("(",
           
           # Run "HLP_005_Git_Update_Main.R" from the Russian River workflow 
           # to ensure the scripts are up-to-date
           shQuote(rPath), rOpts, 
           shQuote("W2_Russian_River\\Scripts\\HLP_005_Git_Update_Main.R"),
           
           " && ",
           
           # Execute "MSTR_v1_LSPC_AUTO.R" to run the Part 1 LSPC process
           shQuote(rPath), rOpts, 
           shQuote("W3_LSPC_Watershed\\scripts\\MSTR_v1_LSPC_AUTO.R"),
           
           # All output is stored in a new file established by 'logPath'
           ") > ", shQuote(logPath[1]), " 2>&1 "),
    
    # Copy the log file to the model archive folder
    paste0(shQuote(rPath), rOpts, 
           shQuote("W3_LSPC_Watershed\\scripts\\HLP_005_Archive_Log_Files.R")),
    
    # Call "exit" to ensure that the batch file closes properly
    "exit") |>
    writeOutput("W3_LSPC_Watershed/Workflow_Bin/LSPC_Workflow_Part_1.bat", 
                writeFunction = "write_lines")
  
  
  # After that is the Part 2 batch file:
  
  c(# Switch the working directory to the root directory of DWRAT_DataScraping
    # (Note: This is an absolute path)
    paste0("cd ", getwd() |> normalizePath() |> shQuote()),
    
    paste0("(",
           
           # Execute "MSTR_v1_LSPC_Part_2.R" to run the Part 2 LSPC process
           shQuote(rPath), rOpts, 
           shQuote("W3_LSPC_Watershed\\scripts\\MSTR_v1_LSPC_Part_2.R"),
           
           # All output is stored in a new file established in 'logPath'
           ") > ", shQuote(logPath[2]), " 2>&1 "),
    
    # Copy the log file to the model archive folder
    paste0(shQuote(rPath), rOpts, 
           shQuote("W3_LSPC_Watershed\\scripts\\HLP_005_Archive_Log_Files.R")),
    
    # Call "exit" to ensure that the batch file closes properly
    "exit") |>
    writeOutput("W3_LSPC_Watershed/Workflow_Bin/LSPC_Workflow_Part_2.bat", 
                writeFunction = "write_lines")
  
  
  # If there are no issues, conclude the script
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'HLP_004_Generate_LSPC_Workflow_Bat.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

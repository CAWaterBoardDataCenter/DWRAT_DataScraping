# Make sure the "main" branch of the DWRAT_DataScraping repository is active 
# and the code is up-to-date

# This script uses a temporary batch file to perform git-related actions

# Please note that "git" must be installed for this script to function
# (Otherwise, Command Prompt will not support these commands)

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
  cat("Starting 'HLP_005_Git_Update_Main.R'!\n")
  
  
  cat("[1/1]\tRunning git commands to setup the repository...\n")
  
  
  # Create a temporary batch file to execute via Command Prompt
  tempBat <- "temp-git.bat"
  
  
  # Write several git commands to this bat file:
  
  c(#        (*) Label the "DWRAT_DataScraping" folder as a safe directory
    #            (to avoid dubious ownership errors)
    paste0("git config --global --add safe.directory ",
           normalizePath("..") |> shQuote(), " && ",
           
           # (*) Switch to the main branch
           "git switch main", " && ",    
           
           # (*) Pull the latest changes in "main"
           "git pull origin main"),
    
    # (*) Call "exit" to ensure that git closes properly
    "exit") |>
    writeOutput(tempBat, writeFunction = "write_lines", quietly = TRUE)
  
  
  # Use `system` to execute the batch file
  gitRes <- system(tempBat, intern = TRUE)
  
  
  # Delete the batch file after these operations are complete
  unlink(tempBat)
  
  
  # Finally, check 'gitRes' for errors
  cat("\n\n")
  print(gitRes)
  cat("\n\n")
  
  
  # If an error occurred, its message will appear in 'gitRes'
  if (any(grepl("(fatal)|(error)|(fail)|(abort)", gitRes, ignore.case = TRUE)) ||
      (!is.null(attr(gitRes, "status")) && attr(gitRes, "status") == 1)) {
    
    # Output an error message for the user
    paste0("Could Not Execute Git Commands\n\n",
           "The procedure failed. Please investigate the messages ",
           "shown above.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there are no issues, conclude the script
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'HLP_005_Git_Update_Main.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

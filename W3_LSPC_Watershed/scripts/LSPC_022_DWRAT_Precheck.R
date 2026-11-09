# Look for the Paradigm DWRAT Anaconda installation


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
  cat("Starting 'LSPC_022_DWRAT_Precheck.R'!\n")
  
  
  # Import functions from another script
  c("dwrat_precheck", "installDWRAT") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_018_DWRAT_Precheck.R", .))
  
  
  # The Russian River workflow has a script procedure that will be replicated here
  dwrat_precheck()
  
  
  cat(col_green("\n'LSPC_022_DWRAT_Precheck.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

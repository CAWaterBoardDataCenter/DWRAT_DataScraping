# This script runs the entire RR Workflow process

# It enables further automation by automatically choosing the start and end dates

# The end date is four days prior to today
# The start date is the beginning of the previous water year


#### Setup ####

# Clear the environment first
base::remove(list = ls())


# Check the working directory
if (!grepl("[/\\\\]DWRAT_DataScraping$", getwd())) {
  stop("Please use \"DWRAT_DataScraping.Rproj\"")
}


# Import packages next

# Install and activate 'renv' if it's not already setup
source("Shared_Scripts/Project_Setup.R")


# Load in packages
source("W2_Russian_River/Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")


#### Procedure ####

# The end date for the model's data scraping procedure will be 
# four days prior to today
plannedEnd <- Sys.Date() - 4


# Get the current water year based on this end date
if (month(plannedEnd) > 9) {
  
  currentWY <- year(plannedEnd) + 1
  
} else {
  
  currentWY <- year(plannedEnd)
  
}


# Get the prior water year
priorWY <- currentWY - 1


# The start date for the model's data scraping procedure will be
# the first day of the previous water year
plannedStart <- paste0(priorWY - 1, "-10-01")


# Read in the control file
controlPath <- "W2_Russian_River/Scripts/CTR_001_Set_Start_and_End_Dates.R"


controlScript <- getFile(controlPath, fileType = "OTHER")


# Replace the value for 'startDate' with 'plannedStart'
indexLoc <- grep("^startDate <-", controlScript)[1]


if (length(indexLoc) != 1) {
  
  stop(paste0("The format of \"", controlPath, "\" has changed. 'startDate' ",
              "could not be located and redefined. Please adjust this script.") |>
         errWrap())
  
}


controlScript[indexLoc] <- paste0("startDate <- \"", plannedStart, "\"")


# Do the same for 'endDate' and 'plannedEnd'
indexLoc <- grep("^endDate <-", controlScript)[1]


if (length(indexLoc) != 1) {
  
  stop(paste0("The format of \"", controlPath, "\" has changed. 'endDate' ",
              "could not be located and redefined. Please adjust this script.") |>
         errWrap())
  
}


controlScript[indexLoc] <- paste0("endDate <- \"", plannedEnd, "\"")


# Save these updates to the control file
writeOutput(controlScript, controlPath, 
            writeFunction = "write_lines", quietly = TRUE)


# Clear the environment
base::remove(list = ls())


# Finally, run the master script and begin the RR Workflow process
source("W2_Russian_River/Scripts/RRW_000_Master_Script.R")

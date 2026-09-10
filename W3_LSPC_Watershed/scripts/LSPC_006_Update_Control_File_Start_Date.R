# Update the start date column in "LSPC_Weather_Control.xlsx" 

# Right now, the file contains the user-specified start date and end date

# Watershed-specific data will be downloaded for the entire modeling period
# (From the historic datasets' start date to the user-specified end date)

# Moreover, when this climate data is processed into weather files, 
# they will be structured for the entire period as well


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
  cat("Starting 'LSPC_006_Update_Control_File_Start_Date.R'!\n")
  
  
  # Import another script's function
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_001_Update_Control_File.R", "updateSpreadsheet")
  
  
  # First, check the raw weather files to determine an appropriate start date
  cat("\n[1/3]\tDetermining model start date...\n")
  
  
  modelStart <- set_model_start_date()
  
  
  cat("\tDone!\n\n")
  
  
  # Check for "LSPC_Weather_Control.xlsx" and load it into the environment
  cat("[2/3]\tReading in LSPC weather control file...\n")
  
  
  controlDF <- read_lspc_weather_control()
  
  
  # To Do: Validation function for weather control file 
  
  controlDF <- controlDF |>
    mutate(start_date = as.Date(start_date))
  
  
  cat("\tDone!\n\n")
  
  
  # Next, apply the user's specified start and end dates to the control file
  cat("[3/3]\tSetting new data scraping bound...\n")
  
  
  # Set the starting date to 'modelStart'
  for (i in 1:nrow(controlDF)) {
    
    controlDF$start_date[i] <- modelStart
    
  }
  
  
  # Write 'controlDF' back to 'controlPath' with these updates
  controlDF |> 
    updateSpreadsheet(lspc_weather_control_path(),
                      lspc_weather_control_worksheet())
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_006_Update_Control_File_Start_Date.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



set_model_start_date <- function () {
  
  # Look into the raw weather files and determine an appropriate model start date
  
  # Ideally, it should be a date that all weather sources have data for
  
  
  weatherFolders <- list.dirs("W3_LSPC_Watershed/data/shared/raw", recursive = FALSE, full.names = TRUE)
  
  # To Do: Replace all references to the "data" folder with calls to the control files?
  
  
  # Exclude CIMIS from this check
  # (It lacks data before November 2003 but it should not restrict the procedure)
  weatherFolders <- weatherFolders |>
    str_subset("/cimis$", negate = TRUE)
  
  
  # Iterate through each weather source's files
  for (i in 1:length(weatherFolders)) {
    
    # Find the raw weather file with the earliest date and try to extract that value
    earliestFile <- list.files(weatherFolders[i]) |>
      sort() |> head(1)
    
    
    # To Do: Validation Error Message
    stopifnot(length(earliestFile) == 1)
    
    
    # Try to extract a date from 'earliestFile' (though its formatting can differ)
    if (grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T", earliestFile)) {
      
      fileDate <- earliestFile |>
        str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}(?=T)") |>
        as.Date(format = "%Y-%m-%d")
      
      # This regular expression looks for a date in YYYY-MM-DD that appears right before a "T"
      # (Datetime values appear in NLDAS filenames)
      
    } else if (grepl("_[0-9]{6}\\.", earliestFile)) {
      
      
      fileDate <- earliestFile |>
        str_extract("(?<=_)[0-9]{6}(?=\\.)") |>
        as_date(format = "%Y%m")
      
      # This regular expression looks for a date in YYYYMM format that appears
      # between an underscore and period
      # (Like in PRISM files)
      
    } else {
      
      paste0("Script Error\n\n",
             "The script does not have information on how to extract a date from ",
             "the file \"", earliestFile, "\". Please address this issue.") |>
        stop_script()
      
    }
    
    
    # In the first iteration, set 'selectedDate' equal to 'fileDate'
    if (i == 1) {
      
      selectedDate <- fileDate
      
    # In subsequent iterations, only update 'selectedDate' if 'fileDate' corresponds
    # to a later value (i.e., we want a value that all sources have data for)
    } else {
      
      if (selectedDate < fileDate) {
        selectedDate <- fileDate
      }
      
    }
    
  }
  
  
  # Return 'selectedDate'
  return(selectedDate)
  
}

#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

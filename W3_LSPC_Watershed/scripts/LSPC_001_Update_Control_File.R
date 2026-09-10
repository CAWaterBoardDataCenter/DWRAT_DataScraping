# Update "LSPC_Weather_Control.xlsx" with the user's specified web scraping dates

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
  cat("Starting 'LSPC_001_Update_Control_File.R'!\n")
  
  
  # Import the user's input start and end dates
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Check for "LSPC_Weather_Control.xlsx" and load it into the environment
  cat("\n[1/2]\tReading in LSPC weather control file...\n")
  
  
  controlDF <- read_lspc_weather_control()
  
  
  # To Do: Validation function for weather control file 
  
  controlDF <- controlDF |>
    mutate(start_date = as.Date(start_date),
           end_date = as.Date(end_date))
  
  
  cat("\tDone!\n\n")
  
  
  # Next, apply the user's specified start and end dates to the control file
  cat("[2/2]\tSetting data scraping bounds...\n")
  
  
  # Set the bounds to 'startDate' and 'endDate'
  # Also, by default, old data should be overwritten
  # So set the 'overwrite' column value to TRUE as well
  for (i in 1:nrow(controlDF)) {
    
    controlDF$start_date[i] <- startDate
    controlDF$end_date[i] <- endDate
    controlDF$overwrite[i] <- TRUE
    
  }
  
  
  # Write 'controlDF' back to 'controlPath' with these updates
  controlDF |> 
    updateSpreadsheet(lspc_weather_control_path(),
                      lspc_weather_control_worksheet())
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_001_Update_Control_File.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



updateSpreadsheet <- function (sheetDF, path, worksheet) {
  
  # Use `openxlsx2` functions to update a sheet in a spreadsheet
  # This method preserves formatting and other components of the spreadsheet
  
  
  # Read in the workbook object
  # Overwrite the control file's worksheet
  
  wb <- wb_load(path)
  
  
  wb <- wb_add_data(wb, sheet = worksheet, sheetDF)
  
  
  wb_save(wb, path)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

# In more recent versions of LSPC (around v6.33), there are new QA/QC checks
# Unfortunately, they can cause the model runs to incorrectly fail 

# This script implements adjustments to the weather files to prevent the 
# accidental errors

# Dummy records are added to the beginning and end of the LSPC weather files
# (Both air and precipitation files)


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
  cat("Starting 'LSPC_015_Add_Dummy_Weather_Entries.R'!\n")
  
  
  # Import functions from other scripts
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R",
                  "read_all_lspc_project_control")
  

  # Load in the LSPC weather control file too
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validation of control file
  
  
  # Use the imported function to get storage information for every watershed
  wsDir <- controlDF |>
    read_all_lspc_project_control(worksheet = "Storage")
  
  # To Do: Validation of storage worksheets
  
  
  # Use the "Storage" worksheets to get the paths to weather files
  cat("\n[1/2]\tGetting list of weather files...\n")
  
  # The finalized weather files are stored in the "curated" folders
  # Get the paths to that folder
  curatedDir <- wsDir |>
    map_chr(~ paste0("W3_LSPC_Watershed/",
                     filter(., scope == "project" & level == "root") |>
                       select(path) |> unlist(use.names = FALSE),
                     "/curated"))
  
  
  # For each watershed, obtain a list of weather files
  weatherFiles <- curatedDir |>
    map(~ list.files(., full.names = TRUE, recursive = TRUE, 
                     pattern = "\\.((air)|(pre))$"))
  
  
  cat("\tDone!\n\n")
  
  
  # After that, use 'controlDF' to choose dummy dates for the weather files
  cat("[2/3]\tChoosing dummy dates...\n")
  
  
  # Set the dummy start date to two days prior to the actual start date of the 
  # weather files
  dummyStart <- min(controlDF$start_date) - days(2)
  
  
  # Similarly, set the dummy end date to two days after the actual end date
  dummyEnd <- max(controlDF$end_date) + days(2)
  
  
  cat("\tDone!\n\n")
  
  
  # After that, iterate through each watershed and weather file
  # Incorporate these dummy entries into each file
  cat("[3/3]\tUpdating weather files...\n")
  
  
  # Begin by iterating through each watershed
  for (i in 1:length(weatherFiles)) {
    
    # Print the watershed's name to the console
    cat("\n\n")
    paste0("\t[", i, "/", length(weatherFiles), "]\t", controlDF$project_name[i]) |>
      cat()
    cat("\n\n")
    
    
    # Then, iterate through each of the watershed's weather files
    for (j in 1:length(weatherFiles[[i]])) {
      
      # Print the name of the weather file
      cat("\n\n")
      paste0("\t\t[", j, "/", length(weatherFiles[[i]]), "]\t", 
             weatherFiles[[i]][j] |> extract_filename()) |>
        cat()
      cat("\n\n")
      
      
      # Update the weather file with 'dummyStart' and 'dummyEnd'
      weatherFiles[[i]][j] |>
        add_dummy_entries(dummyStart, dummyEnd)
      
    }
    
  }
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_015_Add_Dummy_Weather_Entries.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



add_dummy_entries <- function (filePath, dummyStart, dummyEnd) {
  
  # Add dummy entries to the beginning and end of the weather file
  
  # The exact procedure will differ slightly depending on the type of file
  
  
  if (grepl("\\.air$", filePath)) {
    
    return(add_dummy_air_entries(filePath, dummyStart, dummyEnd))
    
  } else if (grepl("\\.pre$", filePath)) {
    
    return(add_dummy_pre_entries(filePath, dummyStart, dummyEnd))
    
  } else {
    
    paste0("Unknown File Type\n\n",
           "The script expected weather files to have the \".air\" and ",
           "\".pre\" extensions (all lowercase). However, an unrecognized ",
           "file appeared in the list of curated weather files.\n\n",
           "Please investigate \"", filePath, "\".") |>
      stop_script()
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



add_dummy_air_entries <- function (filePath, dummyStart, dummyEnd) {
  
  # Read in the air weather file
  # Include the dummy dates at the beginning and end of its dataset
  
  # There is one challenge related to this type of file
  # There is metadata at the beginning that must remain intact
  
  
  # Start by reading in the file
  airVec <- filePath |>
    getFile(fileType = "OTHER")
  
  # To Do: Validate the air file
  
  
  # Find the start of the actual evapotranspiration data
  headerEnd <- airVec |>
    find_matches("Date/time")
  
  
  # Get the first non-header row and extract its ID
  idVal <- airVec[headerEnd + 1] |>
    str_split("\t") |>
    unlist() |> head(1)
  
  
  # With this value, row entries can be prepared for 'dummyStart' and 'dummyEnd'
  # Seven column values will be separated by tabs
  newStartRow <- create_air_row(idVal, dummyStart)
  
  newEndRow <- create_air_row(idVal, dummyEnd)
  
  
  # The next step is to insert these dummy rows into 'airVec'
  
  
  # Add 'newStartRow' right after 'headerEnd'
  # Append 'newEndRow' to the very end of the vector
  airVec <- c(airVec[1:headerEnd],
              newStartRow,
              airVec[(headerEnd + 1):length(airVec)],
              newEndRow)
  
  
  # Write 'airVec' back to 'filePath'
  airVec |>
    writeOutput(filePath, writeFunction = "write_lines", quietly = TRUE, 
                sep = "\r\n")
  
  # Note: "\r\n" is the full expression for a new line marker in Windows
  
  # The LSPC executable file explicitly requires "\r\n" in all text-based files
  # Otherwise, the file will fail to parse (without any clear error message)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



create_air_row <- function (id, date, hour = 0, minute = 0, et = 0) {
  
  # Create a new row for an air weather file
  
  # The formatting will be:
  # [ID]\t[YEAR]\t[MONTH]\t[DAY]\t[HOUR]\t[MINUTE]\t[ET_VALUE]
  
  
  # Define a vector of values and collapse them together with tab spaces
  return(c(id, year(date), month(date), day(date), hour, minute, 
           sprintf("%.1f", et)) |>
           paste0(collapse = "\t"))
  
  # `sprintf` sets 'et' to always have one decimal place (even when it's 0)
  
}



add_dummy_pre_entries <- function (filePath, dummyStart, dummyEnd) {
  
  # Read in the pre weather file
  # Include the dummy dates at the beginning and end of its dataset
  
  
  # Start by reading in the file
  preVec <- filePath |>
    getFile(fileType = "OTHER")
  
  # To Do: Validate the pre file
  
  
  # Using 'dummyStart' and 'dummyEnd', 
  # create comma-separated entries (with fake datetimes and values)
  newStartRow <- create_pre_row(dummyStart)
  
  newEndRow <- create_pre_row(dummyEnd)
  
  
  # The next step is to insert these dummy rows into 'preVec'
  
  
  # Add 'newStartRow' right to the beginning and 'newEndRow' to the end of 'preVec'
  preVec <- c(newStartRow,
              preVec,
              newEndRow)
  
  
  # Write 'preVec' back to 'filePath'
  preVec |>
    writeOutput(filePath, writeFunction = "write_lines", quietly = TRUE, 
                sep = "\r\n")
  
  # Note: "\r\n" is the full expression for a new line marker in Windows
  
  # The LSPC executable file explicitly requires "\r\n" in all text-based files
  # Otherwise, the file will fail to parse (without any clear error message)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



create_pre_row <- function (date, hour = 0, minute = 0, second = 0, precip = 0) {
  
  # Create a new row for a precipitation weather file
  
  # The formatting will be:
  # [MONTH]/[DAY]/[YEAR] [HOUR]:[MINUTE]:[SECOND],[PRECIP_VALUE]
  
  
  # Define a vector of values and collapse them together with tab spaces
  return(paste0(format(date, "%m/%d/%Y"), " ", 
                sprintf(fmt = "%.2d", hour), ":", 
                sprintf(fmt = "%.2d", minute), ":", 
                sprintf(fmt = "%.2d", second), ",", 
                sprintf(fmt = "%.1f", precip)))
  
  # `sprintf` helps ensure that the time values are two digits each
  # For the precipitation value, it will always have one decimal place
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

# Download precipitation data from RAWS at various locations across each watershed

# Each watershed's project control file lists RAWS stations used in their
# weather files

# Some watersheds rely on the same RAWS stations too

# This script will compile a list of required RAWS stations and download their
# data to a shared "raw/gage/raws" folder


#### Setup ####

base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'LSPC_007c_Download_RAWS_Data.R'!\n")
  
  
  # Import functions from other scripts
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R",
                  "read_all_lspc_project_control")
  
  
  c("requestRAWS", "adjustScrapingBounds", "getDatasetBounds", "splitRequest") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_003_RAWS_HTTP_Scraper.R", .))
  
  
  # Read in the main weather control file for LSPC
  controlDF <- read_lspc_weather_control()
  
  
  # Read in all project control files after that
  wsDir <- read_all_lspc_project_control(controlDF, worksheet = "Storage")
  
  # To Do:
  # Validate the "Storage" worksheets
  
  
  # After that, import a lookup table that relates MesoWest Station IDs to RAWS IDs
  # (the watershed project control files use the former to identify RAWS stations)
  rawsLookup <- "W3_LSPC_Watershed/src/etl/fetch/gage/station_id_to_raws_id_mapping.csv" |>
    getFile()
  
  # To Do: Validate Mapping File
  
  
  # Finally, define the start and end dates for data scraping based on 'controlDF'
  startDate <- min(controlDF$start_date)
  
  endDate <- max(controlDF$end_date)
  
  # (Each watershed may have different date bounds specified)
  # (For that reason, use the broadest range possible for each station)
  
  
  # Start by creating a shared "RAWS" folder
  # (This does not exist in the original procedure of the climate scripts)
  cat("\n[1/2]\tChecking directories...\n")
  
  
  # The required path components actually redundantly in every watershed's spreadsheet
  # However, they should all ultimately result in the exact same path
  
  # To Do: A function for building shared paths (checks every file and confirms consistency)
  # build_lspc_shared_path(storageList, ...)
  
  
  # Use each watershed's project control file to build a path to the shared RAWS folder
  # Then, call `unique`, which should result in a single path
  rawFolder <- wsDir |>
    map_chr(~ paste0("W3_LSPC_Watershed/",
                     filter(., scope == "shared" & level == "root") |>
                       select(path) |> unlist(use.names = FALSE),
                     "/",
                     filter(., scope == "project" & level == "raw" & source == "raws") |>
                       select(path) |> unlist(use.names = FALSE))) |>
    unique()
  
  stopifnot(length(rawFolder) == 1)
  
  
  # Try to create the raw RAWS folder
  # Note: This type of folder did not exist in the initial design
  #       of the LSPC climate processing scripts
  try(dir_create(rawFolder), silent = TRUE)
  
  
  cat("\tDone!\n\n")
  
  
  # After that, create a single data frame with every relevant RAWS station
  # (across all watersheds)
  rawsDF <- controlDF |>
    gather_lspc_raws_ids(mergeDFs = TRUE)
  
  
  # Join 'rawsLookup' to this table
  # "RAWS_ID" contains the actual required station IDs for the procedure
  # (The MesoWest column must be renamed to match, though)
  rawsDF <- rawsDF |>
    left_join(rawsLookup |> rename(station_id = `MesoWest Station ID`),
              by = "station_id", relationship = "one-to-one")
  
  
  # Finally, begin downloading data for each grid cell
  cat(paste0("[2/2]\tGetting precipitation data for ", nrow(controlDF), 
             " watersheds (", nrow(rawsDF), " RAWS stations)...\n"))
  
  
  # Iterate through the rows of 'rawsDF'
  for (i in 1:nrow(rawsDF)) {
    
    # Each row corresponds to a single PRISM grid cell
    # (though multiple watersheds may rely on the same cell)
    
    
    paste0("\t[", i, "/", nrow(rawsDF), "]\t",
           "Downloading data for Station ", rawsDF$RAWS_ID[i], "...\n\n") |>
      cat()
    
    
    # Define the initial download path
    # At first, the file will be stored in the shared RAWS directory
    initialPath <- paste0(rawFolder, "/", 
                          rawsDF$station_id[i], "_", startDate, "_", endDate, ".csv")
    
    
    # Download RAWS data and save it to 'initialPath'
    resDF <- requestRAWS(rawsDF$RAWS_ID[i], startDate, endDate)
    
    
    # Wait a moment before proceeding
    Sys.sleep(runif(1, min = 1.05, max = 1.3))
    
    
    # Write 'resDF' to 'initialPath'
    resDF |>
      writeOutput(initialPath)
    
    
  } # End of loop through PRISM grid cells
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_007c_Download_RAWS_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



gather_lspc_raws_ids <- function (controlDF, mergeDFs = TRUE) {
  
  # Each watershed has a list of relevant RAWS stations in their 
  # respective project control files
  
  # These tables contain RAWS IDs
  
  # This function can help prepare a list, with each element containing 
  # a different watershed's RAWS tibble
  
  # Alternatively, if 'mergeDFs' is TRUE, that list can be combined 
  # into a single tibble
  
  
  # To Do: This function can probably be combined with `gather_lspc_prism_ids`
  
  
  # Use the weather control spreadsheet to read in all watersheds' gage worksheets
  wsGage <- read_all_lspc_project_control(controlDF, worksheet = "Gage")
  
  
  # To Do:
  # Validate the "Gage" worksheets of each control file
  
  
  # Filter the gage table to just RAWS IDs
  # Keep only the "station_id" column too
  wsGage <- wsGage |>
    map(~ filter(., agency_id == "raws") |>
          select(station_id))
  
  
  # If 'mergeDFs' is TRUE, the list of tibbles will be consolidated
  if (mergeDFs) {
    
    # Combine every list element into one tibble
    wsGage <- wsGage |>
      list_rbind()
    
    
    # Address cases of duplicate IDs too
    wsGage <- wsGage |>
      select(station_id) |>
      unique()
    
  }
  
  
  # Return 'wsGage'
  return(wsGage)
  
}



#### Script Execution ####

mainProcedure()


base::remove(list = ls())

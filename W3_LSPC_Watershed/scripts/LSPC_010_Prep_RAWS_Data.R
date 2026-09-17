# Before staging the downloaded climate data into preliminary weather files,
# perform some pre-processing on the RAWS data

# At this point, raw data is present as a shared resource for all watersheds

# In this script, raw RAWS files will be copied into the individual watersheds'
# project folders

# The modified files will be stored within each watershed's "raw/gage/raws" folder
# (e.g., "data/projects/[Watershed]/raw/gage/raws/[id]_[start]_[end].csv")


# To Do: This script contains a lot of overlap with LSPC_009 (PRISM Prep)
# They can be made more generic and share code


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
  cat("Starting 'LSPC_010_Prep_RAWS_Data.R'!\n")
  
  
  # Import the user's input start and end dates
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Import functions from other scripts
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R",
                  "read_all_lspc_project_control")
  
  
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_007c_Download_RAWS_Data.R",
                  "gather_lspc_raws_ids")
  
  
  # After that, read in the LSPC weather control file
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validate Control File
  
  
  # Read in the list of required RAWS grid cells for each watershed
  # ('wsRAWS' will be a list with separate entries for every watershed)
  # (Each element will contain that watershed's required )
  wsRAWS <- controlDF |>
    gather_lspc_raws_ids(mergeDFs = FALSE)
  
  
  # Read in all project control files' "Storage" worksheets after that
  wsDir <- read_all_lspc_project_control(controlDF, worksheet = "Storage")
  
  # To Do:
  # Validate the "Storage" worksheets
  
  
  # Start by making sure each watershed has its raw "raws" folder
  cat("\n[1/3]\tChecking directories...\n")
  
  
  # Iterate through each file
  for (i in 1:length(wsDir)) {
    
    # Try to create the RAWS folder if it doesn't already exist
    try(dir_create(paste0("W3_LSPC_Watershed/",
                          wsDir[[i]] |>
                            filter(scope == "project" & level == "root") |>
                            select(path) |> unlist(use.names = FALSE), 
                          "/",
                          wsDir[[i]] |>
                            filter(scope == "project" & level == "raw" & source == "raws") |>
                            select(path) |> unlist(use.names = FALSE))),
        silent = TRUE)
    
    # To Do: Optimize extracting paths from the storage worksheet with functions
    # build_lspc_project_path(is_shared, level = "root", source = NA_character_)
    
  }
  
  
  cat("\tDone!\n\n")
  
  
  # After that, get information on the downloaded RAWS files
  cat("[2/3]\tGathering RAWS file information...\n")
  
  
  # Use functions from the RAWS download script to get a list of unique station IDs
  # This tibble contains every RAWS station used by the watersheds in 'controlDF'
  rawsDF <- controlDF |>
    gather_lspc_raws_ids(mergeDFs = TRUE)
  
  
  # Get a list of downloaded RAWS files too
  
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
  
  # To Do: A function for building shared paths (checks every file and confirms consistency)
  # build_lspc_shared_path(storageList, ...)
  
  
  # Once the path to the raw folder has been determined, get a list of all CSV files
  # stored within that directory
  rawFiles <- rawFolder |>
    list.files(pattern = "\\.csv$")
  
  
  cat("\tDone!\n\n")
  
  
  # The next step will be producing raw RAWS files for each watershed project
  cat("[3/3]\tCreating RAWS files for each watershed...\n")
  
  
  # Start by iterating through each station file
  for (i in 1:nrow(rawsDF)) {
    
    paste0("\t\t[", i, "/", nrow(rawsDF), "]\t",
           "Processing data for Station ", rawsDF$station_id[i], "...\n\n") |>
      cat()
    
    
    # Identify files in 'rawFolder' that contain this iteration's RAWS station
    searchRegex <- paste0("^", rawsDF$station_id[i], "_")
    
    
    fileSubset <- rawFiles |>
      str_subset(searchRegex)
    
    # This regular expression looks for files that begin with the PRISM ID
    # (followed by an underscore)
    
    
    # Confirm that exactly one file was found
    error_if(length(fileSubset) == 0,
             
             paste0("Could Not Locate Downloaded RAWS Files\n\n",
                    "Through previous scripts, RAWS data should have ",
                    "been downloaded for Station \"", 
                    rawsDF$station_id[i], "\". However, no CSV file ",
                    "was found using the regex \"",
                    searchRegex, "\". Please investigate."))
    
    
    error_if(length(fileSubset) > 1,
             
             paste0("Found Multiple Downloaded RAWS Files\n\n",
                    "Through previous scripts, RAWS data should have ",
                    "been downloaded for Station \"", 
                    rawsDF$station_id[i], "\". However, more than one CSV file ",
                    "was found using the regex \"",
                    searchRegex, "\". Please investigate."))
    
    
    # After that, read in the table of RAWS data for this station
    stationDF <- paste0(rawFolder, "/", fileSubset[1]) |>
      getFile()
    
    
    # To Do: Validate file using the Russian River function?
    
    
    # Extract only the "DATE" and "PRECIPITATION" columns
    # These will be written to a file (without any header)
    stationDF <- stationDF |>
      select(DATE, PRECIPITATION)
    
    
    # The next step is to identify every watershed that requires this file
    relevantWS <- wsRAWS |>
      map_lgl(~ rawsDF$station_id[i] %in% .[["station_id"]]) |>
      which()
    
    # The ID in 'rawsDF' should appear within the watershed's RAWS worksheet
    # (in the "station_id" column)
    
    # Because of `which`, 'relevantWS' will contain the numerical indices of
    # watersheds in 'wsRAWS' that use this RAWS station
    
    
    # For each watershed in 'relevantWS', write 'stationDF' to its raw gage folder
    # (Without any column names)
    for (j in 1:length(relevantWS)) {
      
      # Construct the output path for this watershed
      outPath <- paste0("W3_LSPC_Watershed/",
                        wsDir[[relevantWS[j]]] |>
                          filter(scope == "project" & level == "root") |>
                          select(path) |> unlist(use.names = FALSE),
                        "/",
                        wsDir[[relevantWS[j]]] |>
                          filter(scope == "project" & level == "raw" & source == "raws") |>
                          select(path) |> unlist(use.names = FALSE),
                        "/",
                        rawsDF$station_id[i], "_RAWS_daily.csv")
      
      # The filename will have the same format in each case
      # "[ID]_RAWS_daily.csv"
      
      
      # To Do: Make extracting and defining these types of paths easier
      # (with a function)
      
      
      # Write 'stationDF' to 'outPath'
      # (Do not include column names)
      stationDF |>
        writeOutput(outPath, col_names = FALSE, quietly = TRUE)
      
    }
    
  } # End of loop through RAWS stations
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_010_Prep_RAWS_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

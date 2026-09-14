# Download precipitation data from PRISM at various locations across each watershed

# These locations correspond to grid cells within each watershed boundary


# Coordinates for each watershed's project control file (in the "Prism" worksheet)

# Each of these files must contain these three columns:
#  (1) prism_id
#  (2) lat
#  (3) lon


# A CSV file is produced for each corresponding row in these worksheets

# These files will be stored within each watershed's "candidate" folder
# (e.g., "data/projects/Navarro/candidate/prism/[id]_[lon]_[lat].csv")


# NOTE: The data will use US Customary units (i.e., inches)


#### Setup ####

base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")


# Allow greater time to download data from PRISM
# (This is only relevant for large data downloads)
options(timeout = 5000) # 5000 seconds



#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'LSPC_004c_Download_PRISM_Data.R'!\n")
  
  
  # Import functions from other scripts
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R",
                  "read_all_lspc_project_control")
  
  
  c("scrapePRISM", "validateReqResults", "splitRequest", "combineRawOutputs") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_001_PRISM_HTTP_Scraper.R", .))
  
  
  # Read in the main weather control file for LSPC
  controlDF <- read_lspc_weather_control()
  
  
  # Import the start and end date next
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # PRISM does not have data earlier than 1981-01-01
  # If 'startDate' is earlier than this date, output a warning message
  if (startDate < prism_start()) {
    
    paste0("The earliest date for which PRISM has data available is ", 
           prism_start(), ". The input start date (\"", startDate, "\") is ",
           "too early.") |>
      errWrap() |>
      message()
    
  }
  
  
  # Read in all project control files after that
  wsDir <- read_all_lspc_project_control(controlDF, worksheet = "Storage")
  
  wsPRISM <- read_all_lspc_project_control(controlDF, worksheet = "Prism")
  
  
  # To Do:
  # Validate the "Prism" worksheets of each control file
  # Validate the "Storage" worksheets too
  
  
  # Start by making sure each watershed has its "candidate" folder
  cat("\n[1/2]\tChecking directories...\n")
  
  
  # Iterate through each file
  for (i in 1:length(wsDir)) {
    
    # Try to create the "candidate" PRISM folder if it doesn't already exist
    try(dir_create(paste0("W3_LSPC_Watershed/",
                          wsDir[[i]] |>
                            filter(scope == "project" & level == "root") |>
                            select(path) |> unlist(use.names = FALSE), 
                          "/",
                          wsDir[[i]] |>
                            filter(scope == "project" & level == "candidate" & source == "prism") |>
                            select(path) |> unlist(use.names = FALSE))),
        silent = TRUE)
    
    # To Do: Optimize extracting paths from the storage worksheet with functions
    # build_lspc_project_path(is_shared, level = "root", source = NA_character_)
    
  }
  
  
  cat("\tDone!\n\n")
  
  
  # After that, create a single data frame with every relevant PRISM grid cell
  # across all watersheds
  prismDF <- wsPRISM |>
    list_rbind()
  
  
  # Address cases of duplicate IDs
  prismDF <- prismDF |>
    remove_duplicate_prism_ids()
  
  
  # Finally, begin downloading data for each watershed
  
  cat(paste0("[2/2]\tGetting precipitation data for ", length(wsPRISM), 
             " watersheds...\n"))
  
  
  # Iterate through the rows of 'prismDF'
  for (i in 1:nrow(prismDF)) {
    
    # Each row corresponds to a single PRISM grid cell
    # (though multiple watersheds may rely on the same cell)
    
    
    # Create a temporary data frame with the required formatting for station information
    stationDF <- data.frame(STATION_ID = prismDF$prism_id[i],
                            LATITUDE = prismDF$lat[i],
                            LONGITUDE = prismDF$lon[i])
    
    # To Do: Validate 'stationDF' using RR Workflow function
    
    
    # Define the initial download path as well
    # At first, the file will be stored in the shared PRISM directory
    initialPath <- paste0("W3_LSPC_Watershed/",
                          
                          wsDir[[1]] |> filter(scope == "shared" & level == "root") |> 
                            select(path) |> unlist(use.names = FALSE), 
                          
                          "/",
                          
                          wsDir[[1]] |> filter(scope == "shared" & level == "raw" & source == "prism") |> 
                            select(path) |> unlist(use.names = FALSE),
                          
                          "/", stationDF$STATION_ID, "_", 
                          startDate, "_", endDate, ".csv")
    
    
    # Download PRISM data and save it to 'initialPath'
    stationDF |> 
      scrapePRISM(startDate, endDate, writePath = initialPath, 
                  useHighRes = FALSE, interpCells = FALSE, 
                  getPrecip = TRUE, getTemp = FALSE,
                  useMetric = FALSE, isDaily = FALSE)
    
    
    # Wait a moment before proceeding
    Sys.sleep(runif(1, min = 1, max = 1.2))
    
    
    # Read in the downloaded PRISM data
    gridDF <- getPRISM(initialPath)
    
    
    # To Do: Validate 'gridDF' using RR Workflow function?
    
    
    # Adjust 'gridDF' 
    # Keep only the date and precipitation columns
    # Also, convert 'Date' into a character column that has a "MM/DD/YYYY" format
    # (with no leading zeros)
    gridDF <- gridDF |>
      select(Date, `ppt (inches)`) |>
      mutate(Date = format(Date, "%m/%d/%Y") |>
               str_remove("^0") |> 
               str_remove("(?<=/)0"))
    
    # The first `str_remove` call removes any leading zero for the month
    # The second `str_remove` call removes any leading zero for the day
    # (it uses a lookbehind regex check for "/")
    
    
    # Identify which watersheds require this grid cell in their dataset
    relevantWS <- wsPRISM |>
      map_lgl(~ stationDF$STATION_ID %in% .[["prism_id"]]) |>
      which()
    
    # The ID in 'stationDF' should appear within the watershed's PRISM worksheet
    # (in the "prism_id" column)
    
    # Because of `which`, 'relevantWS' will contain the numerical indices of 
    # watersheds in 'wsPRISM' that use this PRISM grid cell
    
    
    # For each watershed in 'relevantWS', write 'gridDF' to its "candidate" folder
    # (Without any column names)
    for (j in 1:length(relevantWS)) {
      
      # Construct the output path for this watershed
      outPath <- paste0("W3_LSPC_Watershed/",
                        wsDir[[relevantWS[j]]] |>
                          filter(scope == "project" & level == "root") |>
                          select(path) |> unlist(use.names = FALSE), 
                        "/",
                        wsDir[[relevantWS[j]]] |>
                          filter(scope == "project" & level == "candidate" & source == "prism") |>
                          select(path) |> unlist(use.names = FALSE),
                        "/",
                        stationDF$STATION_ID, "_", stationDF$LONGITUDE, "_",
                        stationDF$LATITUDE, ".csv")
      
      
      # Write 'gridDF' to 'outPath'
      # (Do not include column names)
      gridDF |>
        writeOutput(outPath, col_names = FALSE, quietly = TRUE)
      
    }
    
  } # End of loop through PRISM grid cells
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_004c_Download_PRISM_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



remove_duplicate_prism_ids <- function (prismDF) {
  
  # Given a tibble of PRISM IDs, check for duplicate entries
  # (same IDs, different coordinates)
  
  # If the coordinates are similar, just remove the less precise coordinates
  # However, if the coordinates are sizably different, output an error message
  
  
  # First, remove duplicates where the coordinates are exactly the same
  prismDF <- prismDF |>
    unique()
  
  
  # Next, get a vector of duplicate IDs
  dupIDs <- prismDF |>
    group_by(prism_id) |>
    summarize(COUNT = n(), .groups = "drop") |>
    filter(COUNT > 1) |> # IDs with a recorded count greater than 1
    select(prism_id) |>
    unlist(use.names = FALSE)
  
  
  # If 'dupIDs' is empty, return 'prismDF'
  # (That would signify that no duplicates are present)
  if (length(dupIDs) == 0) {
    return(prismDF)
  }
  
  
  # Otherwise, check each duplicated ID
  for (i in 1:length(dupIDs)) {
    
    # Get the pairs of coordinates that correspond to this ID
    latVec <- prismDF$lat[prismDF$prism_id == dupIDs[i]]
    lonVec <- prismDF$lon[prismDF$prism_id == dupIDs[i]]
    
    
    # Round each vector to three decimal places
    # If the number of unique values in each vector is greater than one, 
    # output an error message
    # (Basically, if each entry's coordinate values are "close enough", there is no major error)
    if (!has_similar_coordinate_values(latVec) || !has_similar_coordinate_values(lonVec)) {
      
      paste0("Different Coordinates for Same PRISM ID\n\n",
             "Across the watersheds' project control files, there are ",
             nrow(prismDF |> filter(prism_id == dupIDs[i])), " entries for ",
             "Grid Cell \"", dupIDs[i], "\". However, the coordinates are ",
             "inconsistently specified. Pleaes correct these values.") |>
        stop_script()
      
      # To Do: It would be helpful to have the names of the actual watershed 
      # control files that have this PRISM ID
      
    }
    
    
    # Next, remove all rows from 'prismDF' that have this ID
    # (They will be replaced with a single row in the next step)
    prismDF <- prismDF |>
      filter(prism_id != dupIDs[i])
    
    
    # Create a replacement row for this PRISM ID
    # Its latitude and longitude coordinates will be averages computed using
    # 'latVec' and 'lonVec'
    prismDF <- prismDF |>
      bind_rows(tibble(prism_id = dupIDs[i], lat = mean(latVec), lon = mean(lonVec)))
    
  }
  
  
  # Return 'prismDF' after this
  return(prismDF)
  
}



has_similar_coordinate_values <- function (coordVec) {
  
  # Given a set of latitude coordinates, or a set of longitude coordinates,
  # check whether the values are similar enough
  
  # Arbitrarily, if the values all match when rounded to three decimal places,
  # return TRUE
  return(coordVec |> round(digits = 3) |> unique() |> length() == 1)
  
  # When rounding the coordinate values, if there is only one unique value
  # left after the operation, that means that they are all "similar enough" 
  
}



#### Script Execution ####

mainProcedure()


base::remove(list = ls())

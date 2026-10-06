# Before staging the downloaded climate data into preliminary weather files,
# perform some pre-processing on the PRISM data

# At this point, raw data is present as a shared resource for all watersheds

# In this script, candidate PRISM files will be created for each individual watershed 
# (the formatting is adjusted to fit the Python staging scripts' expectations)

# The modified files will be stored within each watershed's "candidate" folder
# (e.g., "data/projects/[Watershed]/candidate/prism/[id]_[lon]_[lat].csv")


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
  cat("Starting 'LSPC_009_Prep_Candidate_PRISM_Data.R'!\n")
  
  
  # Import the user's input start and end dates
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Import functions from other scripts
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R",
                  "read_all_lspc_project_control")
  
  c("gather_lspc_prism_ids", "remove_duplicate_prism_ids", "has_similar_coordinate_values") |>
    map(~ functionStealer("W3_LSPC_Watershed/scripts/LSPC_004c_Download_PRISM_Data.R", .))
  
  
  # After that, read in the LSPC weather control file
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validate Control File
  
  
  # Read in the list of required PRISM grid cells for each watershed
  # ('wsPRISM' will be a list with separate entries for every watershed)
  # (Each element will contain that watershed's required )
  wsPRISM <- controlDF |>
    gather_lspc_prism_ids(mergeDFs = FALSE)
  
  
  # Read in all project control files' "Storage" worksheets after that
  wsDir <- read_all_lspc_project_control(controlDF, worksheet = "Storage")
  
  # To Do:
  # Validate the "Storage" worksheets
  
  
  # Start by making sure each watershed has its "candidate" folder
  cat("\n[1/3]\tChecking directories...\n")
  
  
  # Iterate through each file
  for (i in 1:length(wsDir)) {
    
    # Try to create the "candidate" PRISM folder if it doesn't already exist
    catch_warnings_and_errors(
      dir_create(paste0("W3_LSPC_Watershed/",
                        wsDir[[i]] |>
                          filter(scope == "project" & level == "root") |>
                          select(path) |> unlist(use.names = FALSE), 
                        "/",
                        wsDir[[i]] |>
                          filter(scope == "project" & level == "candidate" & source == "prism") |>
                          select(path) |> unlist(use.names = FALSE)))
    )
    
    # To Do: Optimize extracting paths from the storage worksheet with functions
    # build_lspc_project_path(is_shared, level = "root", source = NA_character_)
    
  }
  
  
  cat("\tDone!\n\n")
  
  
  # After that, get information on the raw PRISM files
  cat("[2/3]\tGathering PRISM file information...\n")
  
  
  # Use functions from the PRISM download script to get a list of unique PRISM IDs
  # This tibble contains every PRISM cell used by the watersheds in 'controlDF'
  prismDF <- controlDF |>
    gather_lspc_prism_ids(mergeDFs = TRUE)
  
  
  # Get a list of raw PRISM files too
  
  # These files were either downloaded recently or copied over from the folder 
  # that contains historic PRISM files
  
  # Use each watershed's project control file to build a path to the shared PRISM folder
  # Then, call `unique`, which should result in a single path
  rawFolder <- wsDir |>
    map_chr(~ paste0("W3_LSPC_Watershed/",
                     filter(., scope == "shared" & level == "root") |>
                       select(path) |> unlist(use.names = FALSE),
                     "/",
                     filter(., scope == "shared" & level == "raw" & source == "prism") |>
                       select(path) |> unlist(use.names = FALSE))) |>
    unique()
  
  stopifnot(length(rawFolder) == 1)
  
  # To Do: A function for building shared paths (checks every file and confirms consistency)
  # build_lspc_shared_path(storageList, ...)
  
  
  # Once the path to the raw folder has been determined, get a list of all CSV files
  # stored within that directory
  prismFiles <- rawFolder |>
    list.files(pattern = "\\.csv$")
  
  
  cat("\tDone!\n\n")
  
  
  # The next step will be producing candidate files for each grid cell
  cat("[3/3]\tCreating candidate files for each PRISM grid cell...\n")
  
  
  # There may be multiple files for each PRISM grid cell
  # (One that was recently downloaded in this workflow and at least one from 
  #  the folder that contains historic data)
  
  # These files will be combined into one file per grid cell
  
  # Then, multiple watersheds may rely on this PRISM grid, so that file can be 
  # saved in multiple "candidate" folders
  
  
  # Begin by iterating through each grid cell
  for (i in 1:nrow(prismDF)) {
    
    # Output a message to the console occassionally to show progress
    if (i %% 50 == 0) {
      
      paste0("\t\t[", i, "/", nrow(prismDF), "]\t",
             "Processing data for Grid Cell ", prismDF$prism_id[i], "...\n\n") |>
        cat()
      
    }
    
    
    # Identify files in 'rawFolder' that contain this iteration's PRISM ID
    searchRegex <- paste0("^", prismDF$prism_id[i], "_")
    
    
    fileSubset <- prismFiles |>
      str_subset(searchRegex)
    
    # This regular expression looks for files that begin with the PRISM ID
    # (followed by an underscore)
    
    
    # Confirm that at least one file was found
    error_if(length(fileSubset) == 0,
             
             paste0("Could Not Locate Raw PRISM Files\n\n",
                    "Through previous scripts, PRISM data should have ",
                    "been downloaded for Grid Cell \"", 
                    prismDF$prism_id[i], "\". However, no CSV file ",
                    "was found using the regex \"",
                    searchRegex, "\". Please investigate."))
    
    
    # After that, if exactly one file is present in 'fileSubset', read in that file
    # Otherwise, if multiple files were downloaded, a more complicate procedure
    # is required (so that they can be combined into a single tibble)
    gridDF <- paste0(rawFolder, "/", fileSubset) |>
      read_and_combine_prism(startDate, endDate)
    
    
    # The next step is to make adjustments to 'gridDF'
    
    # Only two columns are required: the Date and precipitation values
    # (though the files will be written without any column names)
    
    # The Date values must have a specific formatting (YYYY-MM-DD)
    
    # In addition, only records with non-zero precipitation values will be kept
    gridDF <- gridDF |>
      select(Date, `ppt (inches)`) |>
      mutate(Date = format(Date, "%Y-%m-%d")) |>
      filter(`ppt (inches)` > 0)
      
    
    # The penultimate step is to determine which watersheds rely on this particular
    # PRISM grid cell
    relevantWS <- wsPRISM |>
      map_lgl(~ prismDF$prism_id[i] %in% .[["prism_id"]]) |>
      which()
    
    # The ID in 'prismDF' should appear within the watershed's PRISM worksheet
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
                        prismDF$prism_id[i], "_", round(prismDF$lon[i], digits = 3), 
                        "_", round(prismDF$lat[i], digits = 3), ".csv")
      
      # The filename will have the same format in each case
      # "[GRID]_[LONGITUDE]_[LATITUDE].csv"
      
      # The coordinates will have at most three decimal places too
      
      
      # To Do: Make extracting and defining these types of paths easier
      # (with a function)
      
      
      # Write 'gridDF' to 'outPath'
      # (Do not include column names)
      gridDF |>
        writeOutput(outPath, col_names = FALSE, quietly = TRUE)
      
    }
    
  } # End of loop through PRISM grid cells
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_009_Prep_Candidate_PRISM_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



read_and_combine_prism <- function (pathVec, startDate, endDate) {
  
  # Given the paths to one or more PRISM files, read them in
  
  # If there is only one filepath, return the resultant tibble only
  
  # Otherwise, if there are multiple PRISM files, try to combine them
  # into one tibble
  
  # If there is overlap between the values of these files, give preference 
  # to files that have a more recent start date (and then a more recent end date)
  
  # If the file's start and end dates match 'startDate' and 'endDate', give those
  # values the highest priority (regardless of whether there's files with later
  # start/end dates)
  
  
  # Start by checking if 'pathVec' contains only one file path
  # In that case, read in the PRISM data and return it
  if (length(pathVec) == 1) {
    
    return(pathVec[1] |> getPRISM())
    
  }
  
  
  # Otherwise, if there are multiple files, try to extract their start and end dates
  # That will help establish the order of priority 
  pathDF <- tibble(PATH = pathVec) |>
    mutate(START = PATH |> str_extract("(?<=_)[0-9]{4}-[0-9]{2}-[0-9]{2}(?=_)") |>
             as.Date(format = "%Y-%m-%d"),
           END = PATH |> str_extract("(?<=_)[0-9]{4}-[0-9]{2}-[0-9]{2}(?=\\.)") |>
             as.Date(format = "%Y-%m-%d"))
  
  
  # Make sure the dates were extracted properly
  # (If the file format is something different, this will result in an error)
  error_if(anyNA(pathDF$START) || anyNA(pathDF$END),
           
           paste0("PRISM File Error\n\n",
                  "All raw PRISM CSV files are expected to have a start date ",
                  "and end date embedded in their filenames ",
                  "(i.e., ending with \"YYYY-MM-DD_YYYY-MM-DD.csv\"). However, ",
                  "these dates could not be extracted properly for \"",
                  pathDF$PATH[is.na(pathDF$START) | is.na(pathDF$END)][1], "\" ",
                  if_else(sum(is.na(pathDF$START) | is.na(pathDF$END)) > 1,
                          "(and other similar files)",
                          ""),
                  ". Please investigate."
           ))
  
  
  # After that, check if any file has start and end dates that match the
  # user's specified date bounds ('startDate' and 'endDate')
  pathDF <- pathDF |>
    mutate(IS_RECENTLY_DOWNLOADED = 
             START == startDate & END == endDate)
  
  
  # The next step is to sort 'pathDF' in the desired order
  pathDF <- pathDF |>
    arrange(IS_RECENTLY_DOWNLOADED, START, END, PATH)
  
  # This sorting places the recently downloaded file at the end of the tibble
  
  # Then, the remaining tibble rows are arranged so that entries with a later
  # start date are lower in the list 
  
  # And if there are ties in the start date, the end date will be used to sort
  # those rows
  
  # In extreme cases, if there are files with the same date bounds (and yet somehow
  # different filenames), these files will be sorted by the differences in their 
  # names in "PATH"
  
  
  # With a sorting established, iteratively read in the files
  # Prioritize records in files that are lower in the list
  for (i in 1:nrow(pathDF)) {
    
    # Read in the file
    tempRead <- pathDF$PATH[i] |>
      getPRISM()
    
    
    # To Do: Validate information in PRISM file
    
    
    # In the first iteration, define 'combinedDF' using this tibble
    if (i == 1) {
      
      combinedDF <- tempRead
      
    } else {
      
      # In subsequent iterations, remove records in 'combinedDF' that overlap 
      # with the dates in 'tempRead'
      
      dateVec <- seq(from = pathDF$START[i], to = pathDF$END[i], by = "days")
      
      # 'dateVec' is a vector with every day from the start date to the end date
      # specified in the name of this iteration's file
      
      
      # Remove records in 'combinedDF' that have a matching date
      combinedDF <- combinedDF |>
        filter_out(Date %in% dateVec)
      
      
      # Then, bind 'tempRead' to 'combinedDF' 
      combinedDF <- bind_rows(combinedDF, tempRead)
      
    }
    
  }
  
  
  # Double-check that no duplicated dates are present in 'combinedDF'
  error_if(length(unique(combinedDF$Date)) != nrow(combinedDF),
           
           paste0("Duplicate Dates Detected\n\n",
                  "While combining the contents of \"", pathDF$PATH[1], 
                  "\" and other files for the same PRISM grid cell, an error ",
                  "occurred. For some reason, the same date appears more ",
                  "than once in the combined result. Please correct the script."))
  
  
  # Filter the dataset to not exceed 'endDate' as well
  combinedDF <- combinedDF |>
    filter(Date < ceiling_date(endDate, unit = "month"))
  
  # Note: `ceiling_date` will round 'endDate' to the next month
  #       (e.g., the ceiling month for 2026-07-01 and 2026-07-31 
  #        is 2026-08-01 in both cases)
  
  
  # After that, as a final check, look for gaps in the dataset
  
  expectedDates <- seq(from = min(combinedDF$Date),
                       to = max(combinedDF$Date), 
                       by = "month")
  # (The data is on a monthly scale, and there should be no gaps)
  
  
  # Look for missing months in 'combinedDF'
  missingDates <- expectedDates[expectedDates %notin% combinedDF$Date]
  
  
  if (length(missingDates) > 0) {
    
    cat("\n\n")
    cat("Missing Dates:\n")
    print(format(missingDates, "%Y-%m"))
    cat("\n\n")
    
    paste0("Missing ", length(missingDates), " Months of Data\n\n",
           "While combining \"", pathDF$PATH[1], "\" and related files, ",
           length(missingDates), " month",
           if_else(length(missingDates) > 1, "s", ""), 
           " in the range between ", 
           min(combinedDF$Date), " and ", max(combinedDF$Date), " ",
           if_else(length(missingDates) > 1, "were", "was"), " found to ",
           "be missing. As a result, the combined dataset is not continuous. ",
           "Please consider adding more historic data for this grid cell.") |>
      stop_script()
    
    # To Do: Make missing date check a generic function
    
  }
  
  
  # Sort 'combinedDF' by "Date" and return it
  return(combinedDF |>
           arrange(Date))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

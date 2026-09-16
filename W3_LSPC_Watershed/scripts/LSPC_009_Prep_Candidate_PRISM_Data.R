# Before staging the downloaded climate data into preliminary weather files,
# perform some pre-processing on the PRISM data

# At this point, raw data is present as a shared resource for all watersheds

# In this script, candidate PRISM files will be created for each individual watershed 
# (the formatting is adjusted to fit the Python staging scripts' expectations)

# The modified files will be stored within each watershed's "candidate" folder
# (e.g., "data/projects/Navarro/candidate/prism/[id]_[lon]_[lat].csv")


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
  
  
  
  
  # There may be multiple files for each PRISM grid cell
  # (One from the historic folder and one that was recently downloaded in this workflow)
  
  
  # Read in all raw files related to a PRISM grid cell
  
  # Prefer the file that has 'startDate' and 'endDate' in its name when there's overlap
  
  
  
  
  
  
  
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

  
  
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_009_Prep_Candidate_PRISM_Data.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

# Copy weather files from the output of the "lspc-climate-processing-restructure" scripts

# Paste them into the weather input folder for an LSPC run

# The required weather files are specified in the "inp" files of the LSPC model files


base::remove(list = ls())


require(tidyverse)


# Locate watersheds' project folders in this "W3" sub-folder
watershedVec <- list.files("W3_LSPC_Watershed/data/projects", full.names = TRUE)


# Only watersheds with a "curated" folder are considered
curatedCheck <- watershedVec |>
  map_lgl(~ "curated" %in% list.files(.))


# If none of them have a "curated" folder, stop this script
# The "lspc-climate-processing-restructure" scripts must be run first
if (!any(curatedCheck)) {
  stop("Please generate weather files before running this script")
}


# Filter 'watershedVec' to only watersheds with the "curated" folder
watershedVec <- watershedVec[curatedCheck]


# Next, look for corresponding model sub-folders for each watershed under 'LSPC'
watershedNames <- watershedVec |>
  str_remove("^.+[/\\\\]")


# All of these watersheds should have a folder containing LSPC model files
if (!all(watershedNames %in% list.files("Models/LSPC"))) {
  stop("Not all watersheds have LSPC model files ready")
}


# After that, get the inp files for each watershed
inpPaths <- watershedNames |>
  map(~ list.files(paste0("Models/LSPC/", ., "/Input"), 
                   full.names = TRUE, pattern = "\\.inp$"))


# Each folder should have exactly one input file
if (any(lengths(inpPaths) > 1)) {
  stop("One or more watersheds have multiple .inp files in their model folders!")
}


if (any(lengths(inpPaths) == 0)) {
  stop("One or more watersheds are missing an .inp file in their model folders!")
}


# Convert 'inpPaths' into a vector
inpPaths <- inpPaths |> unlist()


# Finally, confirm that the "Weather" folders are present for each watershed model
weatherPaths <- watershedNames |>
  map(~ list.files(paste0("Models/LSPC/", ., "/Input"), 
                   full.names = TRUE, pattern = "^Weather$"))


# There should be one "Weather" folder per watershed model folder
if (any(lengths(weatherPaths) > 1)) {
  stop("One or more watersheds somehow have multiple \"Weather\" folders! That's impossible!")
}


if (any(lengths(weatherPaths) == 0)) {
  stop("One or more watersheds are missing a \"Weather\" sub-folder in their model input folders!")
}


# Convert the list into a vector
weatherPaths <- weatherPaths |>
  unlist()


# For each watershed, copy over the weather files
cat("\n\n")
cat(paste0("Copying weather files for ", length(watershedNames), " watershed(s)!"))
cat("\n\n")


# Iterate through each watershed
for (i in 1:length(watershedNames)) {
  
  cat("\n\n")
  cat(paste0("Watershed ", i, ": ", watershedNames[i]))
  cat("\n\n")
  
  
  # Location of Weather Output Files
  generatedWeatherDir <- paste0(watershedVec[i], "/curated/") |>
    normalizePath(mustWork = TRUE)
  
  
  # Path to LSPC inp file
  inpFilePath <- inpPaths[i]
  
  
  # Location of LSPC weather folder
  weatherFolder <- weatherPaths[i]
  
  
  # Error Check
  stopifnot(dir.exists(generatedWeatherDir))
  stopifnot(file.exists(inpFilePath))
  stopifnot(dir.exists(weatherFolder))
  
  
  # Get a list of all air and pre files from 'generatedWeatherDir'
  weatherDF <- tibble(FULL_PATH = list.files(generatedWeatherDir, recursive = TRUE, full.names = TRUE)) |>
    mutate(FILENAME = str_remove(FULL_PATH, "^.+/")) |>
    mutate(ID = str_extract(FILENAME, "^[0-9]+") |> as.numeric(),
           TYPE = if_else(grepl("\\.air", FILENAME), "AIR", "PRE"))
  
  
  # Read the inp file to identify which weather files are actually needed
  weatherReq <- read_lines(inpFilePath)
  
  
  # Keep only Card 10 (c10) in 'weatherReq'
  # This section contains the names of the required weather files
  weatherReq <- weatherReq[grep("c10 ", weatherReq)[1]:length(weatherReq)]
  
  
  weatherReq <- weatherReq[1:grep("^c--", weatherReq)[1]]
  
  
  # Remove all lines that start with "c" (these are comment fields)
  weatherReq <- weatherReq |>
    str_subset("^c", negate = TRUE)
  
  
  # Reformat 'weatherReq' (splitting the tabs) and extract the filenames
  weatherReq <- weatherReq |>
    str_split("\\t") |>
    unlist() |>
    str_subset("\\.(air)?(pre)?") |>
    tibble() |>
    set_names("FILENAME") |>
    mutate(ID = str_extract(FILENAME, "^[0-9]+") |> as.numeric(),
           TYPE = if_else(grepl("\\.air", FILENAME), "AIR", "PRE"))
  
  
  stopifnot(nrow(weatherReq) > 0)
  
  
  # Iterate through the list given in 'weatherReq' and find matching files in 'weatherDF'
  # If an exact match is found, copy that file as-is into 'weatherFolder'
  # Otherwise, try to find the corresponding file 
  # (Some filenames may be different due to changing conventions)
  for (j in 1:nrow(weatherReq)) {
    
    # Try to find this file among the script-generated weather files in 'weatherDF'
    matchIndex <- which(weatherDF$FILENAME == weatherReq$FILENAME[j])
    
    
    # If a match was found, copy it to the LSPC weather folder
    if (length(matchIndex) == 1) {
      
      file.copy(from = weatherDF$FULL_PATH[matchIndex],
                to = paste0(weatherFolder, "/", weatherReq$FILENAME[j]) |> normalizePath(mustWork = FALSE),
                overwrite = TRUE)
      
      # If no match was found, check for similar-named files
    } else if (length(matchIndex) == 0) {
      
      # Look for files with the same TYPE and ID
      candidateFiles <- weatherDF |>
        filter(TYPE == weatherReq$TYPE[j] & ID == weatherReq$ID[j])
      
      
      # If the file in 'weatherReq' has underscores, and only one file in 'candidateFiles'
      # also has underscores, assume that that file is its equivalent one
      if (grepl("_", weatherReq$FILENAME[j]) &&
          sum(grepl("_", candidateFiles$FILENAME)) == 1) {
        
        matchIndex <- grep("_", candidateFiles$FILENAME)
        
        
        file.copy(from = candidateFiles$FULL_PATH[matchIndex],
                  to = paste0(weatherFolder, "/", weatherReq$FILENAME[j]) |> normalizePath(mustWork = FALSE),
                  overwrite = TRUE)
        
        
        cat(paste0("\nRenaming \"", candidateFiles$FILENAME[matchIndex], "\" to \"",
                   weatherReq$FILENAME[j], "\"!\n"))
        
        
        # In all other cases, output an error
      } else {
        
        stop("Could not find a corresponding file among the generated weather files")
        
      }
      
      
      # If multiple matches are found, that's a weird error
    } else {
      
      stop("Multiple matches for the same weather file?")
      
    }
    
  }
  
}


# Output a message
print("Done!")

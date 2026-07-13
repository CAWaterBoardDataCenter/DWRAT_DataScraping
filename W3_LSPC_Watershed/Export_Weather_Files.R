# Copy weather files from the output of "lspc-climate-processing-restructure"

# Paste them into the weather input folder for an LSPC run

# The required weather files are specified in the "inp" file of the LSPC model files


remove(list = ls())


require(tidyverse)


# Location of Weather Output Files
generatedWeatherDir <- "projects/Navarro/curated/"
  #"Navarro/curated"


# Path to LSPC inp file
inpFilePath <- "LSPC/Navarro/Input/Navarro_extended_2025-07-31.inp"


# Location of LSPC weather folder
weatherFolder <- "LSPC/Navarro/Input/Weather"


# Click "Source" to run this script after setting the above paths!



# Error check
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
for (i in 1:nrow(weatherReq)) {
  
  # Try to find this file among the script-generated weather files in 'weatherDF'
  matchIndex <- which(weatherDF$FILENAME == weatherReq$FILENAME[i])
  
  
  # If a match was found, copy it to the LSPC weather folder
  if (length(matchIndex) == 1) {
    
    file.copy(from = weatherDF$FULL_PATH[matchIndex],
              to = paste0(weatherFolder, "/", weatherReq$FILENAME[i]) |> normalizePath(mustWork = FALSE),
              overwrite = TRUE)
    
  # If no match was found, check for similar-named files
  } else if (length(matchIndex) == 0) {
    
    # Look for files with the same TYPE and ID
    candidateFiles <- weatherDF |>
      filter(TYPE == weatherReq$TYPE[i] & ID == weatherReq$ID[i])
    
    
    # If the file in 'weatherReq' has underscores, and only one file in 'candidateFiles'
    # also has underscores, assume that that file is its equivalent one
    if (grepl("_", weatherReq$FILENAME[i]) &&
        sum(grepl("_", candidateFiles$FILENAME)) == 1) {
      
      matchIndex <- grep("_", candidateFiles$FILENAME)
      
      
      file.copy(from = candidateFiles$FULL_PATH[matchIndex],
                to = paste0(weatherFolder, "/", weatherReq$FILENAME[i]) |> normalizePath(mustWork = FALSE),
                overwrite = TRUE)
      
      
      cat(paste0("\nRenaming \"", candidateFiles$FILENAME[matchIndex], "\" to \"",
                 weatherReq$FILENAME[i], "\"!\n"))
      
      
      # In all other cases, output an error
    } else {
      
      stop("Could not find a corresponding file among the generated weather files")
      
    }
    
    
  # If multiple matches are found, that's a weird error
  } else {
    
    stop("Multiple matches for the same weather file?")
    
  }
  
}


# Output a message
print("Done!")

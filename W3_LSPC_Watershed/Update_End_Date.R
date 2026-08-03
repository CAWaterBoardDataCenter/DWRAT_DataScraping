# Within the LSPC model files, check the inp files for each watershed
# Ensure that the end dates in Card 50 (c50) match the weather files' values


base::remove(list = ls())


require(tidyverse)


# Get the list of watersheds in the LSPC model folder
watershedList <- list.files("Models/LSPC/")


# Iterate through each watershed
for (i in 1:length(watershedList)) {
  
  cat("\n\n")
  print(paste0("Watershed ", i, " of ", length(watershedList), ": ", watershedList[i]))
  
  
  # Locate the watershed's inp file
  inpPath <- list.files(paste0("Models/LSPC/", watershedList[i], "/Input/"),
                        pattern = "\\.inp$", full.names = TRUE)
  
  
  # There should be exactly one match
  if (length(inpPath) == 0) {
    stop(paste0(".inp file not found for ", watershedList[i]))
  } else if (length(inpPath) > 1) {
    stop(paste0("Multiple .inp files found for ", watershedList[i]))
  }
  
  
  # Check one of the .air weather files for this watershed
  # Identify the end date based on the contents of this file
  airPath <- list.files(paste0("Models/LSPC/", watershedList[i], "/Input/Weather/"),
                        pattern = "\\.air$", full.names = TRUE)[1]
  
  
  if (length(airPath) == 0) {
    stop(paste0(watershedList[i], " has no .air weather files?"))
  }
  
  
  # Read in 'airPath' (keep the last line only)
  airVec <- read_lines(airPath) |>
    tail(1)
  
  
  # Extract the date from 'airVec'
  endDate <- airVec |>
    str_extract("[0-9]{4}\t[0-9]{1,2}\t[0-9]{1,2}") |>
    as_date(format = "%Y\t%m\t%d")
  
  
  print(paste0("End date is ", endDate, "!"))
  
  
  # 'endDate' will be used as the value for 'mend' and 'moend'
  # (model end day and model output end day)
  
  
  # Read in the inp file after that
  inpVec <- read_lines(inpPath)
  
  
  # Locate the line in Card 50 that contains the input date values
  # (It comes after a line that contains the header names like "mstart" and "mend")
  dateLine <- grep("mstart.mend.deltm", inpVec)[1] + 1
  
  
  # Split the contents of 'inpVec' at 'dateLine'
  inpLine <- inpVec[dateLine] |>
    str_split("\t") |>
    unlist()
  
  
  # Replace the second date and the final date with 'endDate'
  dateLocs <- which(grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", inpLine))
  
  
  if (length(dateLocs) != 4) {
    stop("Could not extract all four dates from the line in Card 50!")
  }
  
  
  # Only the second and fourth dates' locations are required
  dateLocs <- c(dateLocs[2], tail(dateLocs, 1))
  
  
  # Set both values as 'endDate' 
  inpLine[dateLocs] <- format(endDate, "%m/%d/%Y") |>
    str_remove("^0") |>
    str_replace("/0", "/")
  
  
  # Combine 'inpLine' back into a single string and insert it back into 'inpVec'
  inpVec[dateLine] <- inpLine |>
    paste0(collapse = "\t")
  
  
  # Write 'inpVec' back to a file
  writeLines(inpVec, inpPath)
  
}


print("Done!")


# Clear the environment
base::remove(list = ls())

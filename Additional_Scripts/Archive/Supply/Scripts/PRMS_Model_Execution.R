# Prepare files for running the PRMS model
# Then, run the model (through R)


#### Dependencies ####

require(tidyverse)

#### Script Procedure ####

mainProcedure <- function (StartDate, EndDate, End_Date) {
  
  # The main body of the script
  
  
  # First, prepare a new DAT file for the model
  # Perform these steps in a separate function
  # (Only return the filepath of the new DAT file here)
  newPath <- createDAT(StartDate, End_Date)
  
  
  
  # Next, update the "prms_rr.control" file 
  # (Do this in a separate function as well)
  controlFileUpdate(newPath, End_Date)

  
  
  # 
  batCheck()
  
  
  
}


createDAT <- function (StartDate, End_Date) {
  
  # Using a source DAT file as a base, create a new DAT file
  # Data rows with dates between StartDate and End_Date will 
  # be replaced using observations and forecasted data prepared by other scripts
  # Data outside of the script's date range will be unchanged
  
  
  # First, prepare a DAT file
  # A "source" version of this file is specified in "File_Paths_and_Dirs.txt" 
  sourcePath <- readPathFile("Path to your source \\.dat")

  
  # The version created by this script will be a modified copy of that file
  # It will also have the forecasted end date in its filename
  newPath <- sourcePath %>% str_replace("update_to_.+\\.dat",
                                        paste0("update_to_", End_Date, ".dat"))
  
  
  # Though, if the source file already has that date in its name, 
  # the filename will be modified further (adding "_New" to the end of the name)
  if (sourcePath == newPath) {
    newPath <- newPath %>%
      str_replace("\\.dat$", "_New.dat")
  }
  
  
  # With the paths established, copy the source DAT file to 'newPath'
  # This copy of the DAT file will now be adjusted with the new data
  file.copy(sourcePath, newPath, overwrite = TRUE)
  
  
  # Read that new file into R
  datData <- readLines(newPath, warn = FALSE)
  
  
  # The data in this file are stored as tab-separated values
  # The data from StartDate to End_Date will be replaced
  
  
  # The new data comes from a text file in the "ProcessedData" folder
  # It is stored in "Dat_Final_[End_Date].txt"
  # Read that into R as well
  newData <- readLines(paste0("ProcessedData/Dat_Final_", End_Date, ".txt"), warn = FALSE)
  
  
  #  Find the rows in 'datData' whose dates match with "StartDate" and "End_Date"
  startIndex <- datFormatDate(StartDate$date) %>%
    grep(datData)
    
  endIndex <- datFormatDate(End_Date) %>%
    grep(datData)
  
  
  # Error Check
  # Verify that one match was found for each date
  stopifnot(length(startIndex) == 1)
  stopifnot(length(endIndex) == 1)
  
  
  # Also, verify that the number of rows that will be replaced in 'datData'
  # equals the number of data rows in 'newData'
  # ('newData' has a header row, but that is not counted in the comparison)
  stopifnot(length(newData) - 1 == endIndex - startIndex + 1)
  
  
  # Once this has been verified, update the specified rows of 'datData'
  datData[startIndex:endIndex] <- newData[-1]
  
  
  # Finally, save this updated datset to the path specified in 'newPath'
  datData %>%
    writeLines(newPath)
  
  
  # Return 'newPath' at the end of the function
  return(newPath)
  
}


readPathFile <- function (lineRegex) {
  
  # Read in the text file "File_Paths_and_Dirs.txt"
  # Then, extract a specific line using 'lineRegex'
  # Finally, return the path portion of that line 
  # (the latter half, which is separated by more than two spaces from the other text on that line)
  return(readLines("InputData/File_Paths_and_Dirs.txt", warn = FALSE) %>%
           str_subset(lineRegex) %>%
           str_extract("\\s{2,}.+$") %>% trimws())
  
}


datFormatDate <- function (dateVal) {
  
  # Format a date as a tab-separated string
  # (This is the formatting used in the DAT file)
  
  return(paste0(year(dateVal), "\t", month(dateVal), "\t", day(dateVal), "\t"))
  
  # (A tab is added at the end to ensure that a date string whose day is one, two, or three 
  # will not match with dates whose days are in the tens, twenties, or thirties, respectively)
  
}


controlFileUpdate <- function (newPath, End_Date) {
  
  # Update the PRMS Control File in preparation for the next run of the model
  
  
  # Get the filepath to the control file
  # This is specified in "File_Paths_and_Dirs.txt"
  controlPath <- readPathFile("Path to .prms_rr.control")
  
  
  # Read in the file into R
  controlFile <- readLines(controlPath, warn = FALSE)
  
  
  
  # Update the DAT filepath on Line 51
  # 'newPath' contains that information
  controlFile[51] <- newPath
  
  
  
  # After that, update Line 319
  # This will be the filepath for the output file
  # For this filename, use the path to the "output" folder specified in 
  # "File_Paths_and_Dirs.txt" as well as 'End_Date'
  controlFile[319] <- readPathFile("Path to the PRMS .output. folder") %>%
    paste0("\\nsubout.update_", End_Date)
  
  
  
  # After these modifications, overwrite the control file
  controlFile %>%
    writeLines(controlPath)
  
  
  # Return nothing
  return(invisible(NULL))
  
}


batCheck <- function () {
  
  # Verify that the correct filepath is mentioned in the "run.bat" file
  
  
  # Get the filepath to the BAT file (this is listed in "File_Paths_and_Dirs.txt")
  batPath <- readPathFile("Path to the .run. batch")
  
  
  # Then read that file into R
  batData <- readLines(batPath, warn = FALSE)
  
  
  # Check that the correct path to "gsflow.exe" is listed on the first line of 'batData'
  
  
  # To do this, first read in the path to "gsflow.exe" from "File_Paths_and_Dirs.txt"
  gsPath <- readPathFile("Path to .gsflow")
  
  
  # The first line of the BAT file should be this path 
  # (along with "prms_rr.control" at the end)
  
  
  # If that is not the case, update that line in 'batData' and overwrite the BAT file
  if (batData[1] != paste0(gsPath, " prms_rr.control")) {
    
    batData[1] <- paste0(gsPath, " prms_rr.control")
    
    writeLines(batData, batPath)
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}


#### Executing the Script ####
# Periodically update the main DAT files for PRMS and SRP 
# as well as the average historic precipitation estimates 

# The primary DAT files should be updated at the start of every new water year

# The historic precipitation estimates should be updated six months after the 
# of end of a water year (since PRISM data is provisional for six months)


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'HLP_008_Update_Main_DAT_and_Historic_Precip_Files.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Check the input files for these fields:
  #  (*) MAIN_PRMS_DAT_FOLDER
  #  (*) MAIN_SRP_DAT_FOLDER
  #  (*) PRISM_PRMS_HISTORIC_PRECIP_FOLDER
  #  (*) PRISM_SRP_HISTORIC_PRECIP_FOLDER
  
  # Look at the second year label in each of these filenames and make 
  # comparisons based on 'endDate' and today's date
  cat("\n[1/2]\tChecking files...\n")
  
  
  inputFiles <- tibble("PRMS_DAT" = 
                         getFromControl_RR("MAIN_PRMS_DAT_FOLDER") |>
                         getLatestFile("^DAT_PRMS_CY1990_to_WY[0-9]{4}\\.csv$", 
                                       "PRMS Main DAT File"),
                       "SRP_DAT" = 
                         getFromControl_RR("MAIN_SRP_DAT_FOLDER") |>
                         getLatestFile("^DAT_SRP_WY1948_to_WY[0-9]{4}\\.csv$", 
                                       "SRP Main DAT File"),
                       "PRMS_PRECIP" = 
                         getFromControl_RR("PRISM_PRMS_HISTORIC_PRECIP_FOLDER") |>
                         getLatestFile(paste0("^RR_Workflow_PRISM_PRMS_",
                                              "Avg_Historic_Precip_",
                                              "CY1981_to_WY[0-9]{4}\\.csv$"),
                                       "PRMS Historic Precip File"),
                       "SRP_PRECIP" = 
                         getFromControl_RR("PRISM_SRP_HISTORIC_PRECIP_FOLDER") |>
                         getLatestFile(paste0("^RR_Workflow_PRISM_SRP_",
                                              "Avg_Historic_Precip_",
                                              "CY1981_to_WY[0-9]{4}\\.csv$"),
                                       "SRP Historic Precip File"))
  
  
  # Extract the second year that appears in each of these filename strings
  # (They are all water years)
  endYears <- inputFiles |>
    map_dfr(~ str_extract(., "(?<=_to_WY)[0-9]{4}(?=\\.)") |> 
              as.numeric())
  
  # NOTE 
  # The above regular expression uses both "lookahead" and "lookbehind" patterns
  #  (*) Before a four-digit number, there should be "_to_WY"
  #  (*) After a four-digit number, there should be a "."
  
  # These lookahead and lookbehind patterns are not included in the extracted
  # string, so `as.numeric` can be applied immediately to the result
  
  
  cat("\tDone!\n\n")
  
  
  # The next step is to determine if updated files are required
  
  # The DAT files will be updated once every water year
  # (at the start of a new year)
  
  # The precipitation files will also be updated once every water year
  # (six months after the start of a new year)
  
  
  cat("[2/2]\tChecking if updated files are required...\n")
  
  
  # Start by checking the DAT files
  # If an update is required, use another function to perform those operations
  if (checkDAT(endDate, endYears$PRMS_DAT[1]) || checkDAT(endDate, endYears$SRP_DAT)) {
    
    message("New DAT files will now be generated!")
    cat("\n\n")
    
    
    # Create new PRMS and SRP DAT files
    updateDAT(startDate, endDate, inputFiles$PRMS_DAT[1], inputFiles$SRP_DAT[1])
    
  }
  
  
  # Check the precipitation files next
  # Generate new files if `checkPrecip` returns TRUE
  if (checkPrecip(endYears$PRMS_PRECIP[1]) || checkPrecip(endYears$SRP_PRECIP[1])) {
    
    message("New historic precipitation files will now be generated!")
    cat("\n\n")
    
    
    # Create new PRMS and SRP Historic Precipitation files
    updatePrecip(startDate, endDate, 
                 inputFiles$PRMS_PRECIP[1], inputFiles$SRP_PRECIP[1])
    
  }
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  "'HLP_008_Update_Main_DAT_and_Historic_Precip_Files.R' is complete!\n\n" |>
    col_green() |>
    cat()
  
  
  # Return nothing
  return(invisible(NULL))
  
}



checkDAT <- function (endDate, fileEndWY) {
  
  # Check the final water year included in the PRMS or SRP DAT files
  # These files should have data through the previous water year
  
  # If not, updates are needed (return TRUE in that case)
  # Otherwise, this function will return FALSE
  
  
  # Get the bounds of the modeled water year using 'endDate'
  wyBounds <- getModeledWY(endDate)
  
  
  # Get the water year from 'wyBounds'
  # The year of the end bound returned by `getModeledWY` is the same as  
  # the modeled water year
  modeledWY <- wyBounds[2] |> year()
  
  
  # If the gap between 'modeledWY' and 'fileEndWY' is MORE THAN 1,
  # updates are required
  
  # 'fileEndWY' should continue until the end of the water year prior to 
  # 'modeledWY' (e.g., 'modeledWY' - 'fileEndWY' = 2026 - 2025 = 1)
  
  # If the gap is larger than that, the file must be updated
  return((modeledWY - fileEndWY) > 1)
  
}



checkPrecip <- function (fileEndWY) {
  
  # Check the final water year included in the historic precipitation files
  
  # Compared to the scripts' run date, these files should extend until the 
  # previous water year
  
  # Unfortunately, PRISM data is not stable until six months have passed, 
  # so these updates must wait until later (i.e., after March)
  
  # Therefore, before April, these precipitation files are expected to extend
  # until two water years prior
  
  # (e.g., if it is August 2026, the files should extend until WY2025, but 
  #        if it's February 2026 instead, it's fine if the files only extend up 
  #        to WY2024)
  
  
  # Get the current water year
  # `getModeledWY` will return the start and end dates of the water year
  # The calendar year that appears in the end date is the same as the water year
  currentWY <- getModeledWY(Sys.Date())[2] |> year()
  
  
  # Next, the expected end year for the files is based on today's date
  
  # If today is later than March 31st, the expected end year of the file is
  # the previous water year
  
  # However, in the first half of the current water year, the file should 
  # be two water years prior, at most
  if (Sys.Date() <= as.Date(paste0(currentWY, "-03-31"), format = "%Y-%m-%d")) {
    
    # October - March
    expectedEndWY <- currentWY - 2
    
  } else {
    
    # April - September
    expectedEndWY <- currentWY - 1
    
  }
  
  
  # If 'fileEndWY' is less than 'expectedEndWY', updates are required
  # (return TRUE in that case) 
  return(fileEndWY < expectedEndWY)
  
}



updateDAT <- function (actualStart, actualEnd, latestPathPRMS, latestPathSRP) {
  
  # Create new long-running DAT files for PRMS and SRP
  
  # Temporarily change 'startDate' and 'endDate' in the control file 
  # to match the bounds of the DAT files
  
  # Then, use some of the workflow's scripts to download data and 
  # produce new files
  
  
  # The PRMS DAT file will start from 1990-01-01, while 
  # the SRP DAT file will start from 1947-10-01
  
  # However, the same downloaded data will be used in both cases
  # (from 1947-10-01 to the end of the previous WY)
  
  # That way, all weather scripts will be run only once
  # (Except in PRISM's case, both models' station inputs appear in the same file anyways)
  
  
  # Set the start date for data downloads
  dlStart <- as.Date("1947-10-01", format = "%Y-%m-%d")
  
  
  # Calculate the final date to include in these new files
  # (It will be the last day of the previous water year)
  fileEnd <- getModeledWY(actualEnd)[1] - days(1)
  
  
  # Download weather data using workflow scripts
  download_weather_data(dlStart, fileEnd, actualStart, actualEnd)
  
  
  # This raw data will need to be processed using the respective PRMS and SRP scripts 
  
  # That will be done in the next function, 
  # which also generates new DAT files for each model
  updateDAT_PRMS(as.Date("1990-01-01", format = "%Y-%m-%d"), fileEnd, dlStart, 
                 latestPathPRMS, actualStart, actualEnd)
  
  
  updateDAT_SRP(as.Date("1947-10-01", format = "%Y-%m-%d"), fileEnd, 
                latestPathSRP, actualStart, actualEnd)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



download_weather_data <- function (datStart, datEnd, actualStart, actualEnd) {
  
  # Use some of the workflow scripts to obtain weather data
  
  # This data will support the creation of both PRMS and SRP DAT files
  
  
  #  (*) Download PRISM data
  #      Using pieces of "RRW_001_PRISM_HTTP_Scraper.R"
  
  #  (*) Download NOAA data
  #      Using "RRW_002_NOAA_API_Scraper.R"
  
  #  (*) Download RAWS data
  #      Using "RRW_003_RAWS_HTTP_Scraper.R"
  
  #  (*) Download CIMIS data
  #      Using "RRW_004_CIMIS_API_Scraper.R"
  
  #  (*) Download CDEC data
  #      Using "RRW_005_CDEC_API_Scraper.R"
  
 
  # Start by downloading PRISM data for this date range ('datStart' to 'datEnd')
  # Download data for PRMS and SRP separately (the units will be different)
  runModifiedPRISM("PRISM_PRMS_STATIONS_CSV", datStart, datEnd,
                   paste0("W2_Russian_River/Intermediate/PRISM_PRMS_Data_", 
                          datStart, "_", datEnd, ".csv"),
                   useHighRes = TRUE, interpCells = TRUE,
                   getPrecip = TRUE, getTemp = TRUE, useMetric = TRUE)
  
  
  runModifiedPRISM("PRISM_SRP_STATIONS_CSV", datStart, datEnd,
                   paste0("W2_Russian_River/Intermediate/PRISM_SRP_Data_", 
                          datStart, "_", datEnd, ".csv"),
                   useHighRes = TRUE, interpCells = TRUE,
                   getPrecip = TRUE, getTemp = TRUE, useMetric = FALSE)
  
  
  # The next five scripts can be run in their entirety 
  
  # However, there are two important points:
  
  #  (1) 'startDate' and 'endDate' must be temporarily modified
  #      in "CTR_001_Set_Start_and_End_Dates.R"
  
  #  (2) The environment-clearing function calls (using `remove`) must be
  #      temporarily disabled
  
  
  # First, modify "CTR_001_Set_Start_and_End_Dates.R"
  # Replace the start and end dates
  updateControlScript(datStart, datEnd)
  
  
  # Then, prepare to execute the other weather download scripts
  
  
  # Request NOAA data
  toggleAndRunScript("W2_Russian_River/Scripts/RRW_002_NOAA_API_Scraper.R")
  
  
  # Run the RAWS script after that
  toggleAndRunScript("W2_Russian_River/Scripts/RRW_003_RAWS_HTTP_Scraper.R")
  
  
  # Then, get CIMIS data
  toggleAndRunScript("W2_Russian_River/Scripts/RRW_004_CIMIS_API_Scraper.R")
  
  
  # After that, get CDEC data
  toggleAndRunScript("W2_Russian_River/Scripts/RRW_005_CDEC_API_Scraper.R")
  
  
  # After running these scripts, revert the dates in the control script
  updateControlScript(actualStart, actualEnd)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



updateDAT_PRMS <- function (datStart, datEnd, dlStart, latestPathPRMS, 
                            actualStart, actualEnd) {
  
  # Generate a new DAT file for PRMS
  
  # In a prior function, the weather data was downloaded 
  # (from 'dlStart' to 'datEnd')
  
  # Now, some final processing and validation steps will occur
  
  # The format of the previous PRMS DAT file (located at 'latestPathPRMS')
  # will influence the final output's format
  
  
  # First, modify "CTR_001_Set_Start_and_End_Dates.R"
  # Replace the start and end dates
  updateControlScript(dlStart, datEnd)
  
  
  # Combine the downloaded weather data after that
  # Disable the `mainProcedure` call when this occurs
  # (Doing so just loads the functions into the current environment)
  toggleAndRunScript("W2_Russian_River/Scripts/RRW_007_Process_PRMS_Weather_Data.R", 
                     includeMainCall = TRUE)
  
  
  # Call `mainProcedure` for the script that combines PRMS weather data
  # But set 'archiveFiles' to FALSE (to prevent any archiving of model files)
  mainProcedure(archiveFiles = FALSE)
  
  
  # After running that script, revert the dates in the control script
  updateControlScript(actualStart, actualEnd)
  
  
  # Some final processing steps are required
  
  # Read in the processed weather file
  # Add columns for "YEAR", "MONTH", "DAY", "HOUR", "MINUTE", and "SECOND"
  newDAT <- paste0("W2_Russian_River/Output/PRMS_Meteorological_", dlStart, "_",
                   datEnd, ".csv") |>
    getFile() |>
    mutate(YEAR = year(DATE),
           MONTH = month(DATE),
           DAY = day(DATE),
           HOUR = 0,
           MINUTE = 0,
           SECOND = 0)
  
  
  # Make sure 'newDAT' covers just the range from 'datStart' to 'datEnd'
  # (since it initially contains data from 'dlStart' to 'datEnd' instead)
  newDAT <- newDAT |>
    filter(DATE >= datStart & DATE <= datEnd)
  
  
  # Add 22 runoff columns to 'newDAT' too
  # (All values will be 1)
  newDAT[paste0("RUNOFF", 1:22)] <- 1
  
  
  # Read in the previous PRMS DAT file
  # (Its column names will be used to check 'newDAT')
  oldPRMS <- getFile(latestPathPRMS)
  
  
  # Validate the newly assembled DAT file
  newDAT <- newDAT |>
    validateInputDAT(sourcePath = NA_character_, model = "PRMS",
                     modelCols = names(oldPRMS), startDate = datStart, 
                     endDate = datEnd, datType = "Final")
  
  
  # Prepare to write 'newDAT' to the same location as 'latestPathPRMS'
  
  # It will have a similar filename
  # However, the final WY portion will be updated
  
  # The new ending water year in the name will equal the year of 'datEnd'
  # (A water year is the same as the calendar year of its end bound)
  newPathPRMS <- latestPathPRMS |>
    str_replace("to_WY[0-9]{4}\\.csv",
                paste0("to_WY", year(datEnd), ".csv"))
  
  
  # Write 'newDAT' to a file
  writeOutput(newDAT |> select(all_of(names(oldPRMS))), 
              newPathPRMS)
  
  
  # Notify the user that a new primary DAT file for PRMS has been generated
  message("Created a new DAT file for PRMS!")
  cat("\n\n")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



updateDAT_SRP <- function (datStart, datEnd, latestPathSRP, 
                           actualStart, actualEnd) {
  
  # Generate a new DAT file for SRP
  
  # In a prior function, the weather data was downloaded 
  # (from 'datStart' to 'datEnd')
  
  # Now, some final processing and validation steps will occur
  
  
  # In this function:
  
  #  (*) Process the weather file
  #      Using pieces of "RRW_012_Process_SRP_Weather_Data.R"
  
  #  (*) Ensure that data for WY1948 to the end of CY1980 is represented
  #      using the data in 'latestPathSRP'
  
  
  # First, modify "CTR_001_Set_Start_and_End_Dates.R"
  # Replace the start and end dates
  updateControlScript(datStart, datEnd)
  
  
  # Combine the downloaded weather data after that
  # Disable the `mainProcedure` call when this occurs
  # (Doing so just loads the functions into the current environment)
  toggleAndRunScript("W2_Russian_River/Scripts/RRW_012_Process_SRP_Weather_Data.R", 
                     includeMainCall = TRUE)
  
  
  # Call `mainProcedure` for the script that combines SRP weather data
  # But set 'archiveFiles' to FALSE (to prevent any archiving of model files)
  mainProcedure(archiveFiles = FALSE)
  
  
  # After running that script, revert the dates in the control script
  updateControlScript(actualStart, actualEnd)
  
  
  # Import the processed weather file next
  # Add columns for "YEAR", "MONTH", "DAY", "HOUR", "MINUTE", and "SECOND"
  newDAT <- paste0("W2_Russian_River/Output/PRMS_Meteorological_", datStart, "_",
                   datEnd, ".csv") |>
    getFile()
  
  
  # Add columns for "YEAR", "MONTH", "DAY", "HOUR", "MINUTE", and "SECOND"
  newDAT <- newDAT |>
    mutate(YEAR = year(DATE),
           MONTH = month(DATE),
           DAY = day(DATE),
           HOUR = 0,
           MINUTE = 0,
           SECOND = 0)
  
  
  # Read in data from the previous SRP DAT file next
  oldSRP <- oldSRP <- getFile(latestPathSRP) |>
  mutate(DATE = paste0(YEAR, "-", MONTH, "-", DAY) |>
           as.Date(format = "%Y-%m-%d"))
  
  
  # If the earliest date in 'newDAT' is later than the start of 'oldSRP',
  # use the previous DAT file to fill in the gaps
  if (min(oldSRP$DATE) < min(newDAT$DATE)) {
    
    
    # Bind 'oldSRP' and 'newDAT'
    newDAT <- bind_rows(oldSRP |> filter(DATE < min(newDAT$DATE)), 
                        newDAT)
    
  }
  
  
  # Validate the new result
  newDAT <- newDAT |>
    validateInputDAT(NA_character_, "SRP",
                     names(oldSRP), datStart, datEnd, datType = "Final")
  
  
  # Prepare to write 'newDAT' to the same location as 'latestPathSRP'
  
  # It will have a similar filename
  # However, the final WY portion will be updated
  
  # The new ending water year in the name will equal the year of 'datEnd'
  # (A water year is the same as the calendar year of its end bound)
  newPathSRP <- latestPathSRP |>
    str_replace("to_WY[0-9]{4}\\.csv",
                paste0("to_WY", year(datEnd), ".csv"))
  
  
  # Write 'newDAT' to a file
  newDAT |>
    select(all_of(names(oldSRP))) |>
    writeOutput(newPathSRP)
  
  
  # Notify the user that a new DAT file for SRP has been generated
  message("Created a new DAT file for SRP!")
  cat("\n\n")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



updatePrecip <- function (actualStart, actualEnd, latestPathPRMS, latestPathSRP) {
  
  # Create new long-running historic precipitation files for PRMS and SRP
  
  # Temporarily change 'startDate' and 'endDate' in the control file 
  # to match the bounds of the precipitation files
  
  # Then, use some of the workflow's functions to download data and 
  # produce new files
  
  
  # Both files will download data from 1981-01-01 onwards
  # (this is the earliest date with data from PRISM)
  prismStart <- as.Date("1981-01-01", format = "%Y-%m-%d")
  
  
  # Calculate the final date to include in these new files
  # (It will be the last day of the previous water year)
  fileEnd <- getModeledWY(actualEnd)[1] - days(1)
  
  
  # For these edits, temporarily modify "CTR_001_Set_Start_and_End_Dates.R"
  # Replace the start and end dates with 'prismStart' and 'fileEnd'
  updateControlScript(prismStart, fileEnd)
  
  
  # Generate new precipitation files for each model in separate function calls
  createPrecipFile(prismStart, fileEnd, model = "PRMS",
                   latestPathPRMS, actualEnd)
  
  
  createPrecipFile(prismStart, fileEnd, model = "SRP",
                   latestPathSRP, actualEnd)
  
  
  # Restore the dates in "CTR_001_Set_Start_and_End_Dates.R" afterwards
  updateControlScript(actualStart, actualEnd)
  
  
  # Notify the user about the status of the procedure
  message("Created new historic precipitation files for PRMS and SRP!")
  cat("\n\n")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



createPrecipFile <- function (precipStart, precipEnd, model = "PRMS",
                              oldPrecipPath, actualEnd) {
  
  
  # Download PRISM data from 'precipStart' to 'precipEnd'
  
  # Calculate averages across all grid cells for every day in the dataset
  
  # Use the filepath of the previous precipitation file to create a new path
  # for this updated dataset
  
  # Based on the model ("PRMS" or "SRP"), the inputs and names are different
  
  
  # Define the path that will contain the downloaded PRISM data
  prismPath <- paste0("W2_Russian_River/Intermediate/PRISM_", model, 
                      "_Domain_Data_", precipStart, 
                      "_", precipEnd, ".csv")
  
  
  # Then, download PRISM data for the model domain grid cells
  runModifiedPRISM(paste0("PRISM_", model, "_GRID_CELLS_CSV"), 
                   precipStart, precipEnd, prismPath,
                   useHighRes = TRUE, interpCells = FALSE,
                   getPrecip = TRUE, getTemp = FALSE, useMetric = TRUE)
  
  
  # Read in this file
  precipDF <- prismPath |>
    getPRISM()
  
  
  # Validate the file
  # The validation function expects both precipitation and temperature,
  # so include dummy columns for "TMIN" and "TMAX" when checking the data
  precipDF |>
    mutate(`tmin (degrees C)` = 0, `tmax (degrees C)` = 0) |>
    validateWebData(dataSource = "PRISM",
                    inputPath = prismPath,
                    stationVec = precipDF$Name |> unique(),
                    siPRISM = TRUE)
  
  
  # For each day in 'precipDF', calculate the average precipitation across 
  # all grid cells in the model domain
  precipDF <- precipDF |>
    group_by(Date) |>
    summarize(`ppt (mm)` = mean(`ppt (mm)`), .groups = "drop")
  
  
  # Validate the result
  precipDF |> 
    validateHistoricPrecipFile(prismPath,
                               getModeledWY(actualEnd)[1])
  
  
  # Write 'precipDF' to a file next
  # Use 'oldPrecipPath' as a base for this filename
  
  # However, the final WY portion will be updated
  
  # The new ending water year in the name will equal the year of 'precipEnd'
  # (A water year is the same as the calendar year of its end bound)
  newPrecipPath <- oldPrecipPath |>
    str_replace("to_WY[0-9]{4}\\.csv",
                paste0("to_WY", year(precipEnd), ".csv"))
  
  
  # Write 'precipDF' to a file
  writeOutput(precipDF, newPrecipPath)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



runModifiedPRISM <- function (sourceName, startDate, endDate, outFile,
                              useHighRes = TRUE, interpCells = TRUE,
                              getPrecip = TRUE, getTemp = TRUE, useMetric = TRUE) {
  
  # Use functions in "RRW_001_PRISM_HTTP_Scraper.R" to run a modified process
  
  # The entire script is NOT used because it downloads data for both PRMS and SRP
  # DAT files and precipitation data
  
  # The data download ranges will differ for these models and files, 
  # so it is better not to use the entire script
  
  
  # Import functions from "RRW_001_PRISM_HTTP_Scraper.R" using `functionStealer`
  c("scrapePRISM", "validateReqResults", "splitRequest", "combineRawOutputs") |>
    map(~ functionStealer("W2_Russian_River/Scripts/RRW_001_PRISM_HTTP_Scraper.R", .))
  
  
  # Read in a list of stations or grid cells for a particular model
  stationDF <- getFromControl_RR(sourceName) |>
    getFile() |>
    unique()
  
  
  # Perform data validation on 'stationDF' next
  validateStationInputFile(stationDF, sourceName, "PRISM")
  
  
  # Call `scrapePRISM` after that
  scrapePRISM(stationDF, startDate, endDate, outFile,
              useHighRes = useHighRes, interpCells = interpCells,
              getPrecip = getPrecip, getTemp = getTemp, useMetric = useMetric)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



updateControlScript <- function (newStart, newEnd) {
  
  # Update 'startDate' and 'endDate' in the control script
  # "CTR_001_Set_Start_and_End_Dates.R"
  
  # Replace the dates with new values ('newStart' and 'newEnd')
  
  # NOTE
  # These two new values should be date variables
  # This script will convert them into strings with the proper formatting
  
  
  # This is the path to the control script
  scriptPath <- "W2_Russian_River/Scripts/CTR_001_Set_Start_and_End_Dates.R"
  
  
  # Read in the text of that file
  ctrlFile <- getFile(scriptPath, fileType = "OTHER")
  
  
  # Update 'startDate' first
  ctrlFile <- ctrlFile |>
    findAndReplace("^\\s*startDate <-",
                   paste0("startDate <- \"", format(newStart, "%Y-%m-%d"), "\""))
  
  
  # Then update 'endDate'
  ctrlFile <- ctrlFile |>
    findAndReplace("^\\s*endDate <-",
                   paste0("endDate <- \"", format(newEnd, "%Y-%m-%d"), "\""))
  
  
  # Write 'ctrlFile' back to its original location
  writeOutput(ctrlFile, scriptPath, writeFunction = "write_lines")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



findAndReplace <- function (vec, pattern, replacement) {
  
  # Look for a line in 'vec' using 'pattern'
  # There should be exactly one match for 'pattern'
  
  # After that, replace that line in 'vec' with 'replacement'
  
  # Then, return 'vec'
  
  
  # Apply 'pattern' to 'vec' to find a match
  # (Exactly one match should be returned)
  matchIndex <- find_matches(vec, pattern, minMatches = 1, maxMatches = 1)
  
  
  # If only one match was found, update that location with 'replacement'
  vec[matchIndex] <- replacement
  
  
  # Return 'vec' after this change
  return(vec)
  
}



toggleAndRunScript <- function (scriptPath, includeMainCall = FALSE) {
  
  # In the text of another script, temporarily disable the `remove` function calls
  # (and possibly the `mainProcedure` call too)
  
  # Then, run a different script
  
  # After that, enable the `remove` function calls again (along with `mainProcedure`)
  
  
  # Disable `base::remove(list = ls())`
  toggleRemoveFunctions(scriptPath, commentOut = TRUE)
  
  
  # If 'includeMainCall' is TRUE, toggle `mainProcedure` too
  if (includeMainCall) {
    toggleMainFunctionCall(scriptPath, commentOut = TRUE)
  }
  
  
  # Run the script
  source(scriptPath)
  
  
  # Enable `base::remove(list = ls())`
  toggleRemoveFunctions(scriptPath, commentOut = FALSE)
  
  
  # If 'includeMainCall' is TRUE, restore the call to `mainProcedure` 
  if (includeMainCall) {
    toggleMainFunctionCall(scriptPath, commentOut = FALSE)
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



toggleRemoveFunctions <- function (scriptPath, commentOut = TRUE) {
  
  # Edit a script to either comment or uncomment the lines that 
  # clear the working environment
  
  # This script searches for lines that contain:
  # "base::remove(list = ls())"
  
  # If 'commentOut' is TRUE, add "#" to the beginning of these lines
  
  # If 'commentOut' is FALSE, remove "#" from the beginning of these lines
  
  
  # Read the lines of 'scriptPath'
  scriptVec <- getFile(scriptPath, fileType = "OTHER")
  
  
  # Get the lines that clear the environment
  matchLines <- find_matches(scriptVec, "^[ #]*base::remove\\(list = ls\\(\\)\\)\\s*$", 
                             minMatches = 1, maxMatches = Inf, filePath = scriptPath)
  
  # This regular expression checks for lines that:
  
  #  (*) Optionally start with one or more spaces and/or comment hashtags
  #  (*) Contains "base::remove(list = ls())"
  #  (*) Optionally end with one or more spaces
  
  
  # If 'commentOut' is TRUE, set those lines with a "#" at the beginning
  # Otherwise, set those lines without any "#" at the beginning
  if (commentOut) {
    
    # Make sure these removal lines are commented out
    scriptVec[matchLines] <- paste0("# ", scriptVec[matchLines])
    
  } else {
    
    # Make sure these removal lines are NOT commented out
    scriptVec[matchLines] <- scriptVec[matchLines] |>
      str_remove("^[ #]*")
    
  }
  
  
  # Write 'scriptVec' back to its file
  writeOutput(scriptVec, scriptPath, "write_lines", quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



toggleMainFunctionCall <- function (scriptPath, commentOut = TRUE, 
                                    mainName = "mainProcedure") {
  
  # Edit a script to either comment or uncomment the line that 
  # executed the primary function (commonly called "mainProcedure")
  
  # This script searches for lines that contain:
  # 'mainName' followed by an left parenthesis "("
  
  # If 'commentOut' is TRUE, add "#" to the beginning of these lines
  
  # If 'commentOut' is FALSE, remove "#" from the beginning of these lines
  
  
  # Read the lines of 'scriptPath'
  scriptVec <- getFile(scriptPath, fileType = "OTHER")
  
  
  # Get the lines that clear the environment
  matchLines <- mainName |>
    str_escape() |>
    paste0("^[ #]*", ... = _, "\\(") |>
    find_matches(scriptVec, pattern = _, 
                 minMatches = 1, maxMatches = 1, filePath = scriptPath)
  
  # This regular expression checks for lines that:
  
  #  (*) Optionally start with one or more spaces and/or comment hashtags
  #  (*) Contains the function name 'mainName' (e.g., "mainProcedure")
  #  (*) Followed by one opening parenthesis "("
  
  
  # If 'commentOut' is TRUE, set those lines with a "#" at the beginning
  # Otherwise, set those lines without any "#" at the beginning
  if (commentOut) {
    
    # Make sure these removal lines are commented out
    scriptVec[matchLines] <- paste0("# ", scriptVec[matchLines])
    
  } else {
    
    # Make sure these removal lines are NOT commented out
    scriptVec[matchLines] <- scriptVec[matchLines] |>
      str_remove("^[ #]*")
    
  }
  
  
  # Write 'scriptVec' back to its file
  writeOutput(scriptVec, scriptPath, "write_lines", quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

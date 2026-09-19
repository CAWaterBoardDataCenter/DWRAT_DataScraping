# For each watershed in the weather control file, 
# several manual review spreadsheets were recently generated

# This script adjusts them to support data archiving efforts

# In addition, if the user has specified a folder path for the field
# "PRIOR_MANUAL_REVIEW_SHEETS_LOCATION", the script will attempt to integrate 
# manual review edits from spreadsheets stored within that folder 


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
  cat("Starting 'LSPC_013_Adjust_Manual_Review_Sheets.R'!\n")
  
  
  # Import functions from other scripts
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R",
                  "read_all_lspc_project_control")
  
  
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_012_Archive_Raw_and_Staged_Files.R",
                  "get_lspc_archive_folder")
  
  
  # Import the data scraping bounds next
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Load in the LSPC weather control file too
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validation of control file
  
  
  # Use the imported function to get storage information for every watershed
  wsDir <- controlDF |>
    read_all_lspc_project_control(worksheet = "Storage")
  
  # To Do: Validation of storage worksheets
  
  
  # Import the location of the archive folder after that
  dirPath <- get_lspc_archive_folder(startDate, endDate)
  
  
  # Finally, try to read in the optional "PRIOR_MANUAL_REVIEW_SHEETS_LOCATION" field
  reviewPath <- get_from_lspc_master_control("PRIOR_MANUAL_REVIEW_SHEETS_LOCATION")
  
  
  # If this field is not empty, check whether the folder path is 
  # a cloud path or local path
  if (!is.na(reviewPath)) {
    
    reviewPath <- reviewPath |>
      sharepointPathCheck(isFolder = TRUE)
    
  }
  
  
  # Get a list of manual review spreadsheets
  cat("\n[1/3]\tGathering file paths...\n")
  
  
  # Get the paths to the "QCSpreadsheets" directory for each watershed
  qcDir <- wsDir |>
    map_chr(~ paste0("W3_LSPC_Watershed/",
                     filter(., scope == "project" & level == "root") |>
                       select(path) |> unlist(use.names = FALSE),
                     "/",
                     filter(., scope == "project" & level == "candidate" & source == "gage") |>
                       select(path) |> unlist(use.names = FALSE),
                     "/QCSpreadsheets"))
  
  
  # Confirm that each QAQC directory exists
  error_if(!all(dir.exists(qcDir)),
           paste0("Could Not Locate QC Folders\n\n",
                  "Each watershed should have a directory within their ",
                  "project folder that stores manual review spreadsheets ",
                  "for \"candidate\" gage data. However, this is not the case for ", 
                  controlDF$project_name[!dir.exists(qcDir)] |> vec2QuotedStr(),
                  ". Please investigate."))
  
  
  # Get the file paths to the manual review spreadsheets
  # (Ignore the NOAA 100 year storm CSV file)
  qcFiles <- qcDir |>
    map(~ list.files(.) |>
          str_subset("NOAA_100", negate = TRUE))
  
  # 'qcFiles' will be a list, with each element containing the file paths
  # to each watershed's QC spreadsheets
  
  
  # Confirm that all watersheds have the expected files
  reqFiles <- c("Ground_stations_Flagged_1_2_3.xlsx",
                "Flag4_Monthly_Only_CompleteData.xlsx")
  
  
  error_if(!all(qcFiles |> map_lgl(~ all(reqFiles %in% .))),
           
           paste0("Each watershed is expected to have ", length(reqFiles), " ",
                  "QC spreadsheets. However, this was not the case for ",
                  controlDF$project_name[which(qcFiles |> map_lgl(~ !all(reqFiles %in% .)))],
                  ". Please investigate."))
  
  
  cat("\tDone!\n\n")
  
  
  # The next step will be to iterate through each of these spreadsheets
  
  # Some adjustments will be made to aid with the manual review process
  
  # In addition, the unedited QC flag worksheets will be duplicated
  # This will preserve a record of what entries were flagged and 
  # what values were present or missing when the review was performed
  
  
  cat("[2/3]\tAdjusting manual review spreadsheets...\n\n")
  
  
  # All archive worksheets will use this name in the spreadsheet
  worksheetArchiveName <- "QC_Archive_DO_NOT_DELETE"
  
  
  # Iterate through each watershed, and check these files
  # Perform some updates on them to support data archiving and faster reviews
  for (i in 1:nrow(controlDF)) {
    
    # Read in the QC workbooks and duplicate their worksheets 
    # (The duplicate will be preserved and remain unchanged)
    # Also, try to incorporate previous manual review decisions
    for (j in 1:length(qcFiles[[i]])) {
      
      # Get the path to the spreadsheet
      sheetPath <- paste0(qcDir[i], "/", qcFiles[[i]][j])
      
      
      # Load in the workbook
      wb <- sheetPath |> 
        wb_load()
      
      
      # Typically, these spreadsheets should have just one worksheet
      # (with the QC flags)
      
      
      # But, just in case there are multiple worksheets for some reason, 
      # only the first sheet will be duplicated, 
      # and users should be made aware of that
      
      
      # Get the worksheet names
      wbNames <- wb_get_sheet_names(wb)
      
      
      # If there's multiple worksheets,
      # arbitrarily assume that the first value in this vector is the QC worksheet
      if (length(wbNames) > 1) {
        
        # Warn users about this 
        cat("\n\n")
        paste0("Warning: Assuming that the \"", wbNames[1], "\" worksheet in \"", 
               sheetPath, "\" contains the QC flags! It will be duplicated for ",
               "archival purposes!") |>
          errWrap() |>
          message()
        cat("\n\n")
        
        
        # Then select the first worksheet
        wbNames <- wbNames[1]
        
      }
      
      
      # Before proceeding forward, perform some data validation
      
      
      # To Do: Validate worksheet
      if (qcFiles[[i]][j] %in% "Ground_stations_Flagged_1_2_3.xlsx") {
        
        stopifnot("Date" %in% names(wb_to_df(wb, sheet = wbNames[1])))
        
      } else if (qcFiles[[i]][j] %in% "Flag4_Monthly_Only_CompleteData.xlsx") {
        
        stopifnot("Year" %in% names(wb_to_df(wb, sheet = wbNames[1])))
        stopifnot("Month" %in% names(wb_to_df(wb, sheet = wbNames[1])))
        
      }
      
      
      # Duplicate the first worksheet in 'wb' to create the archive sheet
      wb <- wb_clone_worksheet(wb, old = wbNames[1], new = worksheetArchiveName)
      
      
      # Then, protect that archive worksheet to deter edits
      wb <- wb_protect_worksheet(wb, worksheetArchiveName, password = "archive")
      
      
      # After that, if 'reviewPath' is not NA, try and see if a corresponding
      # manual review spreadsheet is stored there
      # If yes, try to incorporate previous review efforts into this file
      wb <- wb_incorporate_old_review(wb, reviewPath, controlDF$project_name[i], 
                                      qcFiles[[i]][j], wbNames[1], 
                                      worksheetArchiveName, startDate)
      
      
      # Finally, write 'wb' back to its file
      wb_save(wb, sheetPath, overwrite = TRUE)
      
    } # End of loop through watershed's QC files
    
  } # End of loop through watersheds
  
  
  cat("\tDone!\n\n")
  
  
  # With this step completed, Part 1 of the workflow is essentially complete
  # Update the metadata file to note the time when Part 1 of the workflow was finished
  
  cat("[3/3]\tUpdating metadata...\n\n")
  
  
  # Record the current time as the approximate time when Part 1 of the workflow concluded
  updateMetadataCSV(dirPath, list("APPROX_PART_1_COMPLETION_TIME" = Sys.Date()))
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_013_Adjust_Manual_Review_Sheets.R' is complete!\n\n"))
  
  
  # After the script completion message, inform the user of the next steps
  paste0("Manual review spreadsheets have been generated for each watershed. ",
         "They are located within each project watershed's respective QC folder ",
         "(e.g., \"", qcDir[[1]], "\").\n\n",
         "Please review the two spreadsheets that contain QC Flags 1, 2, 3, and 4.",
         "Delete the entries of values that should be removed. Later scripts ",
         "will fill in all blank entries with data from PRISM.") |>
    errWrap() |>
    cat()
  cat("\n\n")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



wb_incorporate_old_review <- function (wb, reviewPath, wsName, qcFile, worksheet, 
                                       worksheetArchiveName, startDate) {
  
  # If 'reviewPath' is a valid folder path, try to locate an equivalent manual
  # review spreadsheet like 'qcFile' for the watershed denoted by 'wsName'
  
  # If a file is found, check that spreadsheet for a review worksheet and 
  # an archive worksheet
  
  # Compare those two worksheets to identify previous manual review corrections
  
  # Then, for dates before 'startDate', incorporate those changes into the 
  # new manual review spreadsheet
  
  
  # If 'reviewPath' is NA, return 'wb' without any changes
  if (is.na(reviewPath)) {
    
    return(wb)
    
  }
  
  
  # If 'reviewPath' contains a folder path, 
  # confirm that the watershed has a project folder
  if (wsName %notin% list.files(reviewPath)) {
    
    # Post a warning before returning 'wb'
    cat("\n\n")
    paste0("Warning: The manual review spreadsheet directory \"", reviewPath,
           "\" does not contain a folder titled \"", wsName, "\"! No prior ",
           "review sheets can be incorporated into this watershed's QC files!") |>
      errWrap() |>
      message()
    cat("\n\n")
    
    
    return(wb)
    
  }
  
  
  # Define the expected path for 'qcFile'
  # Check if it exists within 'reviewPath'
  priorPath <- paste0(reviewPath, "/", wsName, "/", qcFile) |>
    normalizePath(mustWork = FALSE)
  
  
  # If a file equivalent to 'qcFile' is not present in the folder for 'wsName',
  # return 'wb' without any changes 
  if (!file.exists(priorPath)) {
    
    # Post a warning again
    cat("\n\n")
    paste0("Warning: The manual review spreadsheet directory \"", reviewPath,
           "\" does not contain a spreadsheet titled \"", qcFile, "\" in its ", 
           "\"", wsName, "\" folder! No review sheet can be incorporated into ",
           "this watershed's version of \"", qcFile, "\"!") |>
      errWrap() |>
      message()
    cat("\n\n")
    
    
    return(wb)
    
  }
  
  
  # If 'priorPath' does exist, try to read it in 
  priorWB <- wb_load(priorPath)
  
  
  
  
  
  
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

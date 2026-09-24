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
  
  
  # If this field is not empty, 
  # check whether the folder path is a cloud path or local path
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
  
  
  # If the temporary file name "~$[NAME].xlsx" appears in 'qcFiles',
  # output an error message
  error_if(any(paste0("~$", reqFiles) %in% unlist(qcFiles)),
           
           paste0("Spreadsheet Opened by User\n\n",
                  "One or more of the QC spreadsheets is currently open. Please ",
                  "close those files before proceeding."))
  
  
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
    
    paste0("\t\t[", i, "/", nrow(controlDF), "]\t", controlDF$project_name[i], "\n\n") |>
      cat()
    
    
    # Read in the QC workbooks and duplicate their worksheets 
    # (The duplicate will be preserved and remain unchanged)
    # Also, try to incorporate previous manual review decisions
    for (j in 1:length(qcFiles[[i]])) {
      
      paste0("\t\t\t[", j, "/", length(qcFiles[[i]]), "]\t", qcFiles[[i]][j], "\n\n") |>
        cat()
      
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
      
      
      # To Do: Validate QC worksheets
      wb_to_df(wb, sheet = wbNames[1]) |>
        check_for_date_cols(qcFiles[[i]][j])
      
      
      # Duplicate the first worksheet in 'wb' to create the archive sheet
      wb <- wb_clone_worksheet(wb, old = wbNames[1], new = worksheetArchiveName)
      
      
      # Then, protect that archive worksheet to deter edits
      wb <- wb_protect_worksheet(wb, worksheetArchiveName, password = worksheetArchiveName)
      
      # (The password to unlock the worksheet is the exact same string as its name)
      
      
      # After that, if 'reviewPath' is not NA, try and see if a corresponding
      # manual review spreadsheet is stored there
      # If yes, try to incorporate previous review efforts into this file
      wb <- wb_incorporate_old_review(wb, reviewPath, controlDF$project_name[i], 
                                      qcFiles[[i]][j], wbNames[1], 
                                      worksheetArchiveName, startDate)
      
      
      # Then, add plots of the gage data to a new worksheet in the file
      wb <- wb_add_gage_charts(wb, wbNames[1], qcFiles[[i]][j])
      
      
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
  paste0("\nManual review spreadsheets have been generated for each watershed. ",
         "They are located within each project watershed's respective QC folder ",
         "(e.g., \"", qcDir[[1]], "\").\n\n",
         "Please review the two spreadsheets that contain QC Flags 1, 2, 3, and 4. ",
         "Delete the entries of values that should be removed. Later scripts ",
         "will fill in all blank entries with data from PRISM.") |>
    errWrap() |>
    cat()
  cat("\n\n")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



check_for_date_cols <- function (df, filePath) {
  
  # Given a QC worksheet (read in as a data frame) and its filepath,
  # confirm that it has the proper date-related columns
  
  
  # Check if the column(s) are missing
  df |>
    checkMissingCol(colNames = qc_expected_date_colnames(filePath))
  
  # (The list of expected date columns for a given QC worksheet is specified
  #  in the function `qc_expected_date_colnames`)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



qc_expected_date_colnames <- function (filePath) {
  
  # For a specific LSPC QC spreadsheet, check if it has the correct date columns
  
  # Depending on the specific spreadsheet, the date may be stored in a different format
  # or split across column names
  
  # This function will return a vector containing the expected column names
  # that are related to the date
  
  
  if (filePath %in% "Ground_stations_Flagged_1_2_3.xlsx") {
    
    return("Date")
    
  } else if (filePath %in% "Flag4_Monthly_Only_CompleteData.xlsx") {
    
    return(c("Year", "Month"))
    
  } else {
    
    paste0("Unrecognized File Name\n\n",
           "The file \"", filePath, "\" does not have any information specified ",
           "in this function. Its expected date-related columns are unknown. ",
           "Please revise the script.") |>
      stop_script()
    
  }
  
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
  
  
  # Confirm that 'priorWB' contains worksheets with the same name as 'wb'
  # (i.e., it should have 'worksheet' and 'worksheetArchiveName')
  if (!all(c(worksheet, worksheetArchiveName) %in% wb_get_sheet_names(priorWB))) {
    
    # Post a warning and return 'wb' without any edits
    cat("\n\n")
    paste0("Warning: In \"", reviewPath, "\", the spreadsheet \"", qcFile, 
           "\" for \"", wsName, "\" has two worksheets: ", 
           c(worksheet, worksheetArchiveName) |> vec2QuotedStr(),
           ". The folder that contains prior manual review spreadsheets has the ",
           "same file, but it does not have both of these worksheets.\n\n",
           "These worksheets are required for incorporating past review decisions ",
           "into the newer spreadsheets. Please investigate.") |>
      errWrap() |>
      message()
    cat("\n\n")
    
    
    return(wb)
    
  }
  
  
  # Once all of these initial checks have been passed, 
  # more checks will be performed
  
  # But before that, the actual spreadsheet tables should be read in
  
  
  # Load in the new manual review spreadsheet's data
  newReview <- wb_to_df(wb, worksheet)
  
  
  # Read in the edited and unedited versions of the prior review sheet too
  priorEdited <- wb_to_df(priorWB, worksheet)
  
  priorArchive <- wb_to_df(priorWB, worksheetArchiveName)
  
  
  # To Do: Validate QC worksheets
  newReview |>
    check_for_date_cols(qcFile)
  
  priorEdited |>
    check_for_date_cols(qcFile)
  
  priorArchive |>
    check_for_date_cols(qcFile)
  
  
  # Confirm that 'priorEdited' and 'priorArchive' have the same dimensions and columns
  error_if(nrow(priorEdited) != nrow(priorArchive) || 
             ncol(priorEdited) != ncol(priorEdited) ||
             !all(names(priorEdited) == names(priorArchive)),
           
           paste0("Worksheet Tables Do Not Match\n\n",
                  "In \"", reviewPath, "\", the old manual review spreadsheet ",
                  "for \"", wsName, "\" (\"", qcFile, "\") should have two worksheets: ",
                  c(worksheet, worksheetArchiveName) |> vec2QuotedStr(), ".\n\n",
                  "These two worksheets should be identical in size and format. ",
                  "The only major difference should be that that a manual ",
                  "review was performed and some values were deleted in \"", 
                  worksheet, "\". However, the two worksheets do not match. ",
                  "Please investigate."))
  
  
  # For consistency, regardless of the spreadsheet type, temporarily add a "DATE" column
  # (Sometimes, there's already a "Date" column)
  # (Alternatively, there's a "Year" and "Month" column, with some merged cell issues)
  # This will ensure that all worksheets can consistently reference dates through the same way
  newReview <- newReview |>
    add_date_column()
  
  priorEdited <- priorEdited |>
    add_date_column()
  
  priorArchive <- priorArchive |>
    add_date_column()
  
  
  # After these checks have been completed, the actual procedure can begin
  
  # Identify locations where edits occurred to 'priorEdited'
  # (These will be entries where 'priorArchive' has a value, but 'priorEdited' does not)
  
  # If the corresponding value in 'newReview' matches the value in 'priorArchive',
  # delete that entry so that it matches 'priorEdited' 
  
  
  # First filter 'priorEdited' and 'priorArchive' to dates before 'startDate'
  # (The user should review any newly downloaded data)
  priorEdited <- priorEdited |>
    filter(DATE < startDate)
  
  
  priorArchive <- priorArchive |>
    filter(DATE < startDate)
  
  
  # Find all locations where 'priorEdited' is NA, while 'priorArchive' is not
  deletedLocs <- which(is.na(priorEdited) & !is.na(priorArchive), arr.ind = TRUE) |>
    data.frame()
  
  # 'deletedLocs' will initially contain a matrix with columns called "row" and "col"
  
  # These indicate the locations in 'priorArchive' and 'priorEdit' 
  # where values were deleted
  
  # 'deletedLocs' is then converted into a data frame 
  
  
  # If 'deletedLocs' is empty, return 'wb' without any changes
  if (length(deletedLocs) == 0) {
    return(wb)
  }
  
  
  # Otherwise, iterate through each edit and apply similar changes to 'newReview'
  for (i in 1:nrow(deletedLocs)) {
    
    # First, locate the corresponding entry in 'newReview'
    
    # Check if the column name in 'priorArchive' can be found in 'newReview'
    
    # Extract the column name that corresponds to this iteration's edit
    priorName <- names(priorArchive)[deletedLocs$col[i]]
    
    
    # If it's not present in 'newReview', skip to the next edit
    if (priorName %notin% names(newReview)) {
      
      # Notify the user
      cat("\n\n")
      paste0("\t\t\t\tSkipping edit to \"", priorName, "\" (not present in the ",
             "new manual review spreadsheet)") |>
        cat()
      cat("\n\n")
      
      next
      
    }
    
    
    # After that, find the corresponding entry in 'newReview' that matches 
    # 'priorName' and the row's date
    newRow <- which(newReview$DATE == priorArchive$DATE[deletedLocs$row[i]])
    
    
    # If this date does not appear in 'newReview', notify the user and skip it
    if (length(newRow) == 0) {
      
      # Send a message to the user
      cat("\n\n")
      paste0("\t\t\t\tSkipping edit to \"", priorName, "\" for ", 
             priorArchive$DATE[deletedLocs$row[i]], " (date not present in ",
             "the new review spreadsheet)") |>
        cat()
      cat("\n\n")
      
      next
      
    }
    
    
    # 'newRow' should also not have more than one index
    # (Each date should only appear once within 'newReview')
    error_if(length(newRow) > 1,
             
             paste0("Spreadsheet Error\n\n",
                    "The \"", qcFile, "\" manual review spreadsheet for ",
                    wsName, " has the same date (", priorArchive$DATE[deletedLocs$row[i]],
                    ") more than once in its file. Please investigate."))
    
    
    # After that, compare the values for this gage and date
    # If 'priorArchive' and 'newReview' have different values, do not edit 'newReview'
    if (round(priorArchive[deletedLocs$row[i], deletedLocs$col[i]], digits = 3) !=
        round(newReview[[priorName]][newRow], digits = 3)) {
      
      # Send a message to the user
      cat("\n\n")
      paste0("\t\t\t\tSkipping edit to \"", priorName, "\" for ", 
             priorArchive$DATE[deletedLocs$row[i]], " (the value has changed ",
             "since the prior review was completed)") |>
        cat()
      cat("\n\n")
      
      next
      
    }
    
    
    # Since the same gage, date, and value are all present in 'newReview',
    # delete the entry in the new review sheet to match 'priorEdited'
    newReview[[priorName]][newRow] <- NA_real_
    
  }
  
  
  # Once 'newReview' has been updated with the edits in the prior manual review,
  # save these changes to 'wb'
  
  
  # First, remove the "DATE" column for consistency
  newReview <- newReview |>
    select(-DATE)
  
  
  # Then, write it to 'wb'
  wb <- wb_add_data(wb, sheet = worksheet, newReview, na = NULL)
  
  
  # Include 'priorEdited' and 'priorArchive' in this workbook too
  
  # They will need their own unique worksheet names too
  editedWorksheet <- "Prior_Review"
  archivedWorksheet <- "Prior_Review_Archive"
  
  
  wb <- wb_add_worksheet(wb, editedWorksheet)
  wb <- wb_add_worksheet(wb, archivedWorksheet)
  
  
  # Write the data frames to these worksheets
  wb <- wb_add_data(wb, editedWorksheet, priorEdited, na = NULL)
  
  wb <- wb_add_data(wb, archivedWorksheet, priorArchive, na = NULL)
  
  
  # Lock the two worksheets too
  wb <- wb_protect_worksheet(wb, editedWorksheet, password = worksheetArchiveName)
  
  wb <- wb_protect_worksheet(wb, archivedWorksheet, password = worksheetArchiveName)
  
  # (The password again is the name of the new review's archive worksheet)
  
  
  # Return 'wb' afterwards
  return(wb)
  
}



add_date_column <- function (qcDF) {
  
  # Some QAQC worksheets have a "Date" column, while others use "Year" and "Month"
  
  # For consistency, create a "DATE" column in all data frames
  
  # In addition, for worksheets that have "Year" and "Month" columns, 
  # there may be NA values if merged cells were used in the spreadsheet
  
  # This script will fill in those missing values too
  
  
  # Start by checking if "Date" already exists in 'qcDF'
  # In that case, just define "DATE" to match "Date" and return 'qcDF'
  if ("Date" %in% names(qcDF)) {
    
    return(qcDF |>
             mutate(DATE = Date))
    
  }
  
  
  # Output an error message if 'qcDF' lacks "Year" and "Month" columns
  error_if(any(c("Year", "Month") %notin% names(qcDF)),
           
           paste0("Unrecognized Date Names\n\n",
                  "A QC worksheet is expected to have either \"Date\" or ",
                  "the pair \"Year\" and \"Month\" in its table. These names ",
                  "must match exactly. However, one or more of these column ",
                  "names were not found in the table.\n\n",
                  "The names that were found instead are: ",
                  names(qcDF) |> vec2QuotedStr(), "\n\n",
                  "Please investigate.\n\n"))
  
  
  # For "Year" and "Month" pairs, there may be "NA" values in "Year"
  # This can happen if there are merged cells in use for "Year"
  # When that worksheet is converted into a data frame, the column gains blanks
  if (anyNA(qcDF$Year)) {
    
    # Add a temporary "YEAR2" column to 'qcDF'
    # That's where the NA values will be filled in
    # (The original "Year" column will not be modified)
    qcDF <- qcDF |>
      mutate(YEAR2 = Year)
    
    
    # The next step will be to fill in the NA gaps
    # Use a `for` loop for that
    
    # Go down the rows of "Year" values
    # When "Year" is not NA, update a variable called 'replacementYear'
    # If "Year" is NA, substitute it with 'replacementYear'
    
    # That way, cases like "2023 NA NA 2024 NA" become "2023 2023 2023 2024 2024"
    
    replacementYear <- NA_real_
    
    
    # Update values using a loop
    for (i in 1:nrow(qcDF)) {
      
      # If this row's iteration of "YEAR2" is not NA, update 'replacementYear'
      if (!is.na(qcDF$YEAR2[i])) {
        
        replacementYear <- qcDF$YEAR2[i]
        
      # Otherwise, if this row's value for "YEAR2" is NA, substitute in 'replacementYear'
      } else {
        
        qcDF$YEAR2[i] <- replacementYear
        
      }
      
      
    }
    
    
    # Define a "DATE" column next
    # Use "YEAR2" and "Month" for that
    qcDF <- qcDF |>
      mutate(DATE = paste0(YEAR2, "-", Month) |>
               as_date(format = "%Y-%m"))
      
    
    # Remove the "YEAR2" column afterwards
    qcDF <- qcDF |>
      select(-YEAR2)
    
  }
  
  
  # Return 'qcDF'
  return(qcDF)
  
}



wb_add_gage_charts <- function (wb, worksheet, fileName) {
  
  # For each gage in a QC worksheet, develop a bar column chart
  
  # Plot the date and precipitation data
  
  
  # First read in the QC flag table from 'worksheet'
  qcDF <- wb_to_df(wb, worksheet)
  
  # To Do: Validate the worksheet
  
  
  # Add a "DATE" column to 'qcDF'
  qcDF <- qcDF |>
    add_date_column()
  
  
  # Identify gage columns in 'qcDF' next
  gageNames <- qcDF |>
    select(where(is.numeric)) |>
    names() |>
    base::setdiff(c("Date", "Year", "Month")) |>
    str_subset("_")
  
  # Look for numeric columns in 'qcDF' 
  # Then, among that list, exclude ones with date column names (like "Year")
  # Also, these gage column names should contain an underscore
  
  
  # Stop if no gages are found
  error_if(length(gageNames) == 0,
           
           paste0("Could Not Find Gage Columns in \"", worksheet, "\"\n\n",
                  "The script could not locate the columns in this spreadsheet ",
                  "that contain precipitation data. Please investigate.\n\n",
                  "(This error occurred for \"", fileName, "\")"))
  
  
  # After that, define a new worksheet in 'wb'
  # This will contain the parsed "DATE" values as well as the charts
  chartWorksheet <- "Gage_Charts"
  
  
  wb <- wb_add_worksheet(wb, chartWorksheet)
  
  
  # Write the "DATE" variable to the first column of that worksheet
  wb <- wb_add_data(wb, chartWorksheet, qcDF |> select(DATE))
  
  
  # The next step is to generate charts for each gage
  
  # Before proceeding, define some variables that set the dimensions of the charts
  startCol <- "D"
  startRow <- 2
  chartWidth <- 7
  chartHeight <- 16
  chartGap <- 2
  
  # The charts will extend from Column D
  # Their widths will be 7 cells (Column D to K)
  
  # The charts will begin from Row 2
  # Their heights will be 16 cells (initially Row 2 to 18)
  
  # There will be a two-cell gap between each chart (in terms of height)
  
  
  # After that, in a loop, create charts for each gage
  for (i in 1:length(gageNames)) {
    
    # Define a new bar chart
    gageChart <- encharter(type = "barChart")
    
    
    # Add a chart title and axis titles to 'gageChart'
    gageChart$set_chart_title(gageNames[i])
    gageChart$set_x_title("Date")
    gageChart$set_y_title("Precip (in)")
    
    # (The chart name will display the gage column name)
    # The x-axis will be called "Date"
    # The y-axis will be called "Precip (in)"
    
    
    # Add data to the chart next
    gageChart$add_series(
      label = paste0(chartWorksheet, "!A$2:$A$", nrow(qcDF) + 1),
      data = paste0(worksheet, "!", 
                    int2col(which(names(qcDF) == gageNames[i])[1]), "$2:$", 
                    int2col(which(names(qcDF) == gageNames[i])[1]), "$", nrow(qcDF) + 1)
    )
    
    # The series name will be retain its default value ("Series1")
    # This is unimportant
    
    # The x-axis labels will be in Column A of the chart's worksheet
    # (These are the "DATE" column values that were written to the sheet before this loop)
    
    # The y-axis data values will come from the main QC worksheet
    # These will be the current iteration's gage precipitation data
    
    
    # Do not show a legend
    # (So the default "Series1" name will not appear in the chart)
    gageChart$set_legend_style(pos = "none")
    
    
    # Calculate the location of the chart in the worksheet next
    chartDims <- paste0(startCol,
                        startRow + chartGap * (i - 1) + chartHeight * (i - 1),
                        ":",
                        int2col(col2int(startCol) + chartWidth),
                        startRow + chartGap * (i - 1) + chartHeight * i)
    
    # Based on the values set before this loop, the charts will cover these ranges:
    # D2:K18, D20:K36, D38:K54, ...
    
    
    # Finally, incorporate this chart into the worksheet
    wb <- wb_add_encharter(wb, chartWorksheet, gageChart, dims = chartDims)
    
  }
  
  
  # After the gage chart worksheet has been created, return 'wb'
  return(wb)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

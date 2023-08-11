# Compare the data in an XLSX module to the results of its script counterpart


# In the "ModuleAndScriptComparisons" folder, there is a separate folder for each module
# Inside each folder, there should be at least three files:
#   (1) The script output file (Something ending with "_Scripted_[Date].xlsx")
#   (2) The input CSV file that helped produce that script output file
#   (3) The Excel module that is the counterpart of the script 
#       (Its name matches the folder name and ends with ".XLSX")
#          NOTE: The data in the input CSV file must already be present in the XLSX file
#          ALSO: The formulas in that XLSX file must be applied to all of the input data


#### Dependencies ####


require(tidyverse)
require(readxl)
require(openxlsx)
require(doParallel)


#### Script Procedure ####

mainProcedure <- function () {
  
  # The main script body
  
  
  # Ask the user to specify which comparisons to run
  comparisonList <- getUserChoice()
  
  
  
  # Iterate through the user's selections
  for (i in 1:length(comparisonList)) {
    
    # Notify the user which module is being checked
    cat(paste0("Initiating a comparison for the '", comparisonList[i], "' module..."))
    
    
    
    # Check the directory to verify that the required files are present
    dirCheck(comparisonList[i])
    
    
    # Next, read in the module and the script output file
    # (Read in all columns as text strings)
    modXLSX <- list.files(paste0("ModuleAndScriptComparisons/", comparisonList[i]), full.names = TRUE) %>%
      str_subset("\\.xlsx$") %>% str_subset("Scripted", negate = TRUE) %>% 
      str_subset("/~\\$", negate = TRUE) %>% str_subset("Difference_Comparison", negate = TRUE) %>%
      read_xlsx(col_names = FALSE, col_types = "text", sheet = getSheetNum(comparisonList[i]))
      
    
    # The module worksheet is usually the first one, 
    # but there some exceptions, as specified in the if_else() statement)
    scriptXLSX <- paste0("ModuleAndScriptComparisons/", comparisonList[i]) %>%
      list.files(full.names = TRUE) %>% str_subset("Scripted") %>%
      str_subset("/~\\$", negate = TRUE) %>% str_subset("Difference_Comparison", negate = TRUE) %>%
      read_xlsx(col_types = "text", col_names = FALSE, 
                sheet = if_else(comparisonList[i] %in% c("Diversion_Out_Of_Season_Part_B", "QAQC_Working_File"), 2, 1))
    
    
    
    # Verify that the dimensions match for both tibbles
    dimensionCheck(modXLSX, scriptXLSX)
    
    
    
    # Then, get a table (data frame) of mismatches between the two tibbles
    mismatchDF <- compareCells(modXLSX, scriptXLSX)
    
    
    # Save 'mismatchDF' as an Excel sheet in the directory
    paste0("ModuleAndScriptComparisons/", comparisonList[i], "/",
           "Difference_Comparison_", comparisonList[i], ".xlsx") %>%
      write.xlsx(x = mismatchDF, overwrite = TRUE)
    
    
    # Make a note on the console about that
    cat("Done!\n")
    
  }
  
  
  # At the end of the iterations, return nothing
  return(invisible(NULL))
  
}


getUserChoice <- function () {
  
  # Ask the user to specify which modules will be compared to their script counterparts
  
  
  # First, get a list of modules in the "Module and Script Comparisons" folder
  optionVec <- list.dirs("ModuleAndScriptComparisons/", recursive = FALSE, full.names = FALSE)
  
  
  # Using the pre-installed utils package, create a Windows dialog box
  optionSelect <- select.list(optionVec, multiple = TRUE, 
                              title = "Select the Modules to Compare to their Script Counterparts",
              graphics = TRUE)
  
  
  # Stop the script if the user cancels the dialog box and does not select anything
  if (length(optionSelect) == 0) {
    stop("The user did not select a module. Stopping the script")
  }
  
  
  # Otherwise, return a vector with the user's selections
  return(optionSelect)
  
}


dirCheck <- function (folderName) {
  
  # Check the user's chosen module folder
  # In this directory, there should be at least three files:
  #   (1) An XLSX file with "Scripted" in its name
  #   (2) A CSV file
  #   (3) An XLSX file with a name matching 'folderName'
  
  
  # Get a list of files in the folder
  fileList <- list.files(paste0("ModuleAndScriptComparisons/", folderName))
  
  
  # Verify that 'fileList' has a length of at least 3
  if (length(fileList) < 3) {
    stop("This module's directory has less than 3 files. It may be missing one of the required files.")
  }
  
  
  # First check for the presence of an XLSX file with "Scripted" in the filename
  if (fileList %>% 
      str_subset("\\.xlsx$") %>% str_subset("Scripted") %>% 
      length() == 0) {
    stop("This module's directory is missing the XLSX output of its script counterpart")
  }
  
  
  # Then, check for the presence of a CSV file
  if (fileList %>%
      str_subset("\\.csv$") %>%
      length() == 0) {
    stop("This module's directory is missing the CSV input file used by the script and the module")
  }
  
  
  # Finally, check for the presence of the XLSX module file
  if (fileList %>%
      str_subset("\\.xlsx$") %>%
      str_subset("~\\$", negate = TRUE) %>%
      str_subset("Scripted\\.xlsx", negate = TRUE) %>%
      length() == 0) {
    stop("This module's directory may be missing the module XLSX file of the same name")
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}


getSheetNum <- function (modName) {
  
  # The Excel module files have multiple sheets
  # Depending on the module, the actual module sheet location may vary
  
  # Depending the module specified in 'modName', return the corresponding module's sheet name
  
  
  
  # For several modules, the sheet name matches the module name
  if (modName %in% c("Beneficial_Use_Return_Flow", "DuplicateReport_SameOwner",
                     "Missing_RMS_Reports")) {
    return(modName)
  }
  
  
  if (modName == "Diversion_Out_Of_Season_Part_A") {
    return("USE_SEASON_FLATFILE")
  }
  
  
  if (modName == "Diversion_Out_Of_Season_Part_B") {
    return("DIVERSION_OUT_OF_SEASON")
  }
  
  
  if (modName == "DuplicateMonths_Years") {
    return("Duplicate_Months_Years")
  }
  
  
  if (modName == "ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics") {
    return("ReportedDiversionAnalysis")
  }
  
  
  if (modName == "Priority_Date") {
    return("PriorityDate")
  }
  
  
  if (modName == "QAQC_Working_File") {
    return("MasterDemandTable")
  }
  
  
  
  # Throw an error if the script reaches this point
  stop(paste0("No sheet name was specified for directory ", modName))
  
}


dimensionCheck <- function (moduleDF, scriptDF) {
  
  # Compare the dimensions of the output tables from the module and script
  # They should have the same width and height
  
  
  # Check the number of rows
  if (nrow(moduleDF) != nrow(scriptDF)) {
    stop("The Excel files for the module and script output have a different number of rows")
  }
  
  
  # Check the number of columns
  if (ncol(moduleDF) != ncol(scriptDF)) {
    stop("The Excel files for the module and script output have a different number of columns")
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}


compareCells <- function (moduleDF, scriptDF) {
  
  # Given two tibbles with the same dimensions,
  # compare corresponding cells
  # Create a data frame that notes any mismatched cells
  
  
  # Each time a cell doesn't match, record:
  # The cell column and row
  # The column name in the module sheet
  # The value in the module sheet
  # The column name in the script sheet
  # The value in the script sheet
  
  
  
  # NOTE
  # This function is only designed to work with tables that have a column length below 208
  # (It can only assign column labels between A and GZ)
  stopifnot(ncol(moduleDF) <= 208)
  
  
  # If 'moduleDF' is too large, use a parallel processing version of this function
  # (More than 100,000 rows)
  if (nrow(moduleDF) > 10^5) {
    return(parallelComparison(moduleDF, scriptDF))
  }
  
  
  # Prepare a vector to hold mismatches
  mismatchVec <- c()
  
  
  
  # Then, iterate through the rows and columns of the tibbles
  for (i in 1:nrow(moduleDF)) {
    
    for (j in 1:ncol(moduleDF)) {
      
      # Compare the values of the cells located at indices [i, j]
      # Ideally they should both have the same values
      
      
      # If they are both NA, skip to the next iteration
      if (moduleDF[[i, j]] %in% c(NA, "NA") && 
          scriptDF[[i, j]] %in% c(NA, "NA")) {
        next
      }
      
      
      # If both are not NA, and they are equal, skip to the next iteration
      if (!(moduleDF[[i, j]] %in% c(NA, "NA")) && 
          !(scriptDF[[i, j]] %in% c(NA, "NA")) &&
          moduleDF[[i, j]] == scriptDF[[i, j]]) {
        next
      }
      
      
      # Include one additional check specifically for date comparisons
      # If the compared cell is meant to contain a date, there may be issues
      # Because of the script procedures, 'scriptDF' may contain a properly formatted 
      # date (as a string), while 'moduleDF' contains a number string instead 
      # (because of how dates work in Excel)
      
      
      # Check if a date-like string appears in 'scriptDF' 
      # and a numeric string appears in 'moduleDF'
      if (grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", scriptDF[[i, j]]) &&
          grepl("^[0-9]+$", moduleDF[[i, j]])) {
        
        # Convert the strings to dates and compare them
        # If they are equal, skip to the next iteration
        if (moduleDF[[i, j]] %>% as.numeric() %>% as.Date(origin = "1899-12-30") ==
            scriptDF[[i, j]] %>% as_date(format = "%m/%d/%Y")) {
          next
        }
        
      }
      
      
      
      # If none of the "if" statements were TRUE,
      # then there is a mismatch here
      # The two tibbles do not have the same value for this index
      
      
      # In that case, make a note in 'mismatchVec'
      # The cell column and row
      # The column name in the module sheet
      # The value in the module sheet
      # The column name in the script sheet
      # The value in the script sheet
      mismatchVec <- c(mismatchVec,
                       paste0(getColumnLabel(j), i),
                       moduleDF[[1, j]], moduleDF[[i, j]],
                       scriptDF[[1, j]], scriptDF[[i, j]])
      
    } # End of loop j
    
  } # End of loop i
  
  
  
  # After the loops have completed,
  # if no differences were found, 'mismatchVec' will be empty
  # In that case, return NULL
  if (length(mismatchVec) == 0) {
    return(NULL)
  }
  
  
  # After the iterations, return 'mismatchVec' as a data frame
  return(mismatchVec %>%
           matrix(ncol = 5, byrow = TRUE) %>%
           data.frame() %>%
           set_names(c("MISMATCHED_CELL", 
                       "MODULE_COL_NAME", "MODULE_VALUE",
                       "SCRIPT_COL_NAME", "SCRIPT_VALUE")))
  
}


parallelComparison <- function (moduleDF, scriptDF) {
  
  # Perform the procedure of compareCells() with parallel R sessions
  
  
  
  # First, setup parallel processing capabilities
  
  
  # Create a cluster of parallel R sessions
  #(Use all except 1 of the system's cores)
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  
  
  # Prepare a variable to hold information about the mismatched cells
  # (The exact required length will be unknown at this point, 
  #  but allocate a lot of space in advance, just in case)
  mismatchList <- vector(mode = "list", length = nrow(moduleDF))
  
  
  # In a separate function, search for mismatched cells
  # The rows of 'moduleDF' and 'scriptDF' will be compared in parallel
  # Within each worker, all of the columns will be checked for mismatches
  mismatchList <- foreach(i = 1:nrow(moduleDF)) %dopar% parallelCellCheck(moduleDF[c(1, i), ], 
                                                                          scriptDF[c(1, i), ], 
                                                                          i)
  # NOTE
  # "c(1, i)" is used to include the header row in the input to parallelCellCheck()
  # (It's okay that the header row is duplicated during the iteration i = 1)
  # (The result will be unaffected)
  
  
  # NOTE 2
  # With 517,983 rows and 7 workers, this parallel loop took about 6 minutes for me
  
  
  
  # At the end of these operations, remove the extra R session workers
  stopCluster(cl)
  
  
  
  # Format 'mismatchList' into a DF and return it
  return(mismatchList %>%
           unlist() %>%
           matrix(ncol = 5, byrow = TRUE) %>%
           data.frame() %>%
           set_names(c("MISMATCHED_CELL", 
                       "MODULE_COL_NAME", "MODULE_VALUE",
                       "SCRIPT_COL_NAME", "SCRIPT_VALUE")))
  
}


parallelCellCheck <- function (moduleRow, scriptRow, i) {
  
  # For given rows from 'moduleDF' and 'scriptDF',
  # check if the values in each corresponding column are the same value
  # If not, note that in a vector that will be returned
  
  
  # Initialize a vector to hold information about mismatches
  mismatchVec <- c()
  
  
  # Iterate through the columns of the DF row
  for (j in 1:ncol(moduleRow)) {
    
    # If they are both NA, skip to the next iteration
    if (moduleRow[[2, j]] %in% c(NA, "NA") && 
        scriptRow[[2, j]] %in% c(NA, "NA")) {
      next
    }
    
    
    # If both are not NA, and they are equal, skip to the next iteration
    if (!(moduleRow[[2, j]] %in% c(NA, "NA")) && 
        !(scriptRow[[2, j]] %in% c(NA, "NA")) &&
        moduleRow[[2, j]] == scriptRow[[2, j]]) {
      next
    }
    
    
    # Include one additional check specifically for date comparisons
    # If the compared cell is meant to contain a date, there may be issues
    # Because of the script procedures, 'scriptRow' may contain a properly formatted 
    # date (as a string), while 'moduleRow' contains a number string instead 
    # (because of how dates work in Excel)
    
    
    # Check if a date-like string appears in 'scriptRow' 
    # and a numeric string appears in 'moduleRow'
    if (grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", scriptRow[[2, j]]) &&
        grepl("^[0-9]+$", moduleRow[[2, j]])) {
      
      # Convert the strings to dates and compare them
      # If they are equal, skip to the next iteration
      if (moduleRow[[2, j]] %>% as.numeric() %>% as.Date(origin = "1899-12-30") ==
          scriptRow[[2, j]] %>% as_date(format = "%m/%d/%Y")) {
        next
      }
      
    }
    
    
    
    # If none of the "if" statements were TRUE,
    # then there is a mismatch here
    # The two tibbles do not have the same value for this index
    
    
    # In that case, make a note in 'mismatchVec'
    # The cell column and row
    # The column name in the module sheet
    # The value in the module sheet
    # The column name in the script sheet
    # The value in the script sheet
    mismatchVec <- c(mismatchVec,
                     paste0(LETTERS[j], i),
                     moduleRow[[1, j]], moduleRow[[2, j]],
                     scriptRow[[1, j]], scriptRow[[2, j]])
    
    
  }
  
  
  
  # After the loop is complete,
  # If 'mismatchVec' is empty, return NULL
  if (length(mismatchVec) == 0) {
    return(NULL)
  }
  
  
  
  # Otherwise, return the vector
  return(mismatchVec)
  
}


getColumnLabel <- function (j) {
  
  # j is the column index
  # Based on this value, return the proper column label
  
  
  # If j is <= 26, use 'LETTERS' to get the label
  if (j <= 26) {
    return(LETTERS[j])
  }
  
  
  # If j is <= 52, use "A" along with 'LETTERS' to get the correct label
  if (j <= 26 * 2) {
    return(paste0("A", rep(LETTERS, 2)[j]))
  }
  
  
  # Use a generic loop procedure for the remaining checks
  # Include support for a maximum of 8 * 26 = 208 columns
  # Greater column sizes can easily be supported with this loop (up to 26 * 26),
  # but module tables should not be bigger than this
  for (i in 3:8) {
    
    if (j >= 26 * (i - 1) && j <= 26 * i) {
      return(paste0(LETTERS[i - 1],
                    rep(LETTERS, i)[j]))
    }
    
  }
  
  
  # If the procedure reaches this point, throw an error
  stop(paste0("The table's column count exceeds the supported length of this function (max is 182, column count is ", 
              j, ")"))
  
}


#### Script Execution ####

mainProcedure()
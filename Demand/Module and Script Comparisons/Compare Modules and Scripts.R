# Compare the data in an XLSX module to the results of its script counterpart


# In the "Module and Script Comparisons" folder, there is a separate folder for each module
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
    modXLSX <- suppressMessages(read_xlsx(paste0("Module and Script Comparisons/", 
                                                 comparisonList[i], "/", 
                                comparisonList[i], ".xlsx"), 
                         col_types = "text", col_names = FALSE, 
                         sheet = comparisonList[i] %>% str_remove("_")))
    
    scriptXLSX <- suppressMessages(paste0("Module and Script Comparisons/", comparisonList[i]) %>%
      list.files(full.names = TRUE) %>% str_subset("Scripted") %>%
      read_xlsx(col_types = "text", col_names = FALSE))
    
    
    
    # Verify that the dimensions match for both tibbles
    dimensionCheck(modXLSX, scriptXLSX)
    
    
    
    # Then, get a table (data frame) of mismatches between the two tibbles
    mismatchDF <- compareCells(modXLSX, scriptXLSX)
    
    
    # Save 'mismatchDF' as an Excel sheet in the directory
    paste0("Module and Script Comparisons/", comparisonList[i], "/",
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
  optionVec <- list.dirs("Module and Script Comparisons/", recursive = FALSE, full.names = FALSE)
  
  
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
  fileList <- list.files(paste0("Module and Script Comparisons/", folderName))
  
  
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
      str_subset(paste0("^", folderName, "\\.xlsx$")) %>% 
      length() == 0) {
    stop("This module's directory is missing the module XLSX file of the same name")
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
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
  # This function is only designed to work with tables that have a column length below 27
  # (It can only assign column labels between A and Z)
  stopifnot(ncol(moduleDF) <= 26)
  
  
  
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
                       paste0(LETTERS[j], i),
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



#### Script Execution ####

# mainProcedure()


# Ask the user 
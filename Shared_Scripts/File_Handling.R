# This script contains functions related to input and output files

# Many of these scripts read or write a file


#### Dependencies ####


# This script DOES NOT call all required packages and dependencies

# Please use "!Shared_Functions_Importer.R"


#### Functions ####


##### File Reading #####

getFile <- function (filePath, fileType = NULL, largeFile = FALSE, delim = NULL,
                     select = NULL, trim_ws = FALSE, 
                     worksheet = NULL, range = NULL, col_names = TRUE,
                     col_types = NULL, skip = 0, n_max = Inf, 
                     guess_max = min(10^6, n_max)) {
  
  # Given the path to a file, read it into a tibble
  
  # There are several optional arguments as well:
  #   (*) 'fileType' is the type of the file
  #       It can have these values: "XLSX", "CSV", "TSV", "DELIM", or "OTHER"
  #       If 'fileType' is NULL, this function will guess the type
  
  #   (*) 'largeFile' is a boolean that applies to "CSV" and "DELIM" files only
  #       If this value is TRUE, fread() from the 'data.table' package will 
  #       be used instead of read_delim() from the 'readr' package
  
  #   (*) The remaining arguments can be supplied to the different file-reading
  #       functions (like `read_xlsx`, `fread`, `read_delim`, and `read_lines`)
  
  
  # First make sure that 'filePath' is not NA
  # This is a sign of a missing input
  if (is.na(filePath)) {
    
    stop(paste0("No Filepath Specified\n\n",
                "The function `getFile` was called without a proper filepath as ",
                " input\n\n", 
                "Please investigate this issue and revise the script, ",
                "if needed") |>
           errWrap() |>
           str_replace("(without)", col_red("\\1")))
    
  }
  
  
  # Check if 'fileType' is NULL
  # If so, guess the type
  if (is.null(fileType)) {
    
    fileType <- guessFileType(filePath, delim)
    
  }
  
  
  # Make sure 'fileType' is one of the accepted values
  if (!(fileType %in% c("XLSX", "CSV", "TSV", "DELIM", "OTHER"))) {
    
    stop(paste0("Unknown File Type\n\n",
                "The file type specified in `getFile()` should only be one of ",
                "these strings: ",
                "\"XLSX\", \"CSV\", \"TSV\", \"DELIM\", or \"OTHER\"") |>
           errWrap() |>
           str_replace_all("\"(.+)\"", paste0("\"", col_green("\\1"), "\"")))
    
  }
  
  
  # Next, check if 'filePath' contains a SharePoint path
  # If it is, modify 'filePath' to be a complete SharePoint path
  filePath <- sharepointPathCheck(filePath, isFolder = FALSE)
  
  
  # If it is not a SharePoint path, and the file still does not exist,
  # output an error message
  if (!file.exists(filePath)) {
    
    stop(paste0("File Does Not Exist\n\n",
                "The specified file could not be found\n\n",
                "Please confirm that this path is correct: '", 
                normalizePath(filePath, mustWork = FALSE), "'") |>
           errWrap() |>
           str_replace("(could not be found)", col_red("\\1")))
    
  }
  
  
  # Finally, call different functions to read in the file
  if (fileType == "XLSX") {
    
    return(getXLSX(filePath, worksheet = worksheet, range = range, 
                   col_names = col_names, col_types = col_types, skip = skip,
                   n_max = n_max, guess_max = guess_max))
    
  } else if (fileType == "CSV") {
    
    return(getDelim(filePath, delim = ",", largeFile, select = select,
                    col_types = col_types, skip = skip, trim_ws = trim_ws,
                    n_max = n_max, guess_max = guess_max, col_names = col_names))
    
  } else if (fileType == "TSV") {
    
    return(getDelim(filePath, delim = "\t", largeFile, select = select,
                    col_types = col_types, skip = skip, trim_ws = trim_ws,
                    n_max = n_max, guess_max = guess_max, col_names = col_names))
    
  } else if (fileType == "DELIM") {
    
    return(getDelim(filePath, delim = delim, largeFile, select = select,
                    col_types = col_types, skip = skip, trim_ws = trim_ws,
                    n_max = n_max, guess_max = guess_max, col_names = col_names))
    
  } else {
    
    # For files labeled as "OTHER", just use read_lines()
    return(read_lines(filePath, n_max = n_max, skip = skip))
    
  }
  
}



getXLSX <- function (filePath, worksheet = NULL, 
                     range = NULL, col_names = TRUE, col_types = NULL, skip = 0,
                     n_max = Inf, guess_max = min(1000, n_max)) {
  
  # This function is a wrapper for readxl's read_xlsx() function
  
  # It has additional error handling processes
  
  
  # First, make sure 'filePath' is a character variable
  if (!is.character(filePath)) {
    
    stop(paste0("Unusable File Path\n\n",
                "The provided file path is not a character variable.\n\n",
                "Please double-check that '", filePath, "' is a valid path. ",
                "Script revisions may be necessary.") |>
           errWrap() |>
           str_replace("(incorrect)", col_red("\\1")))
    
  }
  
  
  sheetDF <- try(readxl::read_xlsx(filePath, sheet = worksheet, range = range,
                                   col_names = col_names, col_types = col_types,
                                   skip = skip, n_max = n_max, guess_max = guess_max), 
                 silent = TRUE)
  
  
  if ("try-error" %in% class(sheetDF)) {
    
    # In every case, output the actual error message first
    cat("\n\n")
    message(sheetDF)
    cat("\n\n")
    
    
    # Next, address different errors with custom messages
    if (grepl("zip file .+ cannot be opened", sheetDF, ignore.case = TRUE)) {
      
      # There are at least two different situations where this error can occur
      
      # This error can occur when trying to read in a file that is NOT 
      # an Excel spreadsheet
      if (!grepl("\\.xls.?$", filePath)) {
        
        stop(paste0("Incorrect File Issue\n\n",
                    "The input file '", filePath, "' does not appear to be ",
                    "an Excel spreadsheet. Please investigate.") |>
               errWrap())
        
        # Alternatively, it can happen when trying to read a SharePoint 
        # spreadsheet that is already open locally
      } else {
        
        stop(paste0("Inaccessible File Issue\n\n",
                    "The above error message usually occurs if the target ",
                    "spreadsheet is open in Excel.\n\nPlease close '", filePath, 
                    "' and try again.") |>
               errWrap() |>
               str_replace("(open)", col_red("\\1")) |>
               str_replace("(close)", col_green("\\1")))
        
      }
      
    } else if (grepl("path. does not exist", sheetDF, ignore.case = TRUE)) {
      
      stop(paste0("File Does Not Exist\n\n",
                  "The above error message usually occurs if the filepath ",
                  "is incorrect.\n\nPlease double-check that '", 
                  normalizePath(filePath, mustWork = FALSE), 
                  "' is a valid path.") |>
             errWrap() |>
             str_replace("(incorrect)", col_red("\\1")))
      
    } else if (grepl("Error in UseMethod..as.cell_limits", sheetDF, ignore.case = TRUE)) {
      
      stop(paste0("Worksheet Name Issue\n\n",
                  "The above error message usually occurs if the specified ",
                  "worksheet name is incorrect.\n\nPlease double-check '", 
                  filePath, "' and verify that the worksheet name '",
                  worksheet, "' is correct.") |>
             errWrap() |>
             str_replace("(incorrect)", col_red("\\1")))
      
    } else {
      
      stop(paste0("Please resolve the error specified above\n\n",
                  "If the issue persists, definitely reach out for assistance") |>
             errWrap())
      
    }
    
  }
  
  
  # If there are no errors, return 'sheetDF'
  return(sheetDF)
  
}



getDelim <- function (filePath, delim, largeFile = FALSE, 
                      select = NULL, col_types = NULL, skip = 0,
                      trim_ws = FALSE, n_max = Inf, guess_max = min(5000, n_max),
                      col_names = TRUE) {
  
  # Use `read_delim` or `fread` to import a file as a data frame
  
  
  # First, make sure 'filePath' is a character variable
  if (!is.character(filePath)) {
    
    stop(paste0("Unusable File Path\n\n",
                "The provided file path is not a character variable.\n\n",
                "Please double-check that '", filePath, "' is a valid path. ",
                "Script revisions may be necessary.") |>
           errWrap() |>
           str_replace("(incorrect)", col_red("\\1")))
    
  }
  
  
  # If 'largeFile' is TRUE, use fread() and import the file as a data frame
  # Otherwise, use read_delim() and read in the file as a tibble
  if (largeFile) {
    
    fileDF <- try(fread(filePath, sep = delim, select = select,
                        strip.white = trim_ws, nrows = n_max,
                        header = col_names), silent = TRUE)
    
  } else {
    
    # Both options use `read_delim` here
    
    # However, the "col_select" argument can interpret arguments using 
    # the tidyverse mini-language, and that causes an error when it tries 
    # to interpret the input value "select", even if it's NULL
    
    # For that reason, a value is specified for "col_select" only when it's
    # actually intended to be used
    if (is.null(select)) {
      
      fileDF <- try(read_delim(filePath, delim = delim, 
                               col_types = col_types, show_col_types = FALSE,
                               skip = skip, trim_ws = trim_ws, n_max = n_max,
                               guess_max = guess_max, col_names = col_names), 
                    silent = TRUE)
      
    } else {
      
      fileDF <- try(read_delim(filePath, delim = delim, 
                               col_types = col_types, show_col_types = FALSE,
                               skip = skip, trim_ws = trim_ws, n_max = n_max,
                               col_names = col_names, guess_max = guess_max,
                               col_select = select), 
                    silent = TRUE)
      
    }
    
  }
  
  
  # Check for errors in 'fileDF'
  if ("try-error" %in% class(fileDF)) {
    
    # In every case, output the actual error message first
    cat("\n\n")
    message(fileDF)
    cat("\n\n")
    
    
    # Next, address different errors with custom messages
    if (grepl("does not exist", fileDF, ignore.case = TRUE)) {
      
      stop(paste0("File Does Not Exist\n\n",
                  "The above error message usually occurs if the filepath ",
                  "is incorrect.\n\nPlease double-check that '", filePath, 
                  "' is a valid path.") |>
             errWrap() |>
             str_replace("(incorrect)", col_red("\\1")))
      
    } else {
      
      stop(paste0("Please resolve the error specified above\n\n",
                  "If the issue persists, definitely reach out for assistance") |>
             errWrap())
      
    }
    
  }
  
  
  # If there are no issues, return 'fileDF'
  return(fileDF)
  
}


###### Non-Standard Files ######

read_out2 <- function (outPath) {
  
  # Given the path to a .out2 file, 
  # read it in and format it as a proper tibble
  
  # The data is space-delimited
  
  # However, the column headers are trapped between rows of metadata
  
  # In addition, sometimes, data entries don't have any spaces between them
  # (generally it can happen when there's a negative number)
  
  
  # First, use read_lines() to read in the file
  outDF <- read_lines(outPath)
  
  
  # Remove empty strings from the vector
  outDF <- outDF |> 
    str_subset("^$", negate = TRUE)
  
  
  # Find the row containing the headers
  headerRow <- grep("Year\\s+mo", outDF, ignore.case = TRUE)
  
  
  if (length(headerRow) != 1) {
    
    stop(paste0("Could Not Find Header Row of Out2 File\n\n",
                "The header was expected to be a line that uniquely starts with ",
                "\"Year\", followed by spaces, and then \"mo\". However ",
                length(headerRow), " matches were found with this pattern. ",
                "Please investigate the file and update the script if ",
                "needed.\n\n",
                "(This error occurred for \"", outPath, "\")") |>
           errWrap())
    
  }
  
  
  # Remove the rows before 'headerRow' in 'outDF'
  outDF <- outDF[headerRow:length(outDF)]
  
  
  # Get a vector of the header names
  columnNames <- outDF[1] |>
    spaceSplit()
  
  
  # If the second row contains units for the columns,
  # append them to 'columnNames'
  if (grepl("\\s+\\(in\\)\\s+", outDF[2], ignore.case = TRUE)) {
    
    unitVec <- outDF[2] |>
      spaceSplit() |>
      str_split("[\\(\\)]") |> unlist() |>
      str_subset("^$", negate = TRUE)
    
    
    # The "Year", "mo", and "day" variables at the beginning do not have any units
    # Add three empty strings to the start of 'unitVec'
    unitVec <- c("", "", "",
                 unitVec)
    
    
    if (length(unitVec) != length(columnNames)) {
      
      stop(paste0("Could Not Assign Units in Out2 File\n\n",
                  "There are ", length(columnNames), " columns in this file, ",
                  "and units were detected in the second row. However, they ",
                  "could not be properly matched to their corresponding ",
                  "headings. Please investigate the file and update the ",
                  "script if needed.\n\n",
                  "(This issue occurred for \"", outPath, "\")") |>
             errWrap())
      
    }
    
    
    # Paste these units at the end of 'columnNames'
    columnNames <- map2_chr(columnNames, unitVec,
                            ~ if_else(.y == "", .x, paste0(.x, " (", .y, ")")))
    
  }
  
  
  # The last non-data row contains the value "initial"
  # Find its index
  removalIndex <- grep("^\\s*initial", outDF)
  
  
  if (length(removalIndex) != 1) {
    
    stop(paste0("Could Not Find Data Cutoff Row of Out2 File\n\n",
                "The non-data rows were expected to end with a line that ",
                "uniquely starts with \"initial\" (and maybe some spaces at ",
                "the beginning). However, ", length(removalIndex), " matches ",
                "were found with this pattern. Please investigate the file ",
                "and update the script if needed.\n\n",
                "(This error occurred for \"", outPath, "\")") |>
           errWrap())
    
  }
  
  
  # Remove all rows up to 'removalIndex'
  outDF <- outDF[-c(1:removalIndex)]
  
  
  # Remove the "Execution elapsed time" row at the end as well
  outDF <- outDF |>
    str_subset("^\\s*Execution elapsed time", negate = TRUE)
  
  
  # The only rows left in 'outDF' should be the data now
  # Within each row, split the data at the spaces
  outDF <- outDF |>
    map(spaceSplit)
  
  
  # Get a vector of lengths for each row
  # Check for rows that do not have the expected length 
  # (there should be one element per column heading)
  rowLens <- lengths(outDF)
  
  
  # Check for entries that have the incorrect number of elements
  problemRows <- which(rowLens != length(columnNames))
  
  
  # Iterate through the problematic rows
  # Try to fix them
  if (length(problemRows) > 0) {
    
    for (i in 1:length(problemRows)) {
      
      # Issue Type #1
      # One potential error comes from having a number followed by 
      # a negative number with no space in-between
      if (sum(grepl("^\\-?[0-9\\.]+\\-[0-9\\.]+$", outDF[[problemRows[i]]])) > 0) {
        
        # Iterate through the entries in this row of 'outDF'
        for (j in 1:length(outDF[[problemRows[i]]])) {
          
          # If this is a row with both a number and a negative number, separate them
          if (grepl("^\\-?[0-9\\.]+\\-[0-9\\.]+$", outDF[[problemRows[i]]][j])) {
            
            # Use a positive look-ahead regex to split the numbers 
            # (while preserving the negative sign)
            outDF[[problemRows[i]]][j] <- outDF[[problemRows[i]]][j] |>
              str_split("(?=\\-)")
            
          }
          
        }
        
        
        # Splitting operations within the list element may create a sub-list there
        # Remove any sub-lists
        outDF[[problemRows[i]]] <- outDF[[problemRows[i]]] |>
          unlist() |> 
          str_subset("^$", negate = TRUE)
        
      } # End of Issue Type #1 resolution
      
    } # End of loop through 'problemRows'
    
  }
  
  
  # Make sure that every row has the proper length now
  rowLens <- lengths(outDF)
  
  
  # Output an error if there are still issues
  if (unique(rowLens) |> length() != 1) {
    
    problemRows <- which(rowLens != length(columnNames))
    
    stop(paste0("Problematic Data in the Out2 File\n\n",
                "Each row is expected to have ", length(columnNames),
                " values. However, ", length(problemRows), " row",
                if_else(length(problemRows) > 1, "s have ", " has "),
                "an issue: \n\n", 
                outDF[problemRows] |>
                  map_chr(~ paste0(., collapse = " ")) |>
                  vec2QuotedStr(), 
                "\n\n",
                "Please investigate the file and update the script if ",
                "needed.\n\n",
                "(This error occurred for \"", outPath, "\")") |>
           errWrap())
    
  } else if (unique(rowLens) != length(columnNames)) {
    
    stop(paste0("Problematic Data in the Out2 File\n\n",
                "Each row is expected to have ", length(columnNames),
                " values. However, every row has ", unique(rowLens),
                if_else(unique(rowLens) > 1, " entries", " entry"), ". ",
                "Please investigate the file and update the script if ",
                "needed.\n\n",
                "(This error occurred for \"", outPath, "\")") |>
           errWrap())
    
  }
  
  
  # Convert 'outDF' into a proper tibble
  outDF <- outDF |>
    unlist() |> as.numeric() |>
    matrix(ncol = length(columnNames), byrow = TRUE) |>
    data.frame() |> tibble() |>
    set_names(columnNames)
  
  
  # After these changes, return 'outDF'
  return(outDF)
  
}



getPRISM <- function (prismPath) {
  
  # Read in the PRISM web data CSV
  
  # Because multiple lines serve as a header, first find the number of header lines
  # Then, use the proper CSV processing function to 
  
  
  prismVec <- getFile(prismPath, fileType = "OTHER", 
                      "n_max" = 50)
  
  
  # Get the line where the headers start
  
  # The line will start with "Name,Longitude,Latitude"
  headerRegex <- "^Name,Longitude,Latitude"
  
  
  headerLine <- grep(headerRegex, prismVec)
  
  
  # Return an error if 'headerLine' was not found (or if multiple matches were found)
  if (length(headerLine) == 0) {
    
    stop(paste0("PRISM Data File - Missing Column Header Issue\n\n", 
                "This script attempted to find the header row in the PRISM ",
                "CSV file containing climate data. However, the header row ",
                "could not be found.\n\n",
                "There could be data corruption issues, or the formatting of ",
                "PRISM's output files may have changed. This script may need ",
                "updates, depending on the cause of this problem.\n\n",
                "Please investigate '", prismPath, "'") |>
           errWrap())
    
  } else if (length(headerLine) > 1) {
    
    stop(paste0("PRISM Data File - Could Not Identify Column Header\n\n", 
                "This script attempted to find the header row in the PRISM ",
                "CSV file containing climate data. However, an unusual issue ",
                "was encountered.\n\n",
                "The header row is usually identified via this regular expression:\n\n",
                "(*) \"", headerRegex, "\"\n\n",
                "There should be exactly one row in the input file that has this ",
                "pattern. However, more than one match was found.\n\n", 
                "Please investigate '", prismPath, "'") |>
           errWrap())
    
  }
  
  
  # If there are no issues, read in the PRISM data again
  # This time, skip lines so that the first row is the header row (identified by 'headerLine')
  prismDF <- getDelim(prismPath, delim = ",", skip = headerLine - 1)
  
  
  # Make sure no rows that only contain "NA" are present
  # Use the "Name" column for that check
  prismDF <- prismDF |>
    filter(!is.na(Name))
  
  
  # Also, confirm that "Date" was parsed as a date column correctly
  # If not, apply that type manually
  if (is.character(prismDF$Date[1])) {
    
    prismDF <- prismDF |>
      mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
    
  }
  
  
  # Return 'prismDF'
  return(prismDF)
  
}



read_dat <- function (datPath, datType = NULL) {
  
  # Read in a DAT file
  
  # It will have one of several formats depending on the model
  
  # This will affect the method of reading in the file
  
  
  # First, read in a portion of the DAT file
  # This can help determine the type of DAT file (as well as the headers)
  datPartial <- getFile(datPath, n_max = 50, fileType = "OTHER")
  
  
  # If 'datType' is NULL, try to guess the type of DAT file
  if (is.null(datType)) {
    
    datType <- guessDAT(datPartial)
    
  }
  
  
  # Make sure 'datType' is one of the expected values
  datOptions <- c("PRMS", "SRP", "RRIHM")
  
  
  # If 'datType' is something else, output an error message
  if (!(datType %in% datOptions)) {
    
    paste0("Unrecognized DAT Type\n\n", 
           "The function parameter 'datType' is expected to be 1 of ",
           length(datOptions), " different options (", vec2QuotedStr(datOptions),
           "). However, `read_dat` received \"", datType, "\" as input ",
           "instead. Please investigate the cause.\n\n", 
           "(This error occurred while reading \"", datPath, "\")") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The next step is to determine the column headers
  # The method will differ depending on the type of DAT file
  # The end result should be a tibble with separate columns and headers applied
  if (datType == "SRP") {
    
    # In the row containing multiple "#", 'datPartial' will have the names of
    # its columns
    
    # Locate that row first
    headerRegex <- "^\\s*#+\\s*"
    
    # This regex says that the line should begin with:
    #   (*) 0 or more spaces
    #   (*) 1 or more "#"
    #   (*) Followed by 0 or more spaces
    
    
    # Locate the header
    headerRow <- grep(headerRegex, datPartial)
    
    
    # If 'headerRow' was not found, output an error message
    if (length(headerRow) != 1) {
      
      paste0("Header Row Not Found\n\n", 
             "While parsing a DAT file for ", datType, ", the function ",
             "failed to locate the header. The regular expression \"", 
             headerRegex, "\" returned ", length(headerRow), " matches instead ",
             "of a single location, as expected. Please investigate the ",
             "cause.\n\n", 
             "(This error occurred while reading \"", datPath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
    
    # Extract the column headers from 'headerRow' 
    # Note: If "date" appears in that list, exclude it
    #       That should never have been there in the first place! ^_^'
    headers <- datPartial[headerRow] |>
      str_split("\\s") |> unlist() |>
      str_subset("^$", negate = TRUE) |>
      str_subset("^#+$", negate = TRUE) |>
      str_subset("^date$", negate = TRUE)
    
    
    # Make sure 'headers' is not empty or NA
    if (length(headers) == 0 || anyNA(headers)) {
      
      paste0("Failed to Extract Headers\n\n", 
             "While parsing a DAT file for ", datType, ", the function ",
             "failed to extract the headers from Line ", headerRow, ". ",
             "Please investigate the cause.\n\n", 
             "(This error occurred while reading \"", datPath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
    
    # Extract the first row after 'headerRow' and confirm that the number 
    # of headers matches the number of elements in the row
    
    # (Though, if 'headerRow' is at the end of 'datPartial', another row
    #  must be read in for this check)
    if (headerRow == length(datPartial)) {
      
      datPartial <- getFile(datPath, fileType = "OTHER", n_max = headerRow + 1)
      
    }
    
    
    # Take the row after 'headerRow' and get the number of elements
    numCols <- datPartial[headerRow + 1] |>
      str_split("\\s") |> unlist() |>
      str_subset("^$", negate = TRUE) |> length()
    
    
    # Raise an exception if 'numCols' does not equal the length of 'headers'
    if (length(headers) != numCols) {
      
      paste0("Header Mismatch\n\n", 
             "While parsing a DAT file for ", datType, ", the function ",
             "extracted column headers from Line ", headerRow, ". ",
             "However, ", length(headers), " header(s) were extracted, while ",
             numCols, " unique column(s) were identified in the subsequent ",
             "line. Please investigate the cause.\n\n", 
             "(This error occurred while reading \"", datPath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
    
    # Next, read in the entirety of the DAT file
    # (Skip the lines up until the header row)
    datDF <- getFile(datPath, fileType = "OTHER", skip = headerRow)
    
    
    # Split 'datDF' wherever spaces occur (and remove empty strings)
    datDF <- datDF |>
      str_split("\\s") |> unlist() |>
      str_subset("^$", negate = TRUE)
    
    
    # Make sure the length of 'datDF' is divisible by 'numCols'
    if (length(datDF) %% numCols != 0) {
      
      paste0("Header Mismatch\n\n", 
             "While parsing a DAT file for ", datType, ", the function ",
             "extracted column headers from Line ", headerRow, ". ",
             "However, ", numCols, " header(s) were extracted, while ",
             length(datDF), " values were identified in the subsequent ",
             "rows. This number is not divisible by ", numCols, ", so the ",
             "data cannot be formatted into a tibble. Please investigate ",
             "the cause.\n\n", 
             "(This error occurred while reading \"", datPath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
    
    # Reformat 'datDF' as a matrix and then a tibble
    # After that, apply 'headers' as the column names
    datDF <- datDF |>
      matrix(ncol = numCols, byrow = TRUE) |>
      as_tibble(.name_repair = "minimal") |>
      set_names(headers)
    
    
    # This procedure is for PRMS DAT files instead
  } else if (datType == "PRMS") {
    
    # Before the row containing multiple "#", 'datPartial' will have the names 
    # of different types of columns (and the number of those columns)
    
    
    # Locate that row first
    headerRegex <- "^\\s*#+\\s*"
    
    # This regex says that the line should begin with:
    #   (*) 0 or more spaces
    #   (*) 1 or more "#"
    #   (*) Followed by 0 or more spaces
    
    
    # Locate the header
    headerCutoff <- grep(headerRegex, datPartial)
    
    
    # If 'headerCutoff' was not found, output an error message
    if (length(headerCutoff) != 1) {
      
      paste0("Header Cutoff Row Not Found\n\n", 
             "While parsing a DAT file for ", datType, ", the function ",
             "failed to locate the header cutoff. The regular expression \"", 
             headerRegex, "\" returned ", length(headerCutoff), " matches instead ",
             "of a single location, as expected. Please investigate the ",
             "cause.\n\n", 
             "(This error occurred while reading \"", datPath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
    
    # Get the rows before 'headerCutoff'
    # Extract the strings that contain numbers
    # (These entries tell the number of columns of that type)
    headerInfo <- datPartial[1:headerCutoff] |>
      str_split("\t") |> unlist() |>
      str_subset("[A-Za-z]+\\s*[0-9]+")
    
    
    # Check to make sure 'headerInfo' is not empty
    if (length(headerInfo) == 0 || anyNA(headerInfo)) {
      
      paste0("Failed to Extract Header Metadata\n\n", 
             "While parsing a DAT file for ", datType, ", the function ",
             "failed to locate the header strings that indicate the number of ",
             "each column type within the file. These strings were expected ",
             "to appear within the first ", headerCutoff, " lines(s) of the ",
             "file. Please investigate the cause.\n\n", 
             "(This error occurred while reading \"", datPath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
    
    # Create a tibble that separates out the column label and the number
    headerTypes <- tibble(TYPE = headerInfo |> str_extract("[A-Za-z]+"),
                          NUM = headerInfo |> str_extract("[0-9]+") |>
                            as.numeric())
    
    
    # Make sure there are no missing entries in 'headerTypes'
    if (nrow(headerTypes) == 0 || anyNA(headerTypes)) {
      
      paste0("Failed to Extract Header Metadata\n\n", 
             "While parsing a DAT file for ", datType, ", the function ",
             "attempted to extract metadata that indicates the number of ",
             "each column type located within the file. However, it failed ",
             "to extract the column types and their frequencies properly. ",
             "These strings appear within the first ", headerCutoff, 
             " lines(s) of the file. Please investigate the cause.\n\n", 
             "(This error occurred while reading \"", datPath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
    
    # Create headers based on the types of headers and the number of them
    # that appear in the DAT file
    typeHeaders <- map2(headerTypes$TYPE, headerTypes$NUM, 
                        ~ paste0(.x, 1:.y)) |>
      unlist()
    
    
    # Additional headers related to the date and time are still missing
    headers <- c("year", "month", "day", "hour", "minute", "sec",
                 typeHeaders)
    
    
    # Extract the first row after 'headerCutoff' and confirm that the number 
    # of headers matches the number of elements in the row
    
    # (Though, if 'headerCutoff' is at the end of 'datPartial', another row
    #  must be read in for this check)
    if (headerCutoff == length(datPartial)) {
      
      datPartial <- getFile(datPath, fileType = "OTHER", 
                            n_max = headerCutoff + 1)
      
    }
    
    
    # Take the row after 'headerCutoff' and get the number of elements
    numCols <- datPartial[headerCutoff + 1] |>
      str_split("\t") |> unlist() |>
      str_subset("^$", negate = TRUE) |> length()
    
    
    # Raise an exception if 'numCols' does not equal the length of 'headers'
    if (length(headers) != numCols) {
      
      paste0("Header Mismatch\n\n", 
             "While parsing a DAT file for ", datType, ", the function ",
             "extracted column headers from before Line ", headerCutoff, ". ",
             "However, ", length(headers), " header(s) were extracted, while ",
             numCols, " unique column(s) were identified in the subsequent ",
             "line. Please investigate the cause.\n\n", 
             "(This error occurred while reading \"", datPath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
    
    # Next, read in the entirety of the DAT file
    # (Skip the lines through the header cutoff)
    datDF <- getFile(datPath, fileType = "OTHER", skip = headerCutoff)
    
    
    # Split 'datDF' wherever tab spaces occur (and remove empty strings)
    datDF <- datDF |>
      str_split("\t") |> unlist() |>
      str_subset("^$", negate = TRUE)
    
    
    # Make sure the length of 'datDF' is divisible by 'numCols'
    if (length(datDF) %% numCols != 0) {
      
      paste0("Header Mismatch\n\n", 
             "While parsing a DAT file for ", datType, ", the function ",
             "extracted column headers from before Line ", headerCutoff, ". ",
             "However, ", numCols, " header(s) were extracted, while ",
             length(datDF), " values were identified in the subsequent ",
             "rows. This number is not divisible by ", numCols, ", so the ",
             "data cannot be formatted into a tibble. Please investigate ",
             "the cause.\n\n", 
             "(This error occurred while reading \"", datPath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
    
    # Reformat 'datDF' as a matrix and then a tibble
    # After that, apply 'headers' as the column names
    datDF <- datDF |>
      matrix(ncol = numCols, byrow = TRUE) |>
      as_tibble(.name_repair = "minimal") |>
      set_names(headers)
    
    
    # This code runs for RRIHM DAT files
  } else {
    
    # Not all RRIHM DAT files share the exact same format
    
    # The procedure will vary slightly based on observed patterns in 'datPartial'
    
    
    # One type of DAT file has no headers at all (e.g., the "Mark West" one)
    # Its first line starts with values right away
    if (grepl("^[0-9]+\t[0-9]+", datPartial[1])) {
      
      # If there are no headers, generic ones will be created (e.g., "X1")
      
      
      # Get the number of headers by counting the number of tab spaces in 
      # the first line of 'datPartial'
      numHeaders <- str_count(datPartial[1], "\t") + 1
      
      
      # Create headers for the DAT file
      headers <- paste0("X", 1:numHeaders)
      
      
    } else {
      
      paste0("Unknown Type of ", datType, " DAT File\n\n", 
             "While parsing an RRIHM DAT file, the function tried to ",
             "determine its sub-type (e.g., \"Mark West\" DAT style). ",
             "However, it did not match any of the sub-types in this function. ",
             "Please investigate the cause.\n\n",
             "(This error occurred while reading \"", datPath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
    
    # Read in the full DAT file next
    datDF <- getFile(datPath, fileType = "OTHER")
    
    
    # Split 'datDF' at the tab spaces
    datDF <- datDF |>
      strsplit("\t")
    
    
    # Double-check that every line in 'datDF' has a matching number of columns
    if (any(lengths(datDF) != numHeaders)) {
      
      cat("\n\n")
      cat("Line(s) with a Different Number of Columns:\n")
      print(which(lengths(datDF) != numHeaders))
      cat("\n\n")
      
      paste0("Inconsistent Number of Colums\n\n", 
             "While parsing a ", datType, " DAT file, the function tried to ",
             "split the data rows into ", numHeaders, " columns each. However, ",
             "one or more rows had a different number of columns (see the ",
             "indices printed above). Please investigate the cause.\n\n",
             "(This error occurred while reading \"", datPath, "\")") |>
        errWrap() |>
        stop()
      
    }
    
    
    # Reformat 'datDF' into a matrix and then a tibble
    datDF <- datDF |> unlist() |> 
      matrix(ncol = numHeaders, byrow = TRUE) |>
      as_tibble(.name_repair = "minimal") |>
      set_names(headers)
    
    
    # Check if a column contains a "#" followed by a date 
    # (i.e., a commented datestamp)
    commentedDateRegex <- "^#[0-9]{4}-[0-9]{2}-[0-9]{2}$"
    
    
    # Use that to add a "DATE" column to 'datDF'
    if (any(map_lgl(datDF[1, ], ~ grepl(commentedDateRegex, .)))) {
      
      # Locate the column that contains a commented-out date
      # Get the index, and if there are multiple matches, take the first one
      dateCol <- datDF[1, ] |>
        map_lgl(~ grepl(commentedDateRegex, .)) |>
        which() |> head(1)
      
      
      # Define a "DATE" column using the values in that column
      datDF <- datDF |>
        mutate(DATE = get(names(datDF)[dateCol]) |>
                 str_remove("^#") |>
                 as.Date(format = "%Y-%m-%d"))
      
    }
    
  }
  
  
  # Once 'datDF' is formatted into a tibble, try to convert numeric columns
  # into numbers
  
  # Iterate through the columns in 'datDF'
  for (j in 1:ncol(datDF)) {
    
    # Check if at least 90% of a column appear to be numeric
    # If yes, convert the column type
    if (sum(numDetector(datDF[[j]])) > 0.90 * nrow(datDF)) {
      
      datDF[[j]] <- datDF[[j]] |> as.numeric()
      
    }
    
  }
  
  
  # Return 'datDF'
  return(datDF)
  
}



read_gag <- function (gagPath) {
  
  # Read in a ".gag" file as a tibble
  # (These files are outputs from SRP)
  
  
  # Read in the lines of the file
  gagVec <- getFile(gagPath, fileType = "OTHER")
  
  
  # GAG files start with metadata
  
  # Find the actual headers using the "DATA" text string 
  # that starts the header row
  headerRegex <- "\"DATA:"
  
  
  headerLine <- grep(headerRegex, gagVec)
  
  
  # Return an error if 'headerLine' was not found (or if multiple matches were found)
  if (length(headerLine) == 0) {
    
    paste0("GAG Data File - Missing Column Header Issue\n\n", 
           "This script attempted to find the header row in an SRP ",
           "GAG output file. However, the header row could not be ",
           "found.\n\n",
           "There could be data corruption issues, or the formatting of ",
           "the GAG files may have changed. This script may need ",
           "updates, depending on the cause of this problem.\n\n",
           "Please investigate \"", gagPath, "\"") |>
      errWrap() |>
      stop()
    
  } else if (length(headerLine) > 1) {
    
    paste0("GAG Data File - Could Not Identify Column Header\n\n", 
           "This script attempted to find the header row in an SRP ",
           "GAG output file. However, an unusual issue was ",
           "encountered.\n\n",
           "The header row is usually identified via this regular ",
           "expression:\n\n",
           "(*) \"", headerRegex, "\"\n\n",
           "There should be exactly one row in the input file that has ",
           "this pattern. However, more than one match was found.\n\n", 
           "Please investigate \"", gagPath, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there are no issues, remove the metadata from 'gagVec'
  gagVec <- gagVec[headerLine:length(gagVec)]
  
  
  # The actual data of the GAG file is stored in a fixed-width format
  # Remove 'headerRegex' and any quotation marks in the dataset 
  # Then, break apart rows at the spaces
  gagDF <- gagVec |>
    str_remove(headerRegex) |>
    str_remove_all("\"") |>
    trimws() |>
    str_split("\\s+") |> unlist()
  
  
  # Make sure that the length of 'gagDF' is divisible by the length of 'gagVec'
  # If there is no remainder, that means that an equal number of columns 
  # were detected in each row of 'gagVec'
  if (length(gagDF) %% length(gagVec) != 0) {
    
    paste0("GAG Data File - Data Parsing Issue\n\n", 
           "This script attempted to split each row of data in an SRP ",
           "GAG output file. However, a consistent number of columns ",
           "per row could not be identified\n\n",
           "Please investigate \"", gagPath, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Convert 'gagDF' into a matrix and then a data frame
  gagDF <- gagDF |>
    matrix(nrow = length(gagVec), byrow = TRUE) |>
    data.frame()
  
  
  # Use the first row of 'gagDF' as headers
  # Then, reformat 'gagDF' into a tibble
  gagDF <- gagDF[-1, ] |>
    set_names(gagDF[1, ] |> unlist(use.names = FALSE)) |>
    tibble()
  
  
  # Finally, convert columns in 'gagDF' into numeric if they contain numbers
  for (j in 1:ncol(gagDF)) {
    
    # Check if at least 90% of a column match this regular expression
    # If yes, convert the column into numeric
    if (sum(numDetector(gagDF[[j]])) > 0.90 * nrow(gagDF)) {
      
      gagDF[[j]] <- gagDF[[j]] |> as.numeric()
      
    }
    
  }
  
  
  # Return 'gagDF'
  return(gagDF)
  
}


##### File Writing #####

writeOutput <- function (x, outPath, writeFunction = NULL, quietly = FALSE,
                         col_names = TRUE, delim = NA_character_, na = "") {
  
  # Write a variable 'x' to 'outPath'
  
  # Use "write_csv", "write_tsv", "write_delim", "write_xlsx", or "write_lines" 
  # depending on the specification in 'writeFunction'
  
  # 'quietly' is a Boolean for whether an output message will be given
  
  # If 'col_names' is TRUE, column names will be written in the output for 
  # "write_csv" and "write_tsv"
  
  
  # If 'writeFunction' is not specified, infer it from the file extension
  if (is.null(writeFunction)) {
    
    # Guess the type of file using 'outPath'
    fileType <- guessFileType(outPath, delim = delim)
    
    
    # For spreadsheets, use `write_xlsx`
    if (fileType == "XLSX") {
      
      writeFunction <- "write_xlsx"
      
      # For CSV files, use `write_xlsx`
    } else if (fileType == "CSV") {
      
      writeFunction <- "write_csv"
      
      # For TSV files, use `write_tsv`
    } else if (fileType == "TSV") {
      
      writeFunction <- "write_tsv"
      
      # For other delimited files, use `write_delim`
    } else if (fileType == "DELIM") {
      
      writeFunction <- "write_delim"
      
      # For all other file types, use `write_lines`
    } else if (fileType == "OTHER") {
      
      writeFunction <- "write_lines"
      
    } else {
      
      paste0("Unrecognized File Type\n\n",
             "The file type returned by `guessFileType` should have been one of ",
             "these strings: ",
             "\"XLSX\", \"CSV\", \"TSV\", \"DELIM\", or \"OTHER\".\n\n",
             "However, it returned an unknown value '", fileType, "'.") |>
        errWrap() |>
        str_replace_all("\"(.+)\"", paste0("\"", col_green("\\1"), "\"")) |>
        stop()
      
    }
    
  }
  
  
  # If 'writeFunction' is "write_csv" or a similar function, 
  # 'x' has to be a data frame
  if (!is.data.frame(x) && writeFunction %in% c("write_csv", "write_tsv", 
                                                "write_delim", "write_xlsx")) {
    
    stop(paste0("Improper Input For `writeOutput()`\n\n",
                "If `write_csv`, `write_tsv`, `write_delim`, or `write_xlsx` ",
                "will be called to write this output, ",
                "the input variable has to be a data frame. Please revise ",
                "the procedure.\n\n",
                "(Note: Nothing was written to \"", outPath, "\")") |>
           errWrap())
    
  }
  
  
  # Try to apply the file writing functions next
  if (writeFunction == "write_csv") {
    
    writeRes <- try(write_csv(x, outPath, col_names = col_names, na = na))
    
  } else if (writeFunction == "write_lines") {
    
    writeRes <- try(write_lines(x, outPath, na = na))
    
  } else if (writeFunction == "write_xlsx") {
    
    writeRes <- try(writexl::write_xlsx(x, outPath, col_names = col_names))
    
  } else if (writeFunction == "write_tsv") {
    
    writeRes <- try(write_tsv(x, outPath, col_names = col_names, na = na))
    
  } else if (writeFunction == "write_delim") {
    
    writeRes <- try(write_delim(x, outPath, delim = delim, col_names = col_names, na = na))
    
  } else {
    
    stop(paste0("Improper Input For `writeOutput()`\n\n",
                "\"", writeFunction, "\" is not a recognized value for ",
                "the function argument 'writeFunction'. Please revise it.\n\n",
                "\"write_csv\", \"write_tsv\", \"write_delim\", \"write_xlsx\", ",
                "and \"write_lines\" are the only acceptable values.\n\n",
                "(Note: Nothing was written to \"", outPath, "\")") |>
           errWrap())
    
  }
  
  
  # Check for any errors in the process
  if ("try-error" %in% class(writeRes)) {
    
    # Output the actual error message first
    message(writeRes)
    
    
    if (grepl("Cannot open file for writing", writeRes, ignore.case = TRUE)) {
      
      stop(paste0("Inaccessible File Issue\n\n",
                  "The above error message usually occurs if the file already ",
                  "exists and is open in a program like Excel. This prevents ",
                  "the script from overwriting the file.\n\nPlease close '", 
                  filePath, "' and try again.") |>
             errWrap() |>
             str_replace("(open)", col_red("\\1")) |>
             str_replace("(close)", col_green("\\1")))
      
      # In all other cases, output a custom acknowledgement 
    } else {
      
      stop(paste0("Please resolve the error specified above\n\n",
                  "If the issue persists, definitely reach out for assistance") |>
             errWrap())
      
    }
    
  }
  
  
  # As a penultimate step, confirm that the output file was generated
  if (!file.exists(outPath)) {
    
    stop(paste0("File Output Failed\n\n",
                "The output file was not detected in the expected location. ",
                "`", writeFunction, "` may have failed, please investigate ",
                "this issue.\n\n",
                "(Note: Nothing was written to \"", outPath, "\")") |>
           errWrap() |>
           str_replace("(not)", col_red("\\1")) |>
           str_replace("(investigate)", col_green("\\1")))
    
  }
  
  
  # Output a message to the user about the result (if 'quietly' is FALSE)
  if (!quietly) {
    
    cat(paste0("\nWrote data to \"", normalizePath(outPath), "\"!\n\n") |>
          col_cyan())
    
  }
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



copyFile <- function (from, to, overwrite = TRUE, quietly = FALSE) {
  
  # Copy a file to a new location
  
  # This is a wrapper for `file.copy` with a custom error message
  
  
  # First, confirm that the file 'from' actually exists
  if (!file.exists(from)) {
    
    paste0("Attempting to Copy Non-Existent File\n\n",
           "The script attempted to copy a file (\"", from, "\") to a new ",
           "location. However, the original file does not appear to exist! ",
           "Please investigate.\n\n",
           "The intended new file was: \"", to, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there is no issue with 'from', attempt to copy it
  copyRes <- file.copy(from = from, to = to, overwrite = overwrite)
  
  
  # Confirm that the file was copied successfully
  if (!copyRes || !file.exists(to)) {
    
    paste0("Could Not Copy File\n\n",
           "The script attempted to copy a file (\"", from, "\") to a new ",
           "location. However, the processed failed for an unknown reason ",
           "(possibly a permission issue). Please investigate.\n\n",
           "The intended new file was: \"", to, "\"") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Output a message too if 'quietly' is FALSE
  if (!quietly) {
    
    cat(paste0("Copied \"", from, "\" to \"", to, "\"!\n\n") |>
          col_cyan())
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}


##### Cloud-Based Drive Functions #####

makeSharePointPath <- function (filePathFragment) {
  
  # Given 'filePathFragment' (most of the filepath), 
  # write a complete filepath to the file
  
  # 'filePathFragment' should continue from the SharePoint drive onwards 
  # Everything up to the SharePoint directory name (inclusive) will already be 
  # specified by this function
  # The rest of the path is needed as input
  
  # (This function assumes that the SharePoint filepath is 
  #  "C:/Users/[username]/[Initial SharePoint Path String]/...")
  
  return(paste0("C:/Users/", Sys.info()[["user"]], "/", 
                getFromMasterControl("INITIAL_SHAREPOINT_FILE_PORTION"), 
                filePathFragment) |>
           normalizePath(mustWork = FALSE))
  
}



sharepointPathCheck <- function (path, isFolder = FALSE) {
  
  # Check if a file/folder is a SharePoint file/folder
  
  # Use its path ('path') to make this assessment
  
  # If yes, return 'path' as a full SharePoint path
  # Otherwise, just return the path as-is
  
  
  # 'isFolder' is TRUE if the input path is for a folder
  # Otherwise, it should be FALSE for files
  
  
  # Note: 'path' can be either a single file path or a vector of paths
  
  
  # Make a SharePoint version of 'path'
  sharepointPath <- makeSharePointPath(path)
  
  
  # Based on 'isFolder', use either `file.exists` or `dir.exists`
  if (isFolder) {
    
    # Check if the folder exists on SharePoint
    # If yes, return the SharePoint path; otherwise, return the original path
    return(if_else(dir.exists(sharepointPath), sharepointPath, path))
    
  } else {
    
    # Check if the file exists on SharePoint
    # If yes, return the SharePoint path; otherwise, return the original path
    return(if_else(file.exists(sharepointPath), sharepointPath, path))
    
  }
  
}


##### Path Extraction #####

getLatestFile <- function (dir, filePattern, title = "File") {
  
  # Given a directory ('dir'), get the path to a file 
  # that can be identified using the regular expression in 'filePattern'
  
  # The list of files that match 'filePattern' will be sorted and the 
  # last option in the list will be returned
  
  # (This generally corresponds to the most recent version of a file
  #  when sorted by date)
  
  
  # First, check whether 'dir' contains a SharePoint path
  dir <- dir |>
    sharepointPathCheck(isFolder = TRUE)
  
  
  # Get the latest file among those that have the format 'filePattern'
  latestFile <- list.files(dir, full.names = TRUE,
                           pattern = filePattern) |>
    sort() |> tail(1)
  
  
  # Output an error message if no files are found at all
  if (length(latestFile) == 0) {
    
    paste0(title, " Not Found\n\n",
           "The directory \"", dir, "\" does not appear to have any file that ",
           "satisfies this regular expression: \"", filePattern, "\". ",
           "Please make the necessary adjustments and try again.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If there are no issues, return 'latestFile'
  return(latestFile)
  
}


##### File Classification #####

guessFileType <- function (filePath, delim = NULL) {
  
  # Guess the type of file input by the user
  
  # The returned string will be one of these values:
  # "XLSX", "CSV", "TSV", "DELIM", or "OTHER"
  
  
  # If the filepath ends in something akin to ".xlsx", assume it is a spreadsheet
  if (grepl("\\.xls[xm]?$", filePath, ignore.case = TRUE)) {
    
    return("XLSX")
    
    # If the file extension is ".csv", return "CSV"
  } else if (grepl("\\.csv$", filePath, ignore.case = TRUE)) {
    
    return("CSV")
    
    # If the file extension is ".tsv", return "TSV" instead
  } else if (grepl("\\.tsv$", filePath, ignore.case = TRUE)) {  
    
    return("TSV")
    
    # If the filepath has a value for 'delim' specified, assume it is delimited
  } else if (!is.null(delim) && !is.na(delim)) {
    
    return("DELIM")
    
    # For all other cases, return "OTHER"
  } else {
    
    return("OTHER")
    
  }
  
}



guessDAT <- function (datPartial) {
  
  # Try to guess the type of DAT file based on the first couple of lines
  # contained within 'datPartial'
  
  # DAT files can be for PRMS, SRP, and RRIHM
  
  
  # If no tab spaces are detected, it is likely a DAT file for SRP
  if (!any(grepl("\t", datPartial))) {
    
    return("SRP")
    
  }
  
  
  # If there is a line with many hashtags "#" in a row as well as "runoff" columns, 
  # it is probably a DAT file for PRMS
  if (any(grepl("#{2,}", datPartial)) && any(grepl("runoff\\s", datPartial))) {
    
    return("PRMS")
    
    # In all other cases, assume it is a DAT file for RRIHM
  } else {
    
    return("RRIHM")
    
  }
  
}

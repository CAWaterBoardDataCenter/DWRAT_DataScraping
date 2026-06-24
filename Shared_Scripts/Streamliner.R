# Many of these functions condense repetitive actions into function calls

# Procedures that involve an "assumption" use functions from here as well

# That way, if the assumption changes, only one edit is needed in this function
# (as opposed to edits wherever the assumption is applied)


#### Dependencies ####


# This script DOES NOT call all required packages and dependencies

# Please use "Shared_Functions_Importer.R"


#### Functions ####

anyFalse <- function (logVec) {
  
  # Given a logical vector, return TRUE if any of these elements are FALSE
  # (This works with single element logical variables too)
  
  return(!all(logVec))
  
}



errWrap <- function (message, widthRatio = 0.99) {
  
  # Modify the wrapping of an error message 
  # This reduces the need for horizontal scrolling
  return(message |>
           strwrap(width = widthRatio * getOption("width")) |>
           paste0(collapse = "\n"))
  
}



functionStealer <- function (scriptPath, functionName) {
  
  # Extract a function that is present in another script ('scriptPath')
  # Then, load it into the environment
  
  # WARNING
  # The function is expected to use the full notation with curly braces
  # ("{" and "}")
  
  # This function is designed with that formatting in mind
  
  # The curly braces are used to identify the start and end of the function
  
  # If there are comments in the function with "{" or "}", they can cause 
  # this function to fail
  
  
  # First, confirm that 'scriptPath' is valid
  if (!file.exists(scriptPath)) {
    
    paste0("Requested Script Does Not Exist\n\n",
           "\"", scriptPath, "\" does not point to a valid location. ",
           "`functionStealer` requires a valid target script as input for ",
           "'scriptPath'. Please investigate.") |>
      errWrap() |>
      stop()
    
  } else if (!grepl("\\.R$", scriptPath, ignore.case = TRUE)) {
    
    paste0("Target Script Must Be An R Script\n\n",
           "\"", scriptPath, "\" is not an R script (i.e., it does not have the ",
           ".R extension). `functionStealer` requires a valid R script ",
           "as input for 'scriptPath'. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Read the lines of the script
  rLines <- getFile(scriptPath)
  
  
  # Locate the function denoted by 'functionName'
  functionStart <- grep(paste0("^\\s*", str_escape(functionName), 
                               "\\s*(<-)|(=)\\s*function"),
                        rLines, ignore.case = TRUE)
  
  # The regular expression that locates the function has this pattern:
  #  (*) Starts with 0 or more spaces
  #  (*) Contains the name of the function 
  #      (escaped in case there are special characters, 
  #       though that shouldn't happen)
  #  (*) Continues with 0 or more spaces after the function name
  #  (*) Has either "<-" or "=" to define the function
  #  (*) Continues with 0 or more spaces after the operator
  #  (*) Followed by the "function" keyword
  
  
  # If no single function is found, output an error message
  if (length(functionStart) != 1) {
    
    paste0("Function Not Found in Target Script\n\n",
           "\"", scriptPath, "\" was expected to contain exactly one ",
           "function with the name \"", functionName, "\". However, it could ",
           "not be located. This input name gave ", length(functionStart), " ",
           "matches. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The next step is to find the last line of a function
  # This procedure uses an algorithm based on the balance of curly braces
  # Once all open braces "{" are closed by a corresponding "}", assume that 
  # the function code has been completely located
  
  # Define several variables to help with this check: 
  
  
  # The line to check in the loop's current iteration
  checkLine <- functionStart - 1
  
  # The current balance of open and closed braces
  braceImbalance <- 0
  
  # Has the first open brace been found?
  # (This is relevant for functions whose input parameters take up multiple 
  #  lines, causing the first open brace to occur on a line after 'functionStart')
  foundFirstOpen <- FALSE
  
  # Has the end of the function (EOF) been reached?
  notAtEOF <- TRUE
  
  
  # Keep looping while not at the end of the function
  # (and while 'checkLine' has not reached the end of the script yet)
  while (notAtEOF && checkLine < length(rLines)) {
    
    # Check the next line of the script
    checkLine <- checkLine + 1
    
    
    # Store that line of 'rLines' in a temporary character variable
    tempLine <- rLines[checkLine]
    
    
    # If "#" appears in 'tempLine' (and it's not part of a quoted string),
    # remove that portion of 'tempLine' for these checks
    # (Braces within comments should not be included in the counts)
    if (grepl("#", tempLine)) {
      
      # The intention is to remove comments only (which start with "#")
      
      # The main complicating factor is that "#" can also appear in strings
      # (e.g., in filenames)
      
      # Check for several cases and apply different regular expressions 
      
      
      # In the simplest case, there are no quotation marks to worry about
      if (grepl("^[^'\"]*#", tempLine)) {
        
        # In that case, just remove everything that comes after "#"
        # (And then remove "#" as well)
        tempLine <- tempLine |>
          str_extract("^[^'\"]*#") |>
          str_remove("#")
        
        
        # Even if quotation marks are present, if none of them follow "#"
        # Then the comment can be safely removed
      } else if (grepl("^.*#[^'\"]*$", tempLine)) {
        
        # Keep only the portion of 'tempLine' that appears before the 
        # comment "#"
        tempLine <- tempLine |>
          str_replace("^(.*)#[^'\"]*$", "\\1")
        
        # There can be other "#" in this string (matched by ".*"), 
        # but only the "#" that seems to lead a comment is 
        # matched by "#" in the regex
        
        # The limitation of this regex, though, is that no quotation marks
        # can appear within the comment string
        
        # The next two checks will allow comments to have quotes in them
        # However, they have to be either single quote only or double quote only
        
        # If a string contains single quotes (and no double quotes),
        # check for "#" that do not appear between quotes
      } else if (grepl("'", tempLine) && !grepl("\"", tempLine) &&
                 grepl("^[^']*([^']*'[^']*'[^']*)*[^']*#", tempLine)) {
        
        # The regex looks complicated, but the main portion to focus on 
        # is "([^']*'[^']*'[^']*)*"
        
        # This group pattern matches strings that are encased in single quotes
        # (With optional non-single-quote characters able to appear before and 
        #  after the opening and closing of the single quotes)
        
        # Any "#" that appears within quotes will count as 
        # part of that group pattern
        
        # So the "#" at the end of the regex should belong to a comment
        
        tempLine <- tempLine |>
          str_extract("^[^']*([^']*'[^']*'[^']*)*[^']*#") |>
          str_remove("#")
        
        # Here's a more thorough breakdown of the regex:
        
        # "^[^']*([^']*'[^']*'[^']*)*[^']*#"
        
        #  (1) Start looking from the beginning of the string
        
        #  (2) Optionally starts with 0 or more non-single-quote characters
        
        #  (3) Optionally contains 0 or more instances of this group pattern:
        #       (a) Optionally starts with 0 or more non-single-quote characters
        #       (b) A single quote '
        #       (c) Optionally contains 0 or more non-single-quote characters
        #       (d) A single quote '
        #       (e) Optionally followed by 0 or more non-single-quote characters
        
        #  (4) Optionally followed by 0 or more non-single-quote characters
        
        #  (5) A "#"
        
        
        # Repeat the same procedure for instances where the string contains
        # double quotes, but no single quotes
      } else if (grepl("\"", tempLine) && !grepl("'", tempLine) &&
                 grepl("^[^\"]*([^\"]*\"[^\"]*\"[^\"]*)*[^\"]*#", tempLine)) {
        
        tempLine <- tempLine |>
          str_extract("^[^\"]*([^\"]*\"[^\"]*\"[^\"]*)*[^\"]*#") |>
          str_remove("#")
        
        # This regex is essentially the same as the previously described one
        # Just switch single quotes with double quotes
        
        
        # The previous regular expressions cover most target scenarios:
        
        # No Quotes                                        [First Check]
        
        # Single Quotes Before # Only                      [Second Check] 
        # Single Quotes After # Only                       [First Check] 
        # Single Quotes Before & After # Only              [Third Check]
        
        # Double Quotes Before # Only                      [Second Check] 
        # Double Quotes After # Only                       [First Check] 
        # Double Quotes Before & After # Only              [Fourth Check]
        
        # Single &/OR Double Quotes Before # Only          [Second Check] 
        # Single &/OR Double Quotes After # Only           [First Check] 
        # Single &/OR Double Quotes Before & After # Only  [???]
        
        
        # The next check is for strings that have both single and double quotes 
        # In addition, to reach this point, the string should have single and/or 
        # double quotes before AND after the "#"
        # (To make sure this is the worth the effort, the function also confirms
        #  whether 'pattern' may even be present after a "#")
      } else if (grepl("'", tempLine) && grepl("\"", tempLine) &&
                 grepl("#.*[\\(\\)]", tempLine)) {
        
        paste0("Rare Case Issue\n\n",
               "The function was not designed to handle this unusual border ",
               "case. Please investigate the procedure for excluding comments ",
               "(denoted by \"#\") from extracting functions via ",
               "`functionStealer`. (The line was \"", tempLine, "\").") |>
          errWrap() |>
          stop()
        
      }
      
    }
    
    # (The above section prevents braces that appear within a comment 
    #  from being included in the counts)
    
    
    # Count the number of open braces on 'checkLine'
    # Exclude open braces in quotes (if present)
    if (grepl("\".*\\{.*\"", tempLine)) { 
      
      numOpen <- str_count(tempLine, "\\{") - 
        str_count(tempLine |> str_extract("\".*\\{.*\""), "\\{")
      
    } else if (grepl("'.*\\{.*'", tempLine)) { 
      
      numOpen <- str_count(tempLine, "\\{") - 
        str_count(tempLine |> str_extract("'.*\\{.*'"), "\\{")
      
    } else {
      
      numOpen <- str_count(tempLine, "\\{")
      
    }
    
    
    # Check for closed braces on 'checkLine'
    # Exclude any braces in double or single quotes
    if (grepl("\".*\\}.*\"", tempLine)) { 
      
      numClosed <- str_count(tempLine, "\\}") - 
        str_count(tempLine |> str_extract("\".*\\}.*\""), "\\}")
      
    } else if (grepl("'.*\\}.*'", tempLine)) { 
      
      numClosed <- str_count(tempLine, "\\}") - 
        str_count(tempLine |> str_extract("'.*\\}.*'"), "\\}")
      
    } else {
      
      numClosed <- str_count(tempLine, "\\}")
      
    }
    
    
    # Adjust 'braceImbalance'
    braceImbalance <- braceImbalance + numOpen - numClosed
    
    
    # There should never be a negative balance of braces
    # (That is a sign of an error)
    if (braceImbalance < 0) {
      
      paste0("Negative Balance of Curly Braces\n\n",
             "`functionStealer` uses the curly braces (\"{\" and \"}\") to ",
             "identify the bounds of the function. However, this procedure ",
             "failed. From Line ", functionStart, " to Line ", checkLine, ", ",
             "there appear to be more closing braces overall than open braces. ",
             "Please investigate.") |>
        errWrap() |>
        stop()
      
    }
    
    
    # If the first open brace has not yet been found yet, 'foundFirstOpen' is FALSE
    # But, if 'numOpen' is positive, then an open brace has now been found
    # In that case, 'foundFirstOpen' should be set to TRUE
    if (!foundFirstOpen && numOpen > 0) {
      foundFirstOpen <- TRUE
    }
    
    
    # While 'braceImbalance' is not 0 
    # (or the first open curly brace has not yet been found),
    # continue this loop
    notAtEOF <- braceImbalance != 0 || !foundFirstOpen
    
  }
  
  
  # If the loop ended by reaching the end of the file, 
  # and 'notAtEOF' is still TRUE, output an error
  if (notAtEOF) {
    
    paste0("Function Extraction Failed\n\n",
           "`functionStealer` uses the curly braces (\"{\" and \"}\") to ",
           "identify the bounds of the function. However, this procedure ",
           "failed. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The entire function should be contained between 
  # 'functionStart' and 'checkLine'
  # Turn those lines of code in 'rLines' into a single string
  functionStr <- paste0(rLines[functionStart:checkLine], collapse = "\n")
  
  
  # Evaluate 'functionStr' as R code
  source(textConnection(functionStr))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



getModeledWY <- function (endDate) {
  
  # Based on the value of 'endDate', identify the water year being modeled
  # Then, return a vector of dates containing the bounds of that water year
  
  
  # (1) Determining the starting bound:
  
  #     If 'endDate' is between January and September (inclusive), the water year
  #     matches the calendar year in 'endDate'
  
  #     If that is true, then the water year starts on October 1st of the 
  #     preceding calendar year
  
  #     (e.g., if it's 2026-03-09, we are modeling WY2026, which began on 
  #      2025-10-01)
  
  #     If 'endDate' is in the October - December range, the water year is the
  #     calendar year plus one
  
  #     In addition, the start of the water year is October 1st of the same year
  #     as 'endDate'
  
  #     (e.g., if it's 2024-12-12, we are modeling WY2025, which began on 
  #      2024-10-01)
  
  
  # (2) Determining the ending bound:
  
  #     If 'endDate' is between January and September (inclusive), the water year
  #     matches the calendar year in 'endDate'
  
  #     If that is true, then the water year ends on September 30th of the 
  #     current calendar year
  
  #     (e.g., if it's 2026-03-09, we are modeling WY2026, which ends on 
  #      2026-09-30)
  
  #     If 'endDate' is in the October - December range, the water year is the
  #     calendar year plus one
  
  #     In addition, the end of the water year is September 30th of the next year
  #     after the year in 'endDate'
  
  #     (e.g., if it's 2024-12-12, we are modeling WY2025, which ends on 
  #      2025-09-30)
  
  
  # Get the start and end dates of the water year
  # Then, return them as a vector of dates
  return(c(start = if_else(month(endDate) < 10,
                           paste0(year(endDate) - 1, "-10-01"),
                           paste0(year(endDate), "-10-01")),
           end = if_else(month(endDate) < 10,
                         paste0(year(endDate), "-09-30"),
                         paste0(year(endDate) + 1, "-09-30"))) |>
           as.Date(format = "%Y-%m-%d"))
  
}



numDetector <- function (str) {
  
  # Check a string (or vector of strings)
  # Use a regular expression to assess whether the data is numeric
  
  
  return(grepl("^-?[0-9]+(\\.[0-9]+)?([Ee][+-][0-9]+)?$", str))
  
  
  # Explanation of the regex: 
  # "^-?[0-9]+(\\.[0-9]+)?([Ee][+-][0-9]+)?$"
  
  #  (*) The string may start with a minus sign ("-")
  #  (*) The string contains some number of digits (1 or more)
  #  (*) The string may contain a decimal point, followed by more digits
  #  (*) The string may end with scientific notation 
  #      ("e" followed by a plus or minus, and then one or more digits)
  
}



spaceSplit <- function (str) {
  
  # Split a string at spaces
  # Remove empty strings and return the string
  return(str|>
           str_split("\\s") |> unlist(use.names = FALSE) |>
           str_subset("^$", negate = TRUE))
  
}



twoDigitText <- function (num) {
  
  # This function is called when a number is being written to a string
  # If it has only one digit, a zero will be added to the beginning
  
  return(sprintf("%.2d", num))
  
}



vec2QuotedStr <- function (strVec) {
  
  # Given a vector of strings, wrap each element in quotation marks
  # Then, return them in a single string as a list
  
  # Example Strings:
  # '"Element 1", "Element 2", and "Element 3"'
  # '"Element 1" and "Element 2"'
  # '"Element 1"'
  
  
  # First, add quotation marks to each element
  strVec <- paste0("\"", strVec, "\"")
  
  
  # If 'strVec' has only one element, return the string 
  # without any further changes
  if (length(strVec) == 1) {
    return(strVec)
  }
  
  
  # If 'strVec' has only two elements, return a string 
  # with the elements separated by " and "
  if (length(strVec) == 2) {
    
    return(paste0(strVec, collapse = " and "))
    
  }
  
  
  # If 'strVec' has 3 or more elements, separate the elements with commas
  # However, the final element should also have "and" after the comma
  if (length(strVec) > 2) {
    
    strVec[length(strVec)] <- paste0("and ", strVec[length(strVec)])
    
    
    return(paste0(strVec, collapse = ", "))
    
  }
  
}



updateMetadataCSV <- function (dirPath, newCols, filename = "metadata.csv") {
  
  # Update a CSV file containing metadata 
  # Add new columns to it using 'newCols'
  
  # 'newCols' should be a list containing named elements
  
  
  # Start by validating 'newCols'
  # Every element should have a name
  if (is.null(names(newCols)) || "" %in% names(newCols)) {
    
    cat("\n\nNames in 'newCols':\n")
    print(names(newCols))
    
    
    paste0("Improper Value for 'newCols'\n\n",
           "The function `updateMetadataCSV` requires a named list of new ",
           "columns to add to the metadata table. However, the input 'newCols' ",
           "does not have names for each element in the list.\n\n",
           "Please correct this issue and try again.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # In addition, every entry in 'newCols' should only have one element each
  # (One value for each new column)
  if (anyFalse(lengths(newCols) == 1)) {
    
    cat("\n\nNumber of Elements per Entry in 'newCols':\n")
    print(lengths(newCols))
    
    
    paste0("Improper Value for 'newCols'\n\n",
           "The function `updateMetadataCSV` requires a named list of new ",
           "columns to add to the metadata table. Each element of this list ",
           "should have only one value. However, the input 'newCols' does not ",
           "abide by this requirement.\n\n",
           "Please correct this issue and try again.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Next, read in the metadata file
  metaPath <- paste0(dirPath, "/", filename) |>
    normalizePath(mustWork = FALSE)
  
  
  # (The metadata file is likely a CSV file)
  metaDF <- getFile(metaPath)
  
  
  # Add 'newCols to 'metaDF'
  metaDF[names(newCols)] <- newCols
  
  
  # Save 'metaDF'
  writeOutput(metaDF, metaPath, quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}

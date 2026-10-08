# This script adjusts the LSPC input files for each watershed

# These inp files contain model configuration information

# Most importantly, the end date for the model run must be updated (Card 50)

# In addition, the irrigation module must be disabled (Card 201)

# Finally, diversion data should be disabled as well (Card 660)


# To Do:
# Implement setting Cards 30, 31, and 45


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
  cat("Starting 'LSPC_019_Finalize_LSPC_Inputs.R'!\n")
  
  
  # Import the user-specified start and end dates
  source("W3_LSPC_Watershed/scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Import functions from other scripts
  c("get_lspc_model_directories", "validate_lspc_model_folder") |>
    map(~ functionStealer("W3_LSPC_Watershed/scripts/LSPC_017_Check_LSPC_Model_Directory.R", .))
  
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_018_Migrate_Weather_Files.R",
                  "get_lspc_inp_paths")
  
  
  # Read in the LSPC weather control file
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validation of control file
  
  
  # Get the directories for each watershed model folder too
  modelFolder <- controlDF |>
    get_lspc_model_directories()
  
  
  modelFolder |>
    map(validate_lspc_model_folder)
  
  
  # After that, get the paths to LSPC input files for each watershed
  inpPaths <- modelFolder |>
    get_lspc_inp_paths()
  
  
  # Iterate through each input file and ensure that
  cat("\n[1/1]\tUpdating .inp files...\n")
  
  
  # For each watershed, read in its inp file
  
  for (i in 1:nrow(controlDF)) {
    
    # Output the name of the watershed
    cat("\n\n")
    paste0("\t[", i, "/", nrow(controlDF), "] ", controlDF$project_name[i]) |>
      cat()
    cat("\n\n")
    
    
    # Start by reading the inp file for this watershed
    inpLines <- inpPaths[i] |>
      getFile(fileType = "OTHER")
    
    # To Do: Validate inp files
    # Look for a text-based file with cards and comments that start with "c"
    
    
    cat("\t\t[1/3]\tSetting Card 50 (Model Simulation Time Period)...\n")
    
    
    # Perform updates to 'inpLines' in another function
    inpLines <- inpLines |>
      update_card_50(controlDF$start_date[i], endDate)
    
    
    cat("\t\tDone!\n\n")
    
    
    cat("\t\t[2/3]\tSetting Card 201 (Irrigation Application Option Flags)...\n")
    
    # In Card 201, only a single parameter's entry will be updated
    
    # The remaining values should be preserved 
    # (when the irrigation module is actually used, 
    #  different watersheds may have slightly different irrigation configurations)
    
    
    inpLines <- inpLines |>
      update_card_201()
    
    
    cat("\t\tDone!\n\n")
    
    
    cat("\t\t[3/3]\tSetting Card 660 (TMDL Point Source Control)...\n")
    
    # The TMDL Point Source configuration 
    
    inpLines <- inpLines |>
      update_card_660()
    
    cat("\t\tDone!\n\n")
    
    
    # Write 'inpLines' back to its original file
    inpLines |>
      writeOutput(inpPaths[i], writeFunction = "write_lines", sep = "\r\n")
    
    # Note: "\r\n" is the full expression for a new line marker in Windows
    
    # On Unix systems, it's just "\n", and that's the default separator  
    # used by `write_lines` (since it typically renders as expected on Windows)
    
    # However, the LSPC executable file explicitly requires "\r\n"
    # Otherwise, the inp file will fail to parse (without any error message)
    
  }
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_019_Finalize_LSPC_Inputs.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



update_card_50 <- function (inpLines, modelStartDate, modelEndDate) {
  
  # Edit Card 50 in an LSPC inp file
  
  # This card configures the model simulation time period
  
  
  # To begin, locate Card 50 in 'inpLines'
  card50 <- inpLines |>
    find_card_lines_in_inp("^c50 ")
  
  # 'card50' will contain the indices of 'inpLines' where Card 50 is stored
  
  
  # To validate this card, 
  # check for a line that contains these expected columns:
  # c, mstart, mend, deltm, mostart, moend, optlevel
  inpLines[card50] |>
    validate_lspc_inp_card(colNames = paste("c", "mstart", "mend", "deltm", 
                                            "mostart", "moend", "optlevel", 
                                            sep = "\t"),
                           cardNum = 50)
  
  
  # Construct a new string of parameter values for this card
  valString <- paste(
    # c
    "",
    # mstart (Model Run Start Date)
    modelStartDate |> format_date_for_inp(),
    # mend (Model Run End Date)
    modelEndDate |> format_date_for_inp(),
    # deltm (Time Step, in minutes)
    60,
    # mostart (Model Output Start Date)
    (modelStartDate + years(1)) |> format_date_for_inp(), # One year spinup
    # moend (Model Output End Date)
    modelEndDate |> format_date_for_inp(),
    # optlevel (If 1, output is on a daily scale)
    1,
    sep = "\t")
  
  
  # 'valString' will be substituted into this card's editable section of 'inpLines'
  
  # Find the location where this string should be inserted and adjust 'inpLines'
  inpLines <- inpLines |>
    update_lspc_inp_card(card50, valString)
  
  
  # Return 'inpLines' afterwards
  return(inpLines)
  
}



find_card_lines_in_inp <- function (inpLines, cardRegex) {
  
  # This function looks at the text in 'inpLines' 
  # (a vector of an LSPC input file)
  
  # It finds the line numbers related to a specific card
  # (which is referenced by 'cardRegex')
  
  # The returned vector of indices contains the line numbers related to the card
  
  
  # 'cardRegex' should be something like "^c10 "
  # It should be specific to a single card, and it should 
  # match with the first line of that card
  
  
  # Start by using 'cardRegex' to find the start of the card
  cardStart <- inpLines |>
    find_matches(cardRegex)
  
  
  # The next step is to find the end of the card
  # Pretty much all cards end with "c", followed by many hyphens
  
  # Find every instance of this end marker
  # Identify the the first instance that occurs after 'cardStart'
  # That should be the end of this card
  
  
  # Get every matching line
  allCardEnds <- inpLines |>
    find_matches("^c--", maxMatches = Inf)
  
  
  # Extract the first index value that is after 'cardStart'
  cardEnd <- allCardEnds[allCardEnds > cardStart][1]
  
  
  # Return the range from 'cardStart' to 'cardEnd'
  return(cardStart:cardEnd)
  
}



validate_lspc_inp_card <- function (cardLines, colNames, cardNum) {
  
  # Confirm that the text of an LSPC card contains 
  # the expected parameter column names
  
  # 'colNames' should be a string in the same tab-separated format as inp files
  # The expected columns (as well as the starting "c") should be specified here
  
  # 'cardLines' should be a vector containing the text lines of an LSPC card
  # (i.e., a subset of the inp file)
  
  # 'cardNum' is the number of the card (used in the error message only)
  
  
  # Confirm that the columns are exactly as expected
  error_if(colNames %notin% cardLines,
           paste0("Could Not Find Card ", cardNum, " Column Names\n\n",
                  "One line in Card ", cardNum, " should contain header ",
                  "information. The expected string was \"", colNames, 
                  "\". However, it was not found in Card ", cardNum, 
                  ". Please investigate."))
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}



format_date_for_inp <- function (date) {
  
  # Given a date value, convert it into a string for use in an LSPC input file
  
  # The inp files use dates in month-day-year format (mm/dd/yyyy)
  # However, there should be no leading zeros
  
  return(date |> format("%m/%d/%Y") |> 
           str_remove("^0") |> str_replace("/0", "/"))
  
  # The `str_remove` command removes any zero that would appear in the month value
  # The `str_replace` command would remove a leading zero in the day portion
  
}



update_lspc_inp_card <- function (inpLines, cardLines, 
                                  replaceLines, replaceRegex = NA_character_,
                                  maxMatches = NA_real_) {
  
  # Update one or more lines of text in a specific card
  
  # 'inpLines' should be a vector that contains the raw text of an LSPC input file
  
  # 'cardLines' should contain numeric indices that correspond to a specific card
  # within 'inpLines'
  
  # This function will search for the line(s) in a card that contain values
  # These lines will be replaced with the strings stored in 'replaceLines'
  
  # Alternatively, if 'replaceRegex' is provided, a `str_replace` command is used
  # to implement `replaceLines` in the text
  
  # Finally, 'maxMatches' is an optional parameter that provides flexibility 
  # in the validation rules when the number of editable lines is variable
  
  
  # Search through the card text in 'inpLines' for parameter entries
  # (All other types of lines begin with "c", while these ones don't)
  updateIndices <- inpLines[cardLines] |>
    find_matches("^[^c]", 
                 minMatches = length(replaceLines), 
                 maxMatches = max(c(length(replaceLines), maxMatches), na.rm = TRUE))
  
  
  # The number of parameter lines found in the card should match the number of 
  # strings included in 'replaceLines' 
  
  # Alternatively, 'maxMatches' can be specified to set a different limit
  
  
  # Update 'inpLines' with the values in 'replaceLines'
  if (is.na(replaceRegex)[1]) {
    
    inpLines[cardLines[updateIndices]] <- replaceLines
    
    # When 'replaceRegex' is NA, 'replaceLines' is directly substituted into 'inpLines'
    
  } else {
    
    inpLines[cardLines[updateIndices]] <- inpLines[cardLines[updateIndices]] |>
      str_replace(replaceRegex, replaceLines)
    
    # When 'replaceRegex' is given, the existing line is instead edited using 
    # both 'replaceRegex' and 'replaceLines' 
    
  }
  
  
  # Return 'inpLines' after these edits
  return(inpLines)
  
}



update_card_201 <- function (inpLines) {
  
  # Adjust the irrigation application options card in an LSPC inp file
  
  # Update the "irrigfg" flag to disable the irrigation module
  
  
  # To begin, locate Card 201 in 'inpLines'
  card201 <- inpLines |>
    find_card_lines_in_inp("^c201 ")
  
  
  # Confirm that it has the expected column ordering:
  # irrigfg, petfg, monVaryIrrig
  
  # (This function will only update the first parameter to disable the module)
  inpLines[card201] |>
    validate_lspc_inp_card(colNames = paste("c", "irrigfg", "petfg", "monVaryIrrig", 
                                            sep = "\t"),
                           cardNum = 201)
  
  
  # Next, perform an update to the parameter text in this card
  # The first parameter ("irrigfg") should be assigned a value of 0
  # This will disable the irrigation module in the model run
  inpLines <- inpLines |>
    update_lspc_inp_card(card201, 
                         replaceRegex = "^\t[0-9]+(.+)$", replaceLines = "\t0\\1")
  
  # The regular expression in "replaceRegex" looks for an initial tab, followed 
  # by some number of digits. The remainder of the line's text is captured into a 
  # group.
  
  # Then, the replacement ("replaceLines") features an initial tab, the number 
  # zero, and the captured group from the original string
  # (This preserves the watershed model's specific irrigation configurations)
  # (Meanwhile, the 0 at the beginning disables "irrigfg", turning off the module
  #  during the model run)
  
  
  # Return 'inpLines' after this update
  return(inpLines)
  
}



update_card_660 <- function (inpLines) {
  
  # Adjust the TMDL point sources card in an LSPC inp file
  
  # Update the "reduction_flow" column to ignore diversion data in the model run
  
  
  # To begin, locate Card 660 in 'inpLines'
  card660 <- inpLines |>
    find_card_lines_in_inp("^c660 ")
  
  
  # Confirm that it has the expected columns:
  # c, rchid, permit, pipe, reduction_flow, 
  # reduction_qual1, reduction_qual2, reduction_qualn
  
  inpLines[card660] |>
    validate_lspc_inp_card(colNames = 
                             paste("c", "rchid", "permit", "pipe", 
                                   "reduction_flow", "reduction_qual1",
                                   "reduction_qual2", "reduction_qualn", 
                                   sep = "\t"),
                           cardNum = 660)
  
  
  # Next, update the parameter values in this card
  # Only the "reduction_flow" argument must be adjusted
  
  # Unfortunately, there are several challenges with updating this value
  
  # First, this parameter appears towards the middle of the row, meaning that
  # a regular expression must account for prior parameters' values while also
  # preserving later values in the row
  
  # Second, the parameters after "reduction_flow" are optional too, so they may 
  # or may not be present in the strings
  
  # Third, the replacement values for "reduction_flow" must exactly equal
  # the values for "pipe" to disable withdrawals
  
  # Fourth, the number of rows is variable (dependent on the number of water
  # rights present in a watershed's demand dataset)
  
  # Still, the "maxMatches" argument is present in `update_lspc_inp_card` 
  # specifically for cases like Card 660 
  
  # Use that function to update 'inpLines' next
  inpLines <- inpLines |>
    update_lspc_inp_card(card660, 
                         replaceRegex = "^(\t.+?\t.+?\t)(.+?)\t[0-9]+(\\.[0-9]+)?(.*)$", 
                         replaceLines = "\\1\\2\t\\2\\4", 
                         maxMatches = Inf)
  
  # The "reduction_flow" argument in Card 660 appears after the fourth tab space
  # Thus, "replaceRegex" begins with representations of the text before "reduction_flow"
  
  # The string must begin with tab, followed by one or more characters (rchid)
  # This is followed by another tab space, and then one or more characters (permit)
  # The first regex capture group ends with the tab space that precedes "pipe"
  # (This is intentional because the value for "pipe" will be used twice)
  
  # The second set of parentheses contains only the parameter value for "pipe"
  
  # After that, a tab space followed by one or more digits is expected
  # Optionally, there might be a group of digits following a decimal point / period
  # (This would correspond to decimal values in the "reduction_flow" numbers)
  
  # The fourth and final regex group matches with zero or more characters and 
  # covers the rest of the string 
  
  # This matches every character after the "reduction_flow" value 
  # (if there are any--those subsequent parameters are optional)
  
  
  # Something important to note about the first two regex groups is that 
  # their "+" markers are modified to be lazy instead of greedy
  # (i.e., match the bare minimum instead of as much as possible)
  
  # A normal ".+" call would match with more than a single parameter since a tab
  # space can also be matched using "."--the lazy modifier "?" ensures that only
  # one parameter is selected at each interval since "." will stop matching at 
  # the next immediate tab space \t
  
  
  # The value for "replaceLines" is then based on this complicated regex
  
  # The first and second regex capture groups are reused in the replacement string
  # in exactly the same arrangement
  
  # But, after a tab space, the value for "pipe" in the second group is reused
  # as a value for "reduction_flow"
  
  # Finally, the fourth capture group is output (if it's not empty)
  # (that group contains all parameters after "reduction_flow", if they exist)
  
  
  # With this regular expression, any value that is specified for "pipe" 
  # will be reused for "reduction_flow", ensuring that the diversion is disabled
  
  
  # Return 'inpLines' after this update
  return(inpLines)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

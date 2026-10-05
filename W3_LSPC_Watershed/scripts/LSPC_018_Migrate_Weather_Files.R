# This script will move curated weather files to their respective model folders


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
  cat("Starting 'LSPC_018_Migrate_Weather_Files.R'!\n")
  
  
  # Import functions from other scripts
  c("get_lspc_model_directories", "validate_lspc_model_folder") |>
    map(~ functionStealer("W3_LSPC_Watershed/scripts/LSPC_017_Check_LSPC_Model_Directory.R", .))
  
  
  functionStealer("W3_LSPC_Watershed/scripts/LSPC_003_Setup_Project_Directories.R",
                  "read_all_lspc_project_control")
  
  
  # Read in the LSPC weather control file too
  controlDF <- read_lspc_weather_control()
  
  # To Do: Validation of control file
  
  
  # Read in all project control files' "Storage" worksheets after that
  wsDir <- read_all_lspc_project_control(controlDF, worksheet = "Storage")
  
  # To Do:
  # Validate the "Storage" worksheets
  
  
  # Get the directories for each watershed model folder too
  modelFolder <- controlDF |>
    get_lspc_model_directories()
  
  
  modelFolder |>
    map(validate_lspc_model_folder)
  
  
  # After that, get the paths to LSPC input files for each watershed
  inpPaths <- modelFolder |>
    get_lspc_inp_paths()
  
  
  # Gather path information for each watershed
  # The paths to the "curated" folder and the model "Input\Weather" folder are required
  cat("\n[1/2]\tChecking for required items...\n")
  
  # Construct paths to watersheds' "curated" weather folders using 'wsDir'
  curatedDirs <- wsDir |>
    map(~ paste0("W3_LSPC_Watershed/",
                 
                 filter(., scope == "project" & level == "root") |>
                   select(path) |> unlist(use.names = FALSE),
                 "/",
                 filter(., level == "curated") |>
                   select(path) |> unlist(use.names = FALSE)))
  
  # To Do: Confirm that each directory exists
  
  
  # Get the paths to the weather folders in each model directory
  weatherPaths <- paste0(modelFolder, "/Input/Weather")
  
  # To Do: Confirm that each directory exists
  
  
  cat("\tDone!\n\n")
  
  
  cat("[2/2]\tCopying weather files...\n")
  
  
  # For each watershed, check its inp file for information on required weather files
  # Locate these files in the corresponding "curated" folder
  # Copy them to the model directory's weather input folder
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
    
    
    # Extract Card 10 (Weather File Definition) from 'inpLines'
    # This contains the names of required weather files
    reqWeather <- extract_c10(inpLines)
    
    
    # 'reqWeather' contains the names of each weather file,
    # but it lacks the model directory's path information to reach those files
    
    # Add that path information to the beginning of the filenames
    reqWeather <- paste0(weatherPaths[i], "/", reqWeather) 
    
    
    # Next, construct a tibble that contains all required files' names and IDs
    reqWeather <- reqWeather |>
      create_weather_tibble()
    
    
    # Now, 'reqWeather' contains the paths and additional useful information about
    # the required weather files for the watershed model
    
    
    # After that, the generated weather files must be examined
    
    
    # Get a vector of all weather files available in the watershed's curated folder
    curatedFiles <- curatedDirs[[i]] |>
      list.files(recursive = TRUE, full.names = TRUE)
    
    
    # Create a similar tibble as 'reqWeather'
    curatedFiles <- curatedFiles |>
      create_weather_tibble()
    
    
    # The final step is copying weather files from the curated folder to 
    # the actual model directory
    copy_weather_files(reqWeather, curatedFiles)
    
  }
  
  
  cat("\tDone!\n\n")
  
  
  cat(col_green("\n'LSPC_018_Migrate_Weather_Files.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



get_lspc_inp_paths <- function (modelPath) {
  
  # Given the path to one or more watershed's LSPC model directory, 
  # get the path to the LSPC input file
  
  # There should be only one inp file in each directory
  # (this is previously confirmed in validation for the model directories,
  #  but it will be double-checked here as well)
  
  
  # The input file should be located in the model "Input" folder
  # Add "/Input" to the model directory path and search for "inp" files in
  # that location
  inpPath <- modelPath |>
    paste0("/Input") |>
    map(~ list.files(., pattern = "\\.inp$", full.names = TRUE)) |> 
    unlist() 
  
  # Even if 'modelPath' contains multiple paths, `map` will ensure that 
  # each directory will be examined for input files
  
  
  # Confirm that the number of inp paths in 'inpPath' match the number of model paths
  error_if(length(modelPath) != length(inpPath),
           paste0("Mismatch of LSPC Input Files\n\n",
                  "The expected number of .inp files that should have been ",
                  "detcted is ", length(modelPath), ". However, the number of ",
                  "files found is ", length(inpPath), ". Please investigate."))
  
  
  # If there are no issues, return 'inpPath'
  return(inpPath)
  
}



extract_c10 <- function (inpLines) {
  
  # Given the lines of text from an LSPC input file, 
  # extract the weather file information stored in Card 10
  
  
  # Shorten 'inpLines' to just contain the text lines related to Card 10
  inpLines <- extract_lspc_inp_card(inpLines, "^c10 ")
  
  
  # Next, remove all comment lines from 'inpLines'
  # All of these lines begin with "c"
  inpLines <- inpLines |>
    str_subset("^c", negate = TRUE)
  
  
  # All of the remaining lines contain the names of weather files
  
  # However, they are encased within tab-separated lines of text 
  # that also specify other information
  
  # Split out each line using the tab separators
  # Then extract the weather filenames 
  weatherFiles <- inpLines |>
    str_split("\t") |> unlist() |>
    str_subset("\\.((air)|(pre))$")
  
  # 'weatherFiles' contains a vector of strings that represent the filenames 
  # of "air" and "pre" weather files
  
  
  error_if(length(weatherFiles) == 0 | anyNA(weatherFiles),
           paste0("Could Not Extract Information from Input File\n\n",
                  "The LSPC input file should contain weather file information ",
                  "in Card 10 (c10). However, issues were encountered ",
                  "when extracting this information. Please investigate."))
  
  
  # Return 'weatherFiles'
  return(weatherFiles)
  
  
}



extract_lspc_inp_card <- function (inpLines, cardRegex) {
  
  # This function takes a subset of 'inpLines' 
  # (a vector of text from an LSPC input file)
  
  # It extract the lines related to a specific card
  # (which is referenced by 'cardRegex')
  
  # The returned vector of strings only contains values from 'inpLines' 
  # that are related to that card
  
  
  # 'cardRegex' should be something like "^c10 "
  # It should be specific to a single card, and it should 
  # match with the first line of that card
  
  
  # Begin by applying 'cardRegex' to identify the start of the model configuration card
  cardStart <- inpLines |>
    find_matches(cardRegex)
  
  
  # Shorten 'inpLines' to begin at the index denoted by 'cardStart'
  cardLines <- inpLines[cardStart:length(inpLines)]
  
  
  # Find the end of this card next
  # Essentially all cards end with a line that contains "c" and many hyphens
  cardEnd <- cardLines |>
    find_matches("^c--", maxMatches = Inf) |>
    head(1)
  
  # With 'inpLines' adjusted to start at the beginning of the card, 
  # the first instance of "c--" marks the end of that card
  
  
  # Remove all lines that appear after 'cardEnd'
  cardLines <- cardLines[1:cardEnd]
  
  
  # Return the shortened 'cardLines' afterwards
  return(cardLines)
  
}



create_weather_tibble <- function (weatherPaths) {
  
  # Given a vector of paths for weather files, construct a tibble
  
  # In addition to the paths, this tibble will contain information on the 
  # IDs featured in the filenames, as well as the type of weather file
  
  
  # First, define a new tibble
  # The first column will be "PATH", and its values will be the values
  # specified in 'weatherPaths'
  weatherDF <- tibble(PATH = weatherPaths)
  
  
  # Define three additional columns:
  #   (*) FILENAME - The weather file's name contained within "PATH"
  #   (*) ID - The numeric ID that appears in weather files' names
  #   (*) TYPE - "AIR" or "PRE" (derived from the weather files' extensions)
  weatherDF <- weatherDF |>
    mutate(FILENAME = extract_filename(PATH),
           ID = FILENAME |> str_extract("^[0-9]+") |> as.numeric(),
           TYPE = if_else(grepl("\\.air$", FILENAME), "AIR", "PRE"))
  
  
  # Make sure there are no missing values
  error_if(anyNA(weatherDF),
           paste0("Failed to Parse Weather File Information\n\n",
                  "The list of weather files for this watershed could not be ",
                  "parsed correctly. Please investigate the files."))
  
  
  # Return 'weatherDF'
  return(weatherDF)
  
}



copy_weather_files <- function (requiredDF, availableDF) {
  
  # Replace weather files noted in 'requiredDF' with their counterparts
  # in 'availableDF'
  
  # In some cases, the appropriate counterpart is not immediately apparent
  
  
  # Iterate through each of the files in 'requiredDF'
  for (j in 1:nrow(requiredDF)) {
    
    
    # Check if the exact same filename in 'requiredDF' appears in 'availableDF'
    if (requiredDF$FILENAME[j] %in% availableDF$FILENAME) {
      
      # Identify the available weather file that matches the required file
      matchIndex <- which(availableDF$FILENAME == requiredDF$FILENAME[j])
      
      
      # Make sure exactly one match is found only
      error_if(length(matchIndex) > 1,
               paste0("Multiple Weather File Matches?\n\n",
                      "Among the available curated files, more than one file ",
                      "matched exactly with \"", requiredDF$FILENAME[j], "\". ",
                      "Please investigate."))
      
      
      # Replace the required file with its newer version in 'availableDF'
      copyFile(availableDF$PATH[matchIndex], requiredDF$PATH[j],
               overwrite = TRUE)
      
    } else {
      
      # If no exact match is found, try to find a similar counterpart
      
      # It is possible that the Python weather scripts output slightly different
      # filenames for this weather file
      
      
      # Look for files in 'availableDF' with the same ID and TYPE
      availableMatches <- availableDF |>
        filter(ID == requiredDF$ID[j] & TYPE == requiredDF$TYPE[j])
      
      
      # Output an error message if 'availableMatches' is empty
      error_if(nrow(availableMatches) == 0,
               paste0("Could Not Find Curated Counterpart File\n\n",
                      "\"", requiredDF$FILENAME[j], "\" is a required weather ",
                      "file for the watershed model. However, no curated file ",
                      "with the same ID and type could be found. Please investigate."))
      
      
      # Check if the required file contains an underscore in its filename
      # If there is exactly one available file that also contains an underscore,
      # assume that they are the equivalent matches
      if (grepl("_", requiredDF$FILENAME[j]) &&
          sum(grepl("_", availableMatches$FILENAME)) == 1) {
        
        # Get the index of the file in 'availableMatches' that also has an underscore
        matchIndex <- which(grepl("_", availableMatches$FILENAME))
        
        
        # Copy over that file to the model directory
        copyFile(availableMatches$PATH[matchIndex], requiredDF$PATH[j],
                 overwrite = TRUE)
        
        
      # In all other cases, output an error message
      } else {
        
        paste0("Could Not Determine Equivalent File\n\n",
               "No weather file in the curated directory contains the exact ",
               "same filename as \"", requiredDF$FILENAME[j], "\". However, ",
               "at least one file has the same ID and type. But there is no ",
               "certainty that the two files are equivalent. Please investigate ",
               "and update the script if they really are counterparts.") |>
          stop_script()
        
      }
      
    }
    
  } # End of loop through required weather files
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

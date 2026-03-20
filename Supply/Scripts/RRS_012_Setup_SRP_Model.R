# Prepare the SRP files for a model run

# The model files will be copied from another location
# to the "ProcessedData" folder

# The source location is specified in the field "RR_SRP_SOURCE_LOCATION" 
# in "RR_Supply_Control_File.xlsx"



#### Setup ####

# Clear the environment
remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRS_012_Setup_SRP_Model.R'!\n\n")
  
  
  # Get the location of the SRP model files
  sourceDir <- getFromSupplyControl_RR("RR_SRP_SOURCE_LOCATION")
  
  
  # Validate the user's input and ensure that this directory contains
  # all required components
  # (Also, if the directory is on SharePoint, 'sourceDir' will be adjusted
  #  to reflect that)
  sourceDir <- validateDirectory(sourceDir, "RR_SRP_SOURCE_LOCATION")
  
  
  cat("[1/1]\tCopying the SRP folder to \"ProcessedData/SRPHM_update_ag\"...\n")
  
  
  # Copy the contents from 'sourceDir' to a new SRP folder in "ProcessedData"
  copyModel(sourceDir)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRS_012_Setup_SRP_Model.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



validateDirectory <- function (sourceDir, sourceField) {
  
  # Ensure that the directory provided by the user is valid
  
  # Also, 'sourceDir' could contain either a local path or 
  # a partial SharePoint path
  # This function will help clarify which type of path it is
  
  
  # Start by checking if the directory is a SharePoint fragment
  # If it exists on SharePoint, convert 'sourceDir' into a SharePoint path
  if (dir.exists(makeSharePointPath(sourceDir))) {
    
    sourceDir <- makeSharePointPath(sourceDir)
    
  }
  
  
  # If the directory cannot be found, notify the user
  if (!dir.exists(sourceDir)) {
    
    stop(paste0("Cannot Find the Specified SRP Directory\n\n",
                "In the RR Supply Control File, the location of the SRP model ",
                "files was specified in \"", sourceField, "\". ",
                "However, \"", sourceDir, "\" does not appear to exist.\n\n",
                "Please correct the value specified for \"", sourceField,
                "\" in the control spreadsheet.") |>
           errWrap())
    
  }
  
  
  # In the root directory of the SRP folder, there should be several sub-folders:
  # "basin", "External Files", "input", "nsub", and "output"
  expectedFolders <- c("basin", "External Files", "input", "nsub", "output")
  
  
  if (anyFalse(expectedFolders %in% list.files(sourceDir))) {
    
    stop(paste0("Missing Components in the SRP Model Folder\n\n",
                "In the RR Supply Control File, the location of the SRP model ",
                "files was set to be \"", sourceDir, "\"\n\n", 
                "However, this directory does not contain all of the expected ",
                "folders that would be present in a proper installation of SRP ", 
                "(", vec2QuotedStr(expectedFolders), "). Please ",
                "obtain a proper installation of SRP and/or correct the value ",
                "given in the control spreadsheet for \"", sourceField, "\".") |>
           errWrap())
    
  }
  
  
  # Next, get a list of all files that appear in 'sourceDir'
  sourceFiles <- list.files(sourceDir, recursive = TRUE)
  
  
  # Ensure that certain key files are included in 'sourceFiles'
  reqFiles <- c("SRPHM_update.control", "Run_updated_Model.bat",
                "gsflow_ag.exe")
  
  
  if (anyFalse(reqFiles %in% sourceFiles)) {
    
    missingFiles <- which(!(reqFiles %in% sourceFiles))
    
    
    stop(paste0("Missing Files in the SRP Model Folder\n\n",
                "In the RR Supply Control File, the location of the SRP model ",
                "files was set to be \"", sourceDir, "\"\n\n", 
                "However, this directory lacks ", length(missingFiles), " key ",
                "component", if_else(length(missingFiles) > 1, "s", ""), " (",
                vec2QuotedStr(reqFiles[missingFiles]), ").\n\n",
                "Please obtain a proper installation of SRP and/or correct the ",
                "value given in the control spreadsheet for \"", sourceField, 
                "\".") |>
           errWrap())
    
  }
  
  
  # Return 'sourceDir' if there are no issues
  # (If 'sourceDir' points to a SharePoint location, it has been updated 
  #  in this function to reflect that)
  return(sourceDir |> normalizePath())
  
}



copyModel <- function (sourceDir) {
  
  # Copy the files from 'sourceDir' into a newly created "SRPHM_update_ag" folder
  # in the "ProcessedData" folder
  
  
  # 'newDir' will contain the new folder location 
  # relative to the working directory
  newDir <- "ProcessedData/SRPHM_update_ag"
  
  
  # If the folder already exists, delete it and its contents
  if (dir.exists(newDir)) {
    
    unlink(newDir, recursive = TRUE)
    
  }
  
  
  # Next, create the "SRPHM_update_ag" folder
  dir.create(newDir)
  
  
  # Copy the entire contents of 'sourceDir' into this new folder
  dir_copy(sourceDir, newDir, overwrite = TRUE)
  
  
  # Side note: It doesn't matter if the source folder has a name that's 
  # different from "SRPHM_update_ag"
  # In "ProcessedData", the folder will still be called "SRPHM_update_ag"
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
remove(list = ls())

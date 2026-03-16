# Prepare the PRMS files for a model run

# The model files will be copied from another location
# to the "ProcessedData" folder

# The source location is specified in the field "RR_PRMS_SOURCE_LOCATION" 
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
  cat("Starting 'RRS_007_Setup_PRMS_Model.R'!\n\n")
  
  
  # Get the location of the PRMS model files
  sourceDir <- getFromSupplyControl_RR("RR_PRMS_SOURCE_LOCATION")
  
  
  # Validate the user's input and ensure that this directory contains
  # all required components
  # (Also, if the directory is on SharePoint, 'sourceDir' will be adjusted
  #  to reflect that)
  sourceDir <- validateDirectory(sourceDir, "RR_PRMS_SOURCE_LOCATION")
  
  
  cat("[1/1]\tCopying the PRMS folder to \"ProcessedData/RR_PRMS\"...\n")
  
  
  # Copy the contents from 'sourceDir' to a new "RR_PRMS" folder in "ProcessedData"
  copyModel(sourceDir)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRS_007_Setup_PRMS_Model.R' is complete!\n\n"))
  
  
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
    
    stop(paste0("Cannot Find the Specified PRMS Directory\n\n",
                "In the RR Supply Control File, the location of the PRMS model ",
                "files was specified in \"", sourceField, "\". ",
                "However, \"", sourceDir, "\" does not appear to exist.\n\n",
                "Please correct the value specified for \"", sourceField,
                "\" in the control spreadsheet.") |>
           errWrap())
    
  }
  
  
  # In the root directory of the PRMS folder, there should be three sub-folders:
  # "bin", "PRMS", and "windows"
  if (anyFalse(c("bin", "PRMS", "windows") %in% list.files(sourceDir))) {
    
    stop(paste0("Missing Components in the PRMS Model Folder\n\n",
                "In the RR Supply Control File, the location of the PRMS model ",
                "files was set to be \"", sourceDir, "\"\n\n", 
                "However, this directory lacks the three folders present in a ",
                "proper installation of PRMS (", 
                vec2QuotedStr(c("bin", "PRMS", "windows")), "). Please ",
                "obtain a proper installation of PRMS and/or correct the value ",
                "given in the control spreadsheet for \"", sourceField, "\".") |>
           errWrap())
    
  }
  
  
  # After that, confirm that the "climate_scenarios" input folder exists 
  # in this model installation
  if (!dir.exists(paste0(sourceDir, "/PRMS/input/climate_scenarios"))) {
    
    stop(paste0("Missing Components in the PRMS Model Folder\n\n",
                "In the RR Supply Control File, the location of the PRMS model ",
                "files was set to be \"", sourceDir, "\"\n\n", 
                "However, this directory lacks an important folder that should ",
                "be present among the input folders (\"PRMS/input/climate_",
                "scenarios\"). Please obtain a proper installation of PRMS ",
                "and/or correct the value given in the control spreadsheet ",
                "for \"", sourceField, "\".") |>
           errWrap())
    
  }
  
  
  
  # Next, get a list of all files that appear in 'sourceDir'
  sourceFiles <- list.files(sourceDir, recursive = TRUE)
  
  
  # Ensure that certain key files are included in 'sourceFiles'
  reqFiles <- c("bin/gsflow.exe", "windows/prms_rr.control",
                "windows/run.bat")
  
  
  if (anyFalse(reqFiles %in% sourceFiles)) {
    
    missingFiles <- which(!(reqFiles %in% sourceFiles))
    
    
    stop(paste0("Missing Files in the PRMS Model Folder\n\n",
                "In the RR Supply Control File, the location of the PRMS model ",
                "files was set to be \"", sourceDir, "\"\n\n", 
                "However, this directory lacks ", length(missingFiles), " key ",
                "component", if_else(length(missingFiles) > 1, "s", ""), " (",
                vec2QuotedStr(reqFiles[missingFiles]), ").\n\n",
                "Please obtain a proper installation of PRMS and/or correct the ",
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
  
  # Copy the files from 'sourceDir' into a newly created "RR_PRMS" folder in
  # the "ProcessedData" folder
  
  
  # 'newDir' will contain the new folder location relative to the working directory
  newDir <- "ProcessedData/RR_PRMS"
  
  
  # If the folder already exists, delete it and its contents
  if (dir.exists(newDir)) {
    
    unlink(newDir, recursive = TRUE)
    
  }
  
  
  # Next, create the "RR_PRMS" folder
  dir.create(newDir)
  
  
  # Copy the entire contents of 'sourceDir' into this new folder
  dir_copy(sourceDir, newDir, overwrite = TRUE)
  
  
  # Side note: It doesn't matter if the source folder has a name that's 
  # different from "RR_PRMS"
  # In "ProcessedData", the folder will still be called "RR_PRMS"
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
remove(list = ls())

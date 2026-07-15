# The purpose of this script is to generate catchment HTML maps for all 
# 17 Paradigm watersheds instead of running the catchment_QAQC.R script 17 times 
# individually!

# Last Updated By: Payman Alemi and Aakash Prashar on 7/14/2026


#### SETUP ####

# Clear the environment
base::remove(list = ls())


# Shared functions and required packages
source("Shared_Scripts/!Shared_Functions_Importer.R")


#### PROCEDURE ####

# Load in the main control file for the Demand Workflow
# It can either be a SharePoint version or a local copy

# For SharePoint paths to be usable, both "INITIAL_SHAREPOINT_FILE_PORTION"
# and "SHAREPOINT_DEMAND_CONTROL_FILE" must be specified in 
# "Master_Control_File.xlsx"
if (!is.na(getFromMasterControl("INITIAL_SHAREPOINT_FILE_PORTION"))) {
  
  # Try and read the SharePoint fragment for the Demand control file
  controlPath <- getFromMasterControl("SHAREPOINT_DEMAND_CONTROL_FILE")
  
  
  # If that value is indeed specified, read it in as 'ctrlDF'
  if (!is.na(controlPath)) {
    
    ctrlDF <- controlPath |>
      makeSharePointPath() |>
      getXLSX(worksheet = "Main_Sheet", skip = 1)
    
  }
  
}


# In all other cases, use the local version of the control file
if (!exists("ctrlDF")) {
  
  controlPath <- "W1_Watershed_Demand/Input/Watershed_Demand_Dataset_Paths.xlsx"
  
  ctrlDF <- getXLSX(controlPath, worksheet = "Main_Sheet", skip = 1)
  
}


# Watersheds to run the script for
sdaIDs <- c("BIGC", "BUTC", "GUAL", "MATT", "NAPA", "NAVR", "PESC", "PETA", "PUTC", 
            "SALM", "SGRO", "SLOP", "SLZO", "SONO", "STMA", "TODB", "TRIN")


# Filter down 'ctrlDF'
ctrlDF <- ctrlDF |>
  filter(ID %in% sdaIDs)


# Make sure every field has a catchment path specified
stopifnot(!anyNA(ctrlDF$SUBBASIN_POLYGONS_DATABASE_PATH))


# Function stealer
# Borrow functions for calling other R scripts without environment-clearing commands
c("toggleAndRunScript", "toggleRemoveFunctions") |>
  map(~ functionStealer("W2_Russian_River/Scripts/HLP_008_Update_Main_DAT_and_Historic_Precip_Files.R", .))


# Loop through every watershed
for (i in 1:nrow(ctrlDF)) {
  
  source("Shared_Scripts/!Shared_Functions_Importer.R")
  
  # Read in "Watershed_Selection.R"
  selLines <- getFile("W1_Watershed_Demand/Scripts/Watershed_Selection.R")
  
  # Update 'index'
  selLines <- selLines |>
    str_replace("^index <- [0-9]+(.*)", paste0("index <- ", ctrlDF$INDEX[i], "\\1"))
  
  # Write 'selLines' back to "Watershed_Selection.R"
  writeOutput(selLines, "W1_Watershed_Demand/Scripts/Watershed_Selection.R")
  
  
  # Run the catchment QAQC script
  toggleAndRunScript("W1_Watershed_Demand/Scripts/Catchment_QAQC.R")
  
}


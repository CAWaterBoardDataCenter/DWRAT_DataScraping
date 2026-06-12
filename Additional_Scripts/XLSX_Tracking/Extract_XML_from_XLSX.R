# Many XLSX files are used in SDA workflows

# However, their contents cannot be parsed and tracked by git


# Before committing changes to spreadsheets, run this script

# The underlying XML content is extracted for these spreadsheets

# Changes in the spreadsheets are reflected in these files
# (And changes to the XML files can be tracked via git)


#### Setup ####


# Clear the environment
base::remove(list = ls())


# Import functions
require(cli)
require(stringr)
require(fs)


#### Procedure ####


# Startup message
cat("\n\n")
cat("Starting 'Extract_XML_from_XLSX.R'...")
cat("\n\n")


# Make sure the current working directory matches the repository's base location
if (!grepl("[/\\\\]DWRAT_DataScraping[/\\\\]?$", normalizePath(getwd()))) {
  stop("Please use this script with the repository's R project active")
}


# Then, locate every ".xlsx" file in the repository
xlsxList <- list.files(pattern = "\\.xlsx$", 
                       recursive = TRUE, full.names = TRUE)


# Exclude certain matches:
# No spreadsheets contained within an "Archive" folder
# No spreadsheets in a "Documentation" folder
# Ignore "Russian_River_Database_2022.xlsx"
xlsxList <- xlsxList |>
  str_subset("/Archive/", negate = TRUE) |>
  str_subset("/Documentation/", negate = TRUE) |>
  str_subset("/RUSSIAN_RIVER_DATABASE_2022\\.xlsx$", negate = TRUE)


# Make sure 'xlsxList' is NOT empty
if (length(xlsxList) == 0) {
  stop("No spreadsheets detected")
}


# Iterate through the spreadsheets next
for (i in 1:length(xlsxList)) {
  
  # Output a message
  paste0("\n\n[", i, "/", length(xlsxList), "] Archiving \"",
         xlsxList[i], "\"...\n\n") |>
    cat()
  
  
  # Make a corresponding ZIP filename for the spreadsheet
  zipPath <- xlsxList[i] |> 
    str_remove("^.+[/\\\\]") |>
    str_replace("\\.xlsx$", ".zip")
  
  
  # Save the ZIP file to this script's directory ("XLSX_Tracking")
  zipPath <- paste0("Additional_Scripts/XLSX_Tracking/", zipPath)
  
  
  # Copy the spreadsheet as a ZIP folder
  file.copy(from = xlsxList[i], to = zipPath, 
            overwrite = TRUE)
  
  
  # A new folder will be created using the ZIP file given by 'zipPath'
  newFolderPath <- zipPath |>
    str_remove("\\.zip$")
  
  
  # If it already exists, delete it
  if (dir.exists(newFolderPath)) {
    dir_delete(newFolderPath)
  }
  
  
  # Unzip the new ZIP folder and leaves its extracted contents as a new folder
  unzip(zipPath, exdir = newFolderPath, overwrite = TRUE)
  
  
  # Finally, remove the ZIP folder
  unlink(zipPath)
  
}


# Output a completion message
"\n\n'Extract_XML_from_XLSX.R' is complete!\n\n" |>
  col_green() |>
  cat()


# Clear the environment
remove(list = ls())

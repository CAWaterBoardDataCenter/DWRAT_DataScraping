# Before committing changes to spreadsheets in the GitHub repo, run this script

# Some useful XML content is extracted for these spreadsheets
# Changes in the spreadsheets are reflected in these files
# (And changes to the XML files can be tracked via git)



#### Procedure ####

# Define a vector of all relevant spreadsheets in the repo
# (Relative to the "Demand" project directory)
xlsxFiles <- c("InputData/Watershed_Demand_Dataset_Paths.xlsx",
               "../Repo_Control_File.xlsx")


# Iterate through the spreadsheets
for (i in 1:length(xlsxFiles)) {
  
  # Make a corresponding ZIP file path for the spreadsheet
  zipPath <- xlsxFiles[i] %>% str_replace("\\.xlsx?", ".zip")
  
  
  # Copy the spreadsheet as a ZIP folder
  file.copy(from = xlsxFiles[i],
            to = zipPath, 
            overwrite = TRUE)
  
  
  # Unzip the new ZIP folder, extracting only two XML files
  # ("sharedStrings.xml" and "sheet1.xml")
  # These files are placed in the same directory as the XLSX and ZIP files
  unzip(zipPath, exdir = str_extract(xlsxFiles[i], "^.+?/"),
        files = c("xl/sharedStrings.xml", "xl/worksheets/sheet1.xml"), junkpaths = TRUE)
  
  
  # Rename the XML files to reference their original source spreadsheets
  file.rename(zipPath %>% str_replace("/.+?$", "/sharedStrings.xml"),
              zipPath %>% str_replace("\\.zip", "_sharedStrings.xml"))
  
  
  file.rename(zipPath %>% str_replace("/.+?$", "/sheet1.xml"),
              zipPath %>% str_replace("\\.zip", "_sheet1.xml"))
  
  
  # Finally, remove the ZIP folder
  file.remove(zipPath)
  
}


# Clear the environment
remove(list = ls())

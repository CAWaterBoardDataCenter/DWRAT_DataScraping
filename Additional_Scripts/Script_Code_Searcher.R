# Search a directory for a certain type of text-based file

# Then, look for a specified string in all of the matched files

# Note: Regular expressions are used in this procedure
#       https://rstudio.github.io/cheatsheets/regex.pdf


base::remove(list = ls())


# Regex for what to search for in each matching file
searchStr <- "no-qa"


# Get a list of files with a certain extension
fileList <- list.files("./", pattern = "\\.R$", 
                       full.names = TRUE, recursive = TRUE)


# Iterate through the list of files
for (i in 1:length(fileList)) {
  
  # Read in the file
  tempRead <- readLines(scriptList[i])
  
  
  # Print out the script name if 'searchStr' is found inside the script text
  if (any(grepl(searchStr, tempRead, ignore.case = TRUE))) {
    
    print(scriptList[i])
    
  }
  
}


# Clear the environment afterwards
base::remove(list = ls())

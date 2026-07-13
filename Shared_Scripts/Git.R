# These functions rely on `git` commands to perform different actions

# (Note: As a result, the user's computer must allow the use of `git` commands
#        via Command Prompt)


#### Dependencies ####


# This script DOES NOT call all required packages and dependencies

# Please use "!Shared_Functions_Importer.R"


#### Functions ####

getGitHash <- function () {
  
  # Generate a temporary batch file that calls "git rev-parse"
  # to get the short hash of the current commit in the repository
  
  # Obtain the hash string from that command and return it
  
  
  # First, start by generating a temporary batch file
  # Save it into the current repository location
  tempName <- "temp_git_check.bat"
  
  
  c("git rev-parse --short HEAD",
    "exit") |>
    writeOutput(tempName, writeFunction = "write_lines", quietly = TRUE)
  
  
  # Execute the batch file
  hashRes <- system(tempName, intern = TRUE)
  
  
  # The hash will be located on a line by itself after the "rev-parse" call
  hashLoc <- grep("rev-parse", hashRes)[1] + 1
  
  
  # Check for errors at this point
  if (length(hashLoc) == 0 || is.na(hashLoc) || 
      any(grepl("Error", hashRes, ignore.case = TRUE))) {
    
    print(hashRes)
    
    paste0("Git Call Failed\n\n",
           "An error occurred when running a batch script that contanied ",
           "a git command (\"", normalizePath(tempName, mustWork = TRUE), 
           "\"). The results were printed above. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If no issues occurred, extract the hash string from 'hashRes'
  hashRes <- hashRes[hashLoc]
  
  
  # Finally, delete the temporary batch file
  unlink(tempName)
  
  
  # Return the short commit hash
  return(hashRes)
  
}

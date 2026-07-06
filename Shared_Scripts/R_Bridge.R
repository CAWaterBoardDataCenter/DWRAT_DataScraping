# This script supports the use of Command Prompt to run R scripts


#### Dependencies ####


# This script DOES NOT call all required packages and dependencies

# Please use "!Shared_Functions_Importer.R"


#### Functions ####

detectRScriptExe <- function () {
  
  # Look for "Rscript.exe" in the active installation of R
  # (It should be in the "bin" folder)
  
  # Return a path to that exe file
  
  
  # Check the expected location of "Rscript.exe"
  exePath <- paste0(R.home(), "/bin/Rscript.exe") |>
    normalizePath(mustWork = FALSE)
  
  
  # Confirm that 'exePath' exists
  if (!file.exists(exePath)) {
    
    paste0("RScript.Exe Not Found\n\n",
           "This procedure requires the executable version of R. However, \"",
           exePath, "\" did not point to a valid file. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return 'exePath' if there are no issues
  return(exePath)
  
}

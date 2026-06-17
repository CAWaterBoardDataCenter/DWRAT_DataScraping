# These functions help interface this repository's R scripts with Python scripts
# (via Anaconda)


#### Dependencies ####


# This script DOES NOT call all required packages and dependencies

# Please use "Shared_Functions_Importer.R"


#### Functions ####

detectAnacondaBat <- function () {
  
  # Locate an installation of "Anaconda" on the user's device
  # Get the path to "activate.bat", which is located under "Scripts"
  
  # Return that path as a string to the user
  
  
  # First, check for for "Anaconda" in the "ProgramData" folder
  anacondaInstallation <- list.files("C:/ProgramData", pattern = "[Aa]naconda",
                                     full.names = TRUE) |>
    sort() |> tail(1)
  
  
  # If no match was found, throw an error
  if (length(anacondaInstallation) == 0) {
    
    paste0("Anaconda Not Found\n\n",
           "This procedure requires an installation of Anaconda. However, ",
           "the program was not found. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Otherwise, look for "activate.bat"
  batPath <- paste0(anacondaInstallation, "/Scripts/activate.bat") |>
    normalizePath(mustWork = FALSE)
  
  
  # Confirm that 'batPath' exists
  if (!file.exists(batPath)) {
    
    paste0("Anaconda Prompt Batch File Not Found\n\n",
           "This procedure executes Python scripts using Anaconda Prompt. ",
           "However, the required batch file was not found in \"", 
           anacondaInstallation, "\". Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # If no issues were encountered, return 'batPath'
  return(batPath)
  
}



installAnacondaEnv <- function (batPath, envPath) {
  
  # Given the path to an Anaconda installation's "activate.bat" file,
  # install a new environment using the file referenced in 'envPath'
  
  
  # Double-check that 'envPath' exists
  if (!file.exists(envPath)) {
    
    paste0("Environment File Not Found\n\n",
           "The input variable 'envPath' (", envPath, ") is invalid. It ",
           "does not point to a real file. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Create a new Anaconda environment using the requirements in 'envPath'
  envRes <- system(paste0(shQuote(batPath), " && ",
                          "conda env create -f ", shQuote(envPath)), 
                   intern = TRUE)
  
  
  # The output of the environment creation command is stored in 'envRes'
  # Check for process errors using this variable
  
  
  # If the final "To activate this environment, use..." message does not
  # appear in 'envRes', that means that the environment was NOT created
  # successfully
  if (!any(grepl("To activate this environment, use", envRes))) {
    
    cat("\n\n")
    print(envRes)
    cat("\n\n")
    
    
    # If an authentication error occurs and a token is required, 
    # perform that update automatically here
    if (any(grepl("AnacondaAuthError", envRes)) &&
        any(grepl("anaconda token install", envRes))) {
      
      cat("\n\n")
      paste0("It appears that a token is required! Attempting install...") |>
        errWrap() |>
        str_replace("Attempting install...", col_green("Attempting install...")) |>
        cat()
      cat("\n\n")
      
      
      # Create a temporary batch file to initiate this process
      tokenBat <- "temp-token.bat"
      
      
      paste0("echo y | (", shQuote(batPath),  " && anaconda token install)") |>
        writeOutput(tokenBat, writeFunction = "write_lines", quietly = TRUE)
      
      # "echo y" provides the "yes" input needed to confirm installation
      # of the token
      
      
      # Try to install the token
      tokenRes <- system(tokenBat, intern = TRUE)
      
      
      # Output the results of the attempt
      cat("\n\n")
      print(tokenRes)
      cat("\n\n")
      
      
      # Delete the batch file next
      unlink(tokenBat)
      
      
      # If the process is successful, retry this function
      if (any(grepl("Success!", tokenRes))) {
        
        paste0("The token was installed successfully! Retrying the ",
               "environment creation process...") |>
          errWrap() |>
          cat()
        cat("\n\n")
        
        
        # Recursively call this function
        return(installAnacondaEnv(batPath, envPath))
        
        # If the token installation process failed, simply return an error 
      } else {
        
        paste0("Could Not Install Token\n\n",
               "The procedure failed for an unknown reason. Please ",
               "investigate the messages from Anaconda shown above.") |>
          errWrap() |>
          stop()
        
      }
      
      
      # In all other cases, output a generic error message
    } else {
      
      paste0("Could Not Create Environment\n\n",
             "The procedure failed for an unknown reason. Please ",
             "investigate the messages from Anaconda shown above.") |>
        errWrap() |>
        stop()
      
    }
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}

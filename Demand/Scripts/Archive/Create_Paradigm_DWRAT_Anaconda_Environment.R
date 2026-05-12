# Create an Anaconda environment titled "paradigm-dwrat"
# This environment will be used by the R scripts to run DWRAT 

# This script assumes that "paradigm-dwrat" does not currently exist
# If it does, you must remove it manually
# Open Anaconda Powershell Prompt and execute the command "conda remove -n paradigm-dwrat --all"



remove(list = ls())



require(tidyverse)



# Get the path to the batch file that activates Anaconda Prompt
batchPath <- c(list.files("C:/", pattern = "[aA]naconda", full.names = TRUE),
               list.files("C:/ProgramData/", pattern = "[aA]naconda", full.names = TRUE)) %>%
  sort() %>%
  str_replace_all("/", "\\\\") %>%
  paste0(., "\\Scripts\\activate.bat") %>%
  head(1)



# Ensure that the Anaconda installation exists and the batch file can be located
if (!file.exists(batchPath)) {
  
  stop(paste0("Could not find the Anaconda batch file that enables command usage",
              " in Command Prompt. The 'Anaconda' installation should be in",
              " the 'ProgramData' folder or the main C: Drive directory.") %>%
         strwrap(width = 0.999 * getOption("width")) %>%
         paste0(collapse = "\n"))
  
}



# First, ensure that the "paradigm-dwrat" environment doesn't already exist
# In case it does, try to remove it
# (This command returns nothing if the environment doesn't exist)
paste0(batchPath, " && ",
       "conda remove -n paradigm-dwrat --all") %>%
  system(intern = TRUE) %>%
  print()



# With the next command, use the "environment.yml" file to create a new Anaconda environment
# (This script assumes that the "Demand" R project is currently active)
# (The working directory would then be set to the "Demand" sub-folder)
paste0(batchPath, " && ",
       paste0("conda env create -f ", getwd() %>% str_remove("/Demand") %>% str_replace_all("/", "\\\\"), "\\Paradigm_DWRAT\\environment.yml")) %>%
  system(intern = TRUE) %>%
  print()



# Output a completion message
cat("\n\nDone! Please check the above output from Anaconda for errors.\n")

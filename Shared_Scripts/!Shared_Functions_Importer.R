# This script calls all of the other R scripts in the "Shared_Scripts" folder

# It references all required dependencies as well

# Workflow scripts that require shared functions should call this script


#### Dependencies ####

require(data.table)
require(tidyverse)
require(readxl)
require(cli)
require(httr)
require(rvest)
require(writexl)


#### Script Calls ####

source("Shared_Scripts/Control_Bridge.R")
source("Shared_Scripts/File_Handling.R")
source("Shared_Scripts/Git.R")
source("Shared_Scripts/Python_Bridge.R")
source("Shared_Scripts/R_Bridge.R")
source("Shared_Scripts/Statistics.R")
source("Shared_Scripts/Streamliner.R")
source("Shared_Scripts/Validation.R")

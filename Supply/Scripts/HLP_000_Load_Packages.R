# This script contains all packages used by the RR Supply procedure
# It simplifies of loading all required functions into the environment

# Since the order in which these packages are loaded affects which functions 
# are masked by another, all packages are loaded for every script,  
# even if some packages are used by only some of the scripts


require(data.table)
require(tidyverse)
require(readxl)
require(cli)
require(httr)
require(rvest)
require(fs)
require(SPEI)

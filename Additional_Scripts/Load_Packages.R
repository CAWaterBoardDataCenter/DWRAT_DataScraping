# This script loads in all packages used by every script in the repository
# (constructed by checking the output of the `dependencies` function from `renv`)

# The order in which scripts are loaded matters
# (some packages' functions mask other packages' functions)

# All scripts can rely on this script to call packages in order to remove
# the risk of inconsistent script behavior due to function masking


# In addition, this script will load in the custom "SDA" package
# (stored under the "Package" folder)


#### Environment Management ####
require(renv)

#### Data Wrangling ####
require(data.table)
require(tidyverse)
require(janitor)

#### XLSX Operations ####
require(openxlsx)
require(readxl)
require(writexl)

#### Web Requests ####
require(httr)
require(rvest)
require(RSelenium)
require(wdman)
require(netstat)
require(binman)
require(jsonlite)

#### GIS ####
require(sf)
require(stars)
require(lwgeom)
require(mapview)
require(webshot)
require(polylabelr)
require(colorspace)
require(leaflet)
require(leafem)
require(arcgis)
require(units)

#### Databases ####
require(odbc)
require(DBI)

#### Miscellaneous ####
require(cli)            # Command line colors
require(fs)             # File and directory management
require(SPEI)           # Standard Precipitation Index (SPI)

#### Custom Package ####
require(devtools)
require(roxygen2)

# Use another script to load the "SDA" package
source("Additional_Scripts/Custom_Package_Setup.R")

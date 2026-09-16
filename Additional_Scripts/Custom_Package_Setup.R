# This script helps install the custom "SDA" package in this repository

# There are two main install options with this script

# Either setup the documentation and install the entire package,
# or run a quicker load of the package using `load_all`


# Option 1 (the longer setup) is used if any of the following apply:
#   (*) The package has not been installed before
#   (*) This is the first run of the day 
#   (*) This is the first run in this R Session


# In all other cases, the faster setup is used


# To check if this is the first run or not, use a custom option parameter
# called "sda_installed_custom_package"

# At the start of a new R session, it will be NULL
# Otherwise, it will contain the last date when Option 1 was used


# Check if "Option 1" applies
if (!("SDA" %in% installed.packages()) || 
    is.null(getOption("sda_installed_custom_package")) || 
    getOption("sda_installed_custom_package") < Sys.Date()) {
  
  # Document the functions in the "Package" folder
  # Then, install them using `install`
  devtools::document("Package/", quiet = TRUE)
  devtools::install("Package/", quiet = TRUE)
  
  # After that, set the custom option to today's date
  options(sda_installed_custom_package = Sys.Date())
  
} else {
  
  # In all other cases, use `load_all` to read in the package
  devtools::load_all("Package/", export_all = FALSE, quiet = TRUE)
  
}

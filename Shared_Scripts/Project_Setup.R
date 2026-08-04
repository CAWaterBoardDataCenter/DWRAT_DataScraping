# This script is different from the other shared scripts
# (It does not contain functions)
# Instead, it has startup procedures for the repository

# This script sets up the "renv" package and installs all required packages


# Install 'renv' if it's not already present
if (!("renv" %in% installed.packages()[, 1])) {
  install.packages("renv", repos = "http://cran.us.r-project.org")
}


# Use 'renv' to setup the required environment
require(renv)


# Configure 'renv' to use "libcurl" instead of "curl" as its default download method
# https://rstudio.github.io/renv/articles/package-install.html#alternative-downloaders
Sys.setenv(RENV_DOWNLOAD_METHOD = "libcurl")

# "libcurl" is the default for `download.file` 
# (see the details about the "method" argument of this function)

# Also, "libcurl" is bundled with R on Windows (4.2.0 onwards), so it is a safe option


# Setup the R environment
restore(prompt = FALSE)

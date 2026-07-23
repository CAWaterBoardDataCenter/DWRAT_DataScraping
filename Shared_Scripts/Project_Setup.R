# This script is different from the other shared scripts
# (It does not contain functions)
# Instead, it has startup procedures for the repository

# This script sets up the "renv" package and installs all required packages


# Install 'renv' if it's not already present
if (!("renv" %in% installed.packages()[, 1])) {
  install.packages("renv")
}


# Use 'renv' to setup the required environment
require(renv)


# Configure 'renv' to use the default download method
# https://rstudio.github.io/renv/articles/package-install.html#alternative-downloaders
#options(renv.download.override = utils::download.file)
Sys.setenv(RENV_DOWNLOAD_METHOD = getOption("download.file.method"))


# Setup the R environment
restore(prompt = FALSE)

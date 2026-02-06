# Set the bounds of the forecast range here

# The "start date" of the forecast range is automatically set to 
# the *day after* the end date of the web scraping range
# (This value is set in "CTR_001"Set_Start_and_End_Dates.R")


# The end date of the forecast


# As a shortcut, the forecast range can be set as the current water year 
# Alternatively, a start and end date can be specified


# Set the forecast dates to the current water year
forecastCurrentWY <- TRUE


# If 'forecastCurrentWY' is TRUE, the next two variables will be ignored

# Both 'forecastStart' and 'forecastEnd' should be input as strings 
# with the dates in a "YYYY-MM-DD" format

forecastStart <- Sys.Date()

forecastEnd <- "2026-09-30"

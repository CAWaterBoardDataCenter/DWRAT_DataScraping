# DATA ACQUISITION SCRIPT

# Specify the range of years to be included in the demand dataset



# Change the numbers here!!
yearRange <- c(Start = 2017, # The start year
               End = 2024)   # The end year



# The scripts will only use RMS reports that contain data for these years 
# (inclusive of the start year and end year)



# No other changes to the script are needed!!



# Error Checks
stopifnot(is.numeric(yearRange[1]))
stopifnot(is.numeric(yearRange[2]))
stopifnot(yearRange[2] >= yearRange[1])



cat(paste0("The demand dataset will use reports submitted for ", 
           yearRange[1], " through ", yearRange[2], "\n"))
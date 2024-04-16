# Download data from the US Army Corps of Engineers website
# This script specifically gathers data for the Warm Springs Dam


require(tidyverse)
require(writexl)


# Specify the bounds for data collection (month/day/year)
# (Only the month and year matter)
dateRange <- c("10/01/2023",
               "04/30/2024") %>%
  as.Date(format = "%m/%d/%Y")



# Get the month-year pairs between the bounds of 
monthVals <- seq(from = dateRange[1], to = dateRange[2], by = "month")



# Iterate through 'monthVals'
for (i in 1:length(monthVals)) {
  
  # Get the month abbreviation that corresponds to this iteration's month
  monthStr <- month.abb[month(monthVals[i])] %>% tolower()
  
  
  
  # Construct the URL
  dataURL <- paste0("https://www.spk-wc.usace.army.mil/fcgi-bin/monthly.py?month=",
  monthStr, "&year=", year(monthVals[i]), 
  "&project=wrs&textonly=true")
  
  
  
  # Request data from this URL
  dataTable <- read_lines(dataURL)
  
  
  
  # Wait at least a second before proceeding
  Sys.sleep(runif(1, min = 1.1, max = 1.3))
  
  
  
  # Remove the text before the column headers
  # (The first header row starts with "Midnight")
  dataTable <- dataTable[grep("\\s+Midnight\\s+", dataTable)[1]:length(dataTable)]
  
  
  
  # Also remove the text after the table
  # (That section starts with rows containing "Totals")
  dataTable <- dataTable[1:(grep("\\s+Totals \\(", dataTable)[1] - 1)]
  
  
  
  # Put the header rows together
  # (The last header row contains the units of measurement)
  headerRow <- dataTable[1:grep("\\s+\\(ft\\)\\s+", dataTable)[1]]
  
  
  # Remove those header rows from 'dataTable'
  dataTable <- dataTable[-c(1:grep("\\s+\\(ft\\)\\s+", dataTable)[1])]
  
  
  
  # Also, remove rows from 'dataTable' that are just spaces (or empty)
  dataTable <- dataTable %>%
    str_subset("^\\s*$", negate = TRUE)
  
  
  # Convert 'dataTable' into a matrix
  dataTable <- dataTable %>%
    str_split("\\s") %>%
    unlist() %>% str_subset("^\\s*$", negate = TRUE) %>%
    as.numeric() %>%
    matrix(nrow = length(dataTable), byrow = TRUE)
  
  
  
  # Set up the proper header labels starting with the second row in 'headerRow'
  headers <- headerRow[2] %>%
    str_split("\\s") %>% unlist() %>%
    str_subset("^$", negate = TRUE)
  
  
  
  # Make manual fixes to the headers
  headers[which(headers == "Storage")[1]] <- "Midnight Storage"
  headers[which(headers == "Change")[1]] <- "Storage Change"
  headers[which(headers == "Inflow")[1]] <- "Mean Inflow"
  headers[which(headers == "Outflow")[1]] <- "Mean Outflow"
  headers[which(headers == "Evap")[1]] <- "Gross Evap"
  headers[which(headers == "Evap")[1]] <- "Pan Evap"
  headers[which(headers == "(in)")[1]] <- "Prec (in)"
  headers[which(headers == "Storage")[1]] <- "Top Con Storage"
  
  
  
  # The third row of 'headerRow' contains unit information
  units <- headerRow[3] %>%
    str_split("\\s") %>% unlist() %>%
    str_subset("^$", negate = TRUE)
  
  
  
  # Append the 'units' to 'headers'
  # Only the "Date" and precipitation columns do not need the units labels
  headers[!(headers %in% c("Date", "Prec (in)"))] <- paste0(headers[!(headers %in% c("Date", "Prec (in)"))], " ", units)
  
  
  
  # Append these headers to 'dataTable'
  # Also convert 'dataTable' into a tibble
  dataTable <- dataTable %>%
    data.frame() %>% tibble() %>%
    set_names(headers)
  
  
  
  # Add month and year fields to 'dataTable' as well
  # Also rename "Date" to "Day" (and a new "Date" column as well)
  dataTable <- dataTable %>%
    rename(Day = Date) %>%
    mutate(Month = which(tolower(month.abb) == monthStr),
           Year = year(monthVals[i]),
           Date = paste0(Year, "-", Month, "-", Day)) %>%
    relocate(Date, Year, Month)
  
  
  
  # Convert the "Mean Inflow (sfd)" values to cfs
  # "sfd" is "second-feet-day" or "cfs-day" (a measure of volume)
  # To convert from sfd (volume) to cfs (volume/time), the sfd values should be
  # divided by a unit of time (day)
  # However, since these are daily measurements, dividing by "1 day" will not change
  # the magnitude of the results
  # Therefore, "Mean Inflow (sfd)" will simply be renamed to "Mean Inflow (cfs)"
  # https://directives.sc.egov.usda.gov/31262.wba
  # https://www.scbid.org/water-measurement
  dataTable <- dataTable %>%
    rename(`Mean Inflow (cfs)` = `Mean Inflow (sfd)`)
  
  
  
  # Add a flag to 'dataTable' if the mean inflow is negative
  dataTable <- dataTable %>%
    filter(NEGATIVE_INFLOW = `Mean Inflow (cfs)` < 0)
  
  
  
  # Add 'dataTable' to a compiled tibble
  # (If this is the first iteration, initialize 'compiledDF' using 'dataTable')
  if (i == 1) {
    
    compiledDF <- dataTable
    
  } else {
    
    compiledDF <- bind_rows(compiledDF, dataTable)
    
  }
  
}



# Write 'compiledDF' to a file
compiledDF %>%
  write_xlsx(paste0("ProcessedData/Warm_Springs_Dam_", 
                    format(dateRange[1], "%Y-%m"), "_to_",
                    format(dateRange[2], "%Y-%m"), ".xlsx"))

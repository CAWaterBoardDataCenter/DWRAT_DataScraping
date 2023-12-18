# This script replaces both Downsizer and "NOAA_Scraper.R"
# It collects climate data from the NOAA NCEI API 
# Specifically, it gets daily values for Precipitation and Temperature (Min/Max) in mm and degrees Celsius, respectively


library(tidyverse)
library(readxl)


cat("Starting 'NOAA_API_Scraper.R'...\n")


# Start by preparing the request URL


# A list of stations is needed for that
# Use "RR_PRMS_StationList (2023-09-05).xlsx"
stationList <- read_xlsx("InputData/RR_PRMS_StationList (2023-09-05).xlsx", sheet = "StationList") %>%
  filter(`Observed and PRISM Station Guide` == "DOWNSIZER") %>%
  select(...5) %>% unlist() %>% as.vector() %>%
  paste0(collapse = ",")



# Paste 'stationList' into the request URL and include other setting customizations as well
requestURL <- paste0("https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries",
                     "&stations=", stationList,
                     "&startDate=", StartDate$date, "T00:00:00",
                     "&endDate=", EndDate$date, "T23:59:59", 
                     "&dataTypes=PRCP,TMAX,TMIN", "&format=csv",
                     "&options=includeAttributes:false,includeStationName:true,includeStationLocation:false",
                     "&units=metric")



# Download the file to the "WebData" folder
download.file(requestURL, paste0("WebData/NOAA_API_", EndDate$date, ".csv"), mode = "wb", quiet = TRUE)




# Use a function from "NOAA_Scraper.R" to adjust the output file to be compatible with "Downsizer_Processor.R"
fileAdjustment <- function (csvPath, EndDate) {
  
  # Adjust the CSV located at 'csvPath' to have a format comparable to Downsizer output files
  
  
  # In the current format, each station has its own row for each date
  # If a station does not have data for that date, it has no row in the dataset
  
  
  # The Downsizer output is typically one row per date, with all stations' data present
  
  
  
  # Read in the CSV file identified with 'csvPath'
  initialDF <- read_csv(csvPath, show_col_types = FALSE)
  
  
  
  # Read in the CSV file "Downsizer_Stations.csv"
  # It contains a single row will all column names for the Downsizer output file
  columnHeaders <- read_csv("InputData/Downsizer_Stations.csv", col_names = FALSE, 
                            col_types = cols(.default = col_character())) %>%
    unlist() %>% as.vector()
  
  
  
  # The headers have numbered IDs for the stations
  # These values can be found in "RR_PRMS_StationList (2023-09-05).xlsx"
  stationDF <- read_xlsx("InputData/RR_PRMS_StationList (2023-09-05).xlsx")
  
  
  # The first row of 'stationDF' actually contains the headers
  stationDF <- stationDF[-1, ] %>% 
    set_names(stationDF[1, ] %>% unlist() %>% as.vector()) %>%
    filter(Source == "DOWNSIZER")
  
  
  
  # Define a vector to hold the reformatted CSV
  # These first few lines will be removed/replaced by 'Downsizer_Processor.R'
  newDF <- c("Written by class gov.usgs.trinli.ft.point.writer.PrmsWriter",
             "////////////////////////////////////////////////////////////",
             "// Station metadata (listed in the same order as the data):",
             "// ID\t\t\t\tType\tLatitude\tLongitude\tElevation",
             "// 049684\t\t\tprecip\t39.4194\t\t-123.3425\t\t1353",
             "// 047109\t\t\tprecip\t39.3619\t\t-123.1286\t\t1018",
             "// 049122\t\t\tprecip\t39.1466\t\t-123.2102\t\t636",
             "// 049126\t\t\tprecip\t39.1266\t\t-123.2719\t\t1328",
             "// 041838\t\t\tprecip\t38.793\t\t-123.0263\t\t400",
             "// 043875\t\t\tprecip\t38.6294\t\t-122.8665\t\t177",
             "// 041312\t\t\tprecip\t38.5768\t\t-122.5781\t\t350",
             "// 043191\t\t\tprecip\t38.515\t\t-123.2447\t\t112",
             "// 043578\t\t\tprecip\t38.4305\t\t-122.8647\t\t200",
             "// 046370\t\t\tprecip\t38.3858\t\t-122.9661\t\t865",
             "// 049684\t\t\ttmax\t39.4194\t\t-123.3425\t\t1353",
             "// 047109\t\t\ttmax\t39.3619\t\t-123.1286\t\t1018",
             "// 049122\t\t\ttmax\t39.1466\t\t-123.2102\t\t636",
             "// 049126\t\t\ttmax\t39.1266\t\t-123.2719\t\t1328",
             "// 041838\t\t\ttmax\t38.793\t\t-123.0263\t\t400",
             "// 043875\t\t\ttmax\t38.6294\t\t-122.8665\t\t177",
             "// 041312\t\t\ttmax\t38.5768\t\t-122.5781\t\t350",
             "// 043191\t\t\ttmax\t38.515\t\t-123.2447\t\t112",
             "// 043578\t\t\ttmax\t38.4305\t\t-122.8647\t\t200",
             "// 046370\t\t\ttmax\t38.3858\t\t-122.9661\t\t865",
             "// 049684\t\t\ttmin\t39.4194\t\t-123.3425\t\t1353",
             "// 047109\t\t\ttmin\t39.3619\t\t-123.1286\t\t1018",
             "// 049122\t\t\ttmin\t39.1466\t\t-123.2102\t\t636",
             '// 049126\t\t\ttmin\t39.1266\t\t-123.2719\t\t1328',
             '// 041838\t\t\ttmin\t38.793\t\t-123.0263\t\t400',
             '// 043875\t\t\ttmin\t38.6294\t\t-122.8665\t\t177',
             '// 041312\t\t\ttmin\t38.5768\t\t-122.5781\t\t350',
             '// 043191\t\t\ttmin\t38.515\t\t-123.2447\t\t112',
             "// 043578\t\t\ttmin\t38.4305\t\t-122.8647\t\t200",
             "// 046370\t\t\ttmin\t38.3858\t\t-122.9661\t\t865",
             '////////////////////////////////////////////////////////////',
             '// Unit: precip = mm, temperature = deg C, elevation = feet',
             '////////////////////////////////////////////////////////////',
             'precip 10',
             'tmax 10',
             "tmin 10",
             "########################################")
  
  
  
  # Get a vector of dates that appear in 'initialDF'
  # 'newDF' will have one row per date
  dateVec <- initialDF$DATE %>% unique() %>% sort()
  
  
  
  # Iterate through the entries in 'dateVec'
  for (i in 1:length(dateVec)) {
    
    # Collect values for each of the columns listed in 'columnHeaders'
    # Combine those values into one space-separated row
    # Then, add that row to 'newDF'
    
    
    
    # Initialize the vector that will hold values for all columns
    rowVec <- c()
    
    
    
    # Iterate through 'columnHeaders' next
    for (j in 1:length(columnHeaders)) {
      
      # Perform different operations depending on the value of 'columnHeaders'
      
      
      # First, if 'columnHeaders' is NA, add -999 to 'rowVec'
      if (is.na(columnHeaders[j])) {
        
        rowVec <- c(rowVec, -999.0)
        
        # The next checks are for the year, month, and day columns
      } else if (columnHeaders[j] == "Year") {
        
        rowVec <- c(rowVec, year(dateVec[i]))
        
      } else if (columnHeaders[j] == "Month") {
        
        rowVec <- c(rowVec, month(dateVec[i]))
        
      } else if (columnHeaders[j] == "Day") {
        
        rowVec <- c(rowVec, day(dateVec[i]), 0, 0, 0)
        
        # 0s are added after "day" to account for Hours, Minutes, and Seconds columns
        
        
        # The next type of column names is formatted like "DOWNSIZER_[COLTYPE][ID]" 
        # COLTYPE can be "PRECIP", "TMAX", or "TMIN"
        # ID is an integer
      } else if (grepl("^DOWNSIZER", columnHeaders[j])) {
        
        
        # First get the name of the column in 'initialDF' that corresponds to
        # the desired variable type (PRECIPITATION, MIN TEMPERATURE, or MAX TEMPERATURE)
        if (grepl("PRECIP", columnHeaders[j])) {
          
          colChoice <- "PRCP"
          
        } else if (grepl("TMAX", columnHeaders[j])) {
          
          colChoice <- "TMAX"
          
        } else if (grepl("TMIN", columnHeaders[j])) {
          
          colChoice <- "TMIN"
          
        } else {
          
          stop(paste0("Unknown variable name ", columnHeaders[j]))
          
        }
        
        
        
        # Next, get the station that corresponds to the column header name/ID
        # Then, for this iteration's date, get the corresponding value and save it to 'rowVec'
        
        
        # The first filter narrows 'initialDF' down to entries with the same station ID ("USC000#####")
        # The names in 'columnHeaders' appear in 'stationDF' with their corresponding stations, so
        # this iteration's column name is referenced in 'stationDF', and the station ID is extracted
        # and compared to the ID column in 'initialDF' ("STATION")
        # The second filter simply reduces the tibble to entries for this iteration's date
        # Finally, the value of one data column (PRECIP, MIN TEMP, or MAX TEMP) is extracted
        extractedVal <- initialDF[initialDF$STATION == stationDF$`Full Station ID`[stationDF$`DAT_File Field Name` == columnHeaders[j]] &
                                    initialDF$DATE == dateVec[i], colChoice] %>%
          unlist() %>% as.vector()
        
        
        # If no value was found for this iteration, 'extractedVal' should be -999
        if (length(extractedVal) == 0) {
          extractedVal <- -999.0
        }
        
        
        # Save 'extractedVal' to 'rowVec'
        rowVec <- c(rowVec, extractedVal)
        
        
      } else {
        
        stop(paste0("No procedure was written for a column called ", columnHeaders[j]))
        
      }
      
    } # End of loop through 'columnHeaders'
    
    
    # Once 'rowVec' has been constructed, merge it into a single string
    # (separated by one space " ")
    # Then, save it to 'newDF'
    newDF <- c(newDF,
               paste0(rowVec, collapse = " "))
    
    
  } # End of loop through 'dateVec'
  
  
  
  # Write 'newVec' to a CSV file
  # Use 'Downsizer' and 'EndDate' in the output name
  writeLines(newDF, paste0("WebData/Downsizer_", EndDate$date, ".csv"))
  
  
  
  # Remove the original CSV file afterwards
  file.remove(csvPath)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



prismFill <- function (EndDate) {
  
  # If the downloaded NOAA file does not have date up to 'EndDate', later scripts will fail
  # Substitute missing entries with data from PRISM
  
  
  
  # First, check if the date in 'EndDate' appears in the downloaded data file
  inputVec <- paste0("WebData/Downsizer_", EndDate$date, ".csv") %>%
    readLines()
  
  
  
  # End the function if the end date does appear in the dataset
  if (grepl(paste(EndDate$year, EndDate$month, EndDate$day, sep = " "), 
            tail(inputVec, 1))) {
    return(invisible(NULL))
  }

  
  
  # Otherwise, read in "Prism_Processed.csv"
  prismDF <- read_csv("ProcessedData/Prism_Processed.csv", show_col_types = FALSE)
  
  
  
  # Also, get the column names for Downsizer and linking table to PRISM
  columnHeaders <- read_csv("InputData/Downsizer_Stations.csv", col_names = FALSE, 
                            col_types = cols(.default = col_character())) %>%
    unlist() %>% as.vector()
  
  
  
  # The headers have numbered IDs for the stations
  # These values can be found in "RR_PRMS_StationList (2023-09-05).xlsx"
  stationDF <- read_xlsx("InputData/RR_PRMS_StationList (2023-09-05).xlsx")
  
  
  
  # The first row of 'stationDF' actually contains the headers
  stationDF <- stationDF[-1, ] %>% 
    set_names(stationDF[1, ] %>% unlist() %>% as.vector()) %>%
    filter(Source == "DOWNSIZER")
  
  
  
  # After that, get the missing dates
  missingDates <- seq(from = 1 + tail(inputVec, 1) %>% 
                        str_extract("^[0-9]+ [0-9]+ [0-9]+") %>%
                        str_replace_all("\\s+", "-") %>%
                        as.Date(), 
                      to = EndDate$date - 1, by = "days")
  
  
  
  # Repeat the next procedure for each missing entry
  # (Create a row that will be appended to 'inputVec')
  for (i in 1:length(missingDates)) {
    
    # Initialize the vector that will hold values for all columns
    rowVec <- c()
    
    
    
    # Iterate through 'columnHeaders' next
    for (j in 1:length(columnHeaders)) {
      
      # Perform different operations depending on the value of 'columnHeaders'
      
      
      # First, if 'columnHeaders' is NA, add -999 to 'rowVec'
      if (is.na(columnHeaders[j])) {
        
        rowVec <- c(rowVec, -999.0)
        
        # The next checks are for the year, month, and day columns
      } else if (columnHeaders[j] == "Year") {
        
        rowVec <- c(rowVec, year(missingDates[i]))
        
      } else if (columnHeaders[j] == "Month") {
        
        rowVec <- c(rowVec, month(missingDates[i]))
        
      } else if (columnHeaders[j] == "Day") {
        
        rowVec <- c(rowVec, day(missingDates[i]), 0, 0, 0)
        
        # 0s are added after "day" to account for Hours, Minutes, and Seconds columns
        
        
        # The next type of column names is formatted like "DOWNSIZER_[COLTYPE][ID]" 
        # COLTYPE can be "PRECIP", "TMAX", or "TMIN"
        # ID is an integer
      } else if (grepl("^DOWNSIZER", columnHeaders[j])) {
        
        
        # First get the name of the column in 'initialDF' that corresponds to
        # the desired variable type (PRECIPITATION, MIN TEMPERATURE, or MAX TEMPERATURE)
        if (grepl("PRECIP", columnHeaders[j])) {
          
          colChoice <- "PRCP"
          
        } else if (grepl("TMAX", columnHeaders[j])) {
          
          colChoice <- "TMAX"
          
        } else if (grepl("TMIN", columnHeaders[j])) {
          
          colChoice <- "TMIN"
          
        } else {
          
          stop(paste0("Unknown variable name ", columnHeaders[j]))
          
        }
        
        
        
        # Next, get the "Rank" value that corresponds to the column header name/ID
        rankVal <- stationDF$Rank[stationDF$`DAT_File Field Name` == columnHeaders[j]]
        
        
        
        # Find the corresponding entry in 'prismDF'
        if (grepl("PRECIP", rankVal)) {
        
          colStr <- "PP_"
          
        } else {
          
          colStr <- "PT_"
          
        }
        
        
        extractedVal <- prismDF[[paste0(colStr, rankVal)]][prismDF$Date == missingDates[i]]
        
        
        
        # If no value was found for this iteration, 'extractedVal' should be -999
        if (length(extractedVal) == 0) {
          extractedVal <- -999.0
        }
        
        
        
        # Save 'extractedVal' to 'rowVec'
        rowVec <- c(rowVec, extractedVal)
        
        
      } else {
        
        stop(paste0("No procedure was written for a column called ", columnHeaders[j]))
        
      }
      
    } # End of loop through 'columnHeaders'
    
    
    
    # Once 'rowVec' has been constructed, merge it into a single string
    # (separated by one space " ")
    # Then, save it to 'inputVec'
    inputVec <- c(inputVec,
                  paste0(rowVec, collapse = " "))
    
    
  } # End of 'i' loop ('missingDates')
  
  
  
  # Save the updated 'inputVec'
  inputVec %>%
    writeLines(paste0("WebData/Downsizer_", EndDate$date, ".csv"))
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



fileAdjustment(paste0("WebData/NOAA_API_", EndDate$date, ".csv"), EndDate)

prismFill(EndDate)



# Now that the procedure is complete, remove the variables
remove(stationList, requestURL, fileAdjustment, prismFill)


cat("'NOAA_API_Scraper.R' is done!\n")
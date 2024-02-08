# This script completes the same objective as "Scripts/CNRFC_Scraper.R" 
# However, it does not rely on RSelenium


#### Dependencies ####


library(tidyverse)


#### Functions ####


mainProcedure <- function () {
  
  
  # Remove previously downloaded CNRFC files from the WebData folder 
  list.files("WebData/", full.names = TRUE) %>%
    str_subset("temperaturePlot|cnrfc") %>%
    map(file.remove)
  
  
  
  # Read in "CNRFC_Stations.csv"
  # It lists the stations whose data will be downloaded
  stationDF <- read_csv("InputData/CNRFC_Stations.csv", show_col_types = FALSE)
  
  
  
  # Begin with collecting temperature data for these stations
  # Use a loop to iterate through the stations
  for (i in 1:nrow(stationDF)) {
    
    # If this station has no ID, skip it
    if (is.na(stationDF$TempStation[i])) {
      next
    }
    
    
    # Using another function, attempt to create a temperature plot CSV file
    # Its format should match that of the downloadable version created by the website's vendor software
    writeTemperatureCSV(stationDF[i, ])
    
    
    
    # Wait a few seconds before proceeding to the next iteration
    Sys.sleep(runif(1, min = 1, max = 3))
    
  }
  
  
  
  # The next step is to work on getting precipitation data
  # Download the 6-Day Basin QPF CSV file to the WebData folder
  download.file("https://www.cnrfc.noaa.gov/data/cnrfc_qpf.csv",
                "WebData/cnrfc_qpf.csv", mode = "wb", quiet = TRUE)
  
  
  
  # After that, include a completion message
  cat("Done!\n")
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



writeTemperatureCSV <- function (stationData) {
  
  # Given the ID of a temperature station,
  # Extract data for that station from the CNRFC NOAA website and save it to a CSV file
  
  
  
  # The URL for the station requires the ID listed in the "TempStation column
  tempURL <- paste0("https://www.cnrfc.noaa.gov/temperaturePlots_hc.php?id=",
                    stationData$TempStation[1])
  
  
  
  # Download the raw HTML of that page
  pageHTML <- readLines(tempURL, warn = FALSE)
  
  
  
  # The data is saved in custom "chart.addSeries" functions on the HTML page
  # Each call to that function contains one column of data (plus its date-time of occurrence)
  # Subset 'pageHTML' to just those rows
  dataArray <- pageHTML %>%
    str_subset("chart\\.addSeries")
  
  
  
  # Error Check
  # There should be 8 columns of data
  stopifnot(length(dataArray) == 8)
  
  
  
  # Use a nested loop and iterate through these elements of 'dataArray'
  # A single dataframe, containing all columns of data will be created in this loop
  for (j in 1:length(dataArray)) {
    
    # Extract the name of this column
    colName <- dataArray[j] %>%
      str_extract("name:'[^']+'") %>%
      str_remove("^name:") %>% str_remove_all("'")
    
    
    
    # If the column has no name, rename it to "Empty"
    # (It still has values and will become a column in the final output)
    if (is.na(colName)) {
      colName <- "Empty"
    }
    
    
    
    # Extract the data pairs next
    # Pull out the "data" attribute from the .addSeries() text
    # Next, remove the extra characters around the data elements 
    # Then, split the data pairs
    # (each pair is contained within a set of brackets; pairs are separated by commas)
    # After removing the brackets, split the pairs and reframe the data as a data frame
    # The dataset will have two columns: 
    #    (1) Observed / Forecast Hydrologic Day
    #    (2) The name specified in 'colName'
    parsedDF <- dataArray[j] %>%
      str_extract("data:\\[\\[.+\\],\\],") %>% 
      str_remove("data:\\[") %>% str_remove(",\\],") %>%
      str_split(",\\[") %>% unlist() %>%
      str_remove_all("[\\[\\]]") %>%
      str_split(",") %>% unlist() %>%
      matrix(ncol = 2, byrow = TRUE) %>%
      data.frame() %>% tibble() %>%
      set_names(c("Observed / Forecast Hydrologic Day", colName))
    
    
    
    # Convert values of "null" to NA in the second column
    parsedDF[[colName]] <- parsedDF[[colName]] %>%
      str_replace("^null$", NA_character_)
    
    
    
    # Before continuing, fix the types of the columns
    # The first column is a datetime, and the second column is numeric
    # To start, both columns should be converted to numeric
    parsedDF <- parsedDF %>%
      mutate_all(as.numeric)
    
    
    
    # The column "Observed / Forecast Hydrologic Day" is in an epoch format
    # (milliseconds since 1970-01-01 00:00:00 UTC)
    # Divide that column's values by 1000 (seconds should be the smallest unit of time),
    # and then convert the column to a datetime (with the timezone set to the Russian River's timezone)
    parsedDF <- parsedDF %>%
      mutate(`Observed / Forecast Hydrologic Day` = as_datetime(`Observed / Forecast Hydrologic Day` / 1000, 
                                                                tz = "America/Los_Angeles"))
    
    
    
    # If there are multiple entries for the same datetime, keep only the last entry
    # (This is how their system appears to handle those cases)
    if (parsedDF$`Observed / Forecast Hydrologic Day` %>% unique() %>% length() < nrow(parsedDF)) {
      
      # Get a list of date-time values that have more than one occurrence in 'parsedDF'
      dupDates <- names(table(parsedDF$`Observed / Forecast Hydrologic Day`))[table(parsedDF$`Observed / Forecast Hydrologic Day`) > 1]
      
      
      
      # Iterate through those duplicate cases
      for (k in 1:length(dupDates)) {
        
        # Get the indices where this date-time is used
        dupIndices <- which(parsedDF$`Observed / Forecast Hydrologic Day` == dupDates[k])
        
        
        
        # Only the last value will be kept
        # The other rows will be erased
        # Remove the last index value from 'dupIndices'
        dupIndices <- dupIndices %>%
          head(-1)
        
        
        
        # Remove the rows specified by 'dupIndices' from 'parsedDF'
        parsedDF <- parsedDF[-dupIndices, ]
        
      } # End of k loop
      
    }
    
    
    
    # If this is the first iteration of the loop, initialize the data frame that
    # will contain all columns of data
    if (j == 1) {
      
      combinedDF <- parsedDF
      
      # Otherwise, make a full join and add 'parsedDF' to 'combinedDF'
    } else {
      combinedDF <- combinedDF %>%
        full_join(parsedDF, by = "Observed / Forecast Hydrologic Day", relationship = "one-to-one")
    }
    
  } # End of 'j' loop
  
  
  
  # If a column named "Empty" is in 'combinedDF', there originally was a column
  # with no name that had its named changed earlier
  # Changed it back to an empty name before writing the data frame to a file
  if ("Empty" %in% names(combinedDF)) {
    names(parsedDF)[which(names(parsedDF) == "Empty")] <- ""
  }
  
  
  
  # Save 'combinedDF' to a CSV file
  combinedDF %>%
    write_csv(paste0("WebData/", stationData$TempStation[1], "_temperaturePlot.csv"))
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

cat("Starting 'CNRFC_Static_Scraper.R'...\n")


#mainProcedure()


#remove(mainProcedure, writeTemperatureCSV)
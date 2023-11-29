# This script is intended to replace "CIMIS_Scraper.R" 


#### Dependencies ####


require(tidyverse)
require(httr)


#### Functions ####

mainProcedure <- function (StartDate, EndDate) {
  
  
  # Read in the list of stations
  stationDF <- read_csv("InputData/CIMIS_Stations.csv", show_col_types = FALSE)
  
  
  
  # Iterate through 'stationDF'
  for (i in 1:nrow(stationDF)) {
    
    
    # Construct the URL to retrieve data for this iteration's station
    cimisURL <- paste0("https://ipm.ucanr.edu/calludt.cgi/WXDATAREPORT?",
                       "STN=", stationDF$Alias[i],
                       "&MAP=",
                       # Start Date
                       "&FROMMONTH=", StartDate$month,
                       "&FROMDAY=", StartDate$day,
                       "&FROMYEAR=", StartDate$year,
                       # End Date
                       "&THRUMONTH=", EndDate$month,
                       "&THRUDAY=", EndDate$day,
                       "&THRUYEAR=", EndDate$year,
                       # Precipitation
                       "&DT_PRECIP=1",
                       # No backup stations
                       "&PRECIP_BACKUP1=.&PRECIP_BACKUP2=.&PRECIP_BACKUPAVG=.",
                       # Air Temperature
                       "&DT_AIR=1",
                       # No backup stations
                       "&AIR_BACKUP1=.&AIR_BACKUP2=.&AIR_BACKUPAVG=.",
                       # Metric units
                       "&UNITS=M",
                       # Retrieve the data in a CSV format
                       "&FFMT=T&ACTION=RETRIEVE+DATA")
    
    
    
    # Retrieve data from CIMIS
    htmlPage <- GET(cimisURL) %>%
      content() %>% as.character() %>%
      str_split("(\r)?\n") %>% unlist()
    
    
    # Take a subset from 'htmlPage' (just the data table)
    cimisDF <- htmlPage[grep('"Station","Date', htmlPage):(grep('</pre', htmlPage) - 1)]
    
    
    
    # Convert 'cimisDF' into a data frame
    cimisDF <- cimisDF %>%
      str_split(",") %>% unlist() %>%
      matrix(nrow = length(cimisDF), byrow = TRUE) %>%
      data.frame()
    
    
    
    # The first row is the header row
    cimisDF <- cimisDF[-1, ] %>%
      set_names(cimisDF[1, ] %>% 
                  unlist() %>% as.vector() %>% 
                  str_remove_all('^"') %>% str_remove_all('"$'))
    
    
    
    # Take a subset of 'cimisDF'
    # Get "Station", "Date", "Precip", max temp ("Air max"), and min temp ("min")
    # Note: There are multiple columns with the name "min"
    # This code takes the "min" column that appears immediately after "Air max"
    cimisDF <- cimisDF[, c(which(names(cimisDF) %in% c("Station", "Date", "Precip", "Air max")),
                           base::intersect(which(names(cimisDF) == "min"), grep("Air max", names(cimisDF)) + 1))]
    
    
    
    # Make sure that all five columns were taken
    stopifnot(ncol(cimisDF) == 5)
    
    
    
    # Adjust the column names
    cimisDF <- cimisDF %>%
      rename(Tmax = `Air max`, Tmin = min)
    
    
    
    # Apply some station-specific changes as well
    if (stationDF$Station[i] == "Sanel Valley 106") {
      
      
      # Exclude the precipitation data for this station
      # Then, rename the columns
      cimisDF <- cimisDF %>%
        select(Date, Station, Tmax, Tmin) %>%
        rename(`Sanel Valley` = Station,
               CIMIS_TMAX3 = Tmax,
               CIMIS_TMIN3 = Tmin)
      
      
    } else if (stationDF$Station[i] == "Santa Rosa 83") {
      
      
      # Exclude the precipitation data for this station
      # Then, rename the columns
      cimisDF <- cimisDF %>%
        select(Date, Station, Tmax, Tmin) %>%
        rename(`Santa Rosa` = Station,
               CIMIS_TMAX4 = Tmax,
               CIMIS_TMIN4 = Tmin)
      
      
    } else if (stationDF$Station[i] == "Windsor 103") {
      
      
      # Exclude the temperature data for this station
      # Then, rename the columns
      cimisDF <- cimisDF %>%
        select(Date, Station, Precip) %>%
        rename(Windsor = Station,
               CIMIS_PRECIP12 = Precip)
      
      
    } else {
      
      stop("No procedure was written for this station")
      
    }
    
    
    
    # If this is the first iteration, define a combined data frame
    # that will hold columns from each station
    # Otherwise, join 'cimisDF' to this combined DF
    if (i == 1) {
      
      combinedDF <- cimisDF
      
    } else {
      
      combinedDF <- combinedDF %>%
        inner_join(cimisDF, by = "Date")
      
    }
    
    
    
    # Wait a bit before proceeding to the next iteration
    Sys.sleep(runif(1, min = 1.5, max = 3))
    
  }
  
  
  
  # Add a fourth data frame to 'combinedDF'
  # This is for the "Hopland 85" station
  combinedDF <- c(seq(from = StartDate$date, to = EndDate$date, by = 'day') %>% 
                    str_remove_all("\\-"),
                  rep("Hopland_85", nrow(cimisDF)),
                  rep(-999, nrow(cimisDF))) %>%
    matrix(ncol = 3, byrow = FALSE) %>%
    data.frame() %>%
    set_names(c("Date", "Hopland", "CIMIS_PRECIP6")) %>%
    inner_join(combinedDF, by = "Date")
  
  
  
  # Select a subset of 'combinedDF' (basically removing the station name columns)
  combinedDF <- combinedDF %>%
    select(Date, CIMIS_PRECIP6, CIMIS_PRECIP12, CIMIS_TMAX3,
           CIMIS_TMAX4,CIMIS_TMIN3, CIMIS_TMIN4)
  
  
  
  #Replace all missing values with -999
  combinedDF[combinedDF == ""] = -999
  
  
  
  # The next steps rely on "PRISM_Processor.R"," CNRFC_Scraper.R", and "CNRFC_Processor.R"
  
  
  
  # Replace missing values with data from PRISM
  prismDF <- read_csv("ProcessedData/Prism_Processed.csv", show_col_types = FALSE) %>%
    select(Date, PP_PRECIP6, PP_PRECIP12, PT_TMAX3, PT_TMAX4, PT_TMIN3, PT_TMIN4)
  
  
  
  # Before substituting the missing values, ensure that 'combinedDF' and 'prismDF' are data frames
  combinedDF <- as.data.frame(combinedDF)
  prismDF <- as.data.frame(prismDF)
  
  
  
  # After that, assign values from 'prismDF' to 'combinedDF'
  combinedDF[combinedDF == -999] <- prismDF[combinedDF == -999]
  
  
  
  # Verify that the "Date" column is set to the required format for DAT_Shell
  combinedDF$Date <- combinedDF$Date %>%
    as.character() %>%
    as.Date(format = "%Y%m%d")
  
  
  
  # The next step is to append CNRFC data to 'combinedDF'
  
  
  
  # Read in "CNRFC_Processed.csv", take a subset of the columns,
  # and rename them to match the column names in 'combinedDF'
  cnrfcDF <- read_csv("ProcessedData/CNRFC_Processed.csv", show_col_types = FALSE) %>%
    select(Date, 
           PRECIP6_HOPC1, PRECIP12_MWEC1,
           TMAX3_CDLC1, TMIN3_CDLC1, TMAX4_LSEC1, TMIN4_LSEC1) %>%
    rename(CIMIS_PRECIP6 = PRECIP6_HOPC1, CIMIS_PRECIP12 = PRECIP12_MWEC1,
           CIMIS_TMAX3 = TMAX3_CDLC1, CIMIS_TMIN3 = TMIN3_CDLC1, 
           CIMIS_TMAX4 = TMAX4_LSEC1, CIMIS_TMIN4 = TMIN4_LSEC1)
  
  
  
  # Bind 'cnrfcDF' to 'combinedDF'
  combinedDF <- combinedDF %>%
    rbind(cnrfcDF)
  
  
  
  # Write 'combinedDF' to a file
  combinedDF %>%
    write_csv("ProcessedData/CIMIS_Processed.csv")
  
  
  
  # Output a completion message
  cat("Done!\n")
  
  
  # Return nothing
  return(invisible(NULL))
  
}


#### Script Execution ####


cat("Starting 'CIMIS_Static_Scraper.R'...")


mainProcedure(StartDate, EndDate)


remove(mainProcedure)
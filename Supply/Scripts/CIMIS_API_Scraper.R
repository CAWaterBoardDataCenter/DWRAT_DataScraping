# This script is intended to replace "CIMIS_Scraper.R" 


#### Dependencies ####


require(tidyverse)
require(httr)


#### Functions ####

mainProcedure <- function (StartDate, EndDate, includeForecast) {
  
  
  # Use the ipm.ucanr URL to get station data
  # (Daily minimum temperature, maximum temperature, and precipitation)
  combinedDF <- urlBasedCall(StartDate, EndDate)
  
  
  
  # Select a subset of 'combinedDF' (basically removing the station name columns)
  combinedDF <- combinedDF %>%
    select(Date, CIMIS_PRECIP6, CIMIS_PRECIP12, CIMIS_TMAX3,
           CIMIS_TMAX4,CIMIS_TMIN3, CIMIS_TMIN4)
  
  
  
  #Replace all missing values with -999
  combinedDF[combinedDF == ""] = -999
  
  
  
  # The next steps rely on the outputs of 
  # "PRISM_Processor.R"," CNRFC_Scraper.R", and "CNRFC_Processor.R"
  
  
  
  print(paste0("Replacing ", sum(combinedDF == -999), " elements (out of ",
               nrow(combinedDF) * ncol(combinedDF), " records) with PRISM data"))
  
  
  
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
  # (If 'includeForecast' is TRUE)
  if (includeForecast) {
    
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
    
  }
  
  
  
  # Write 'combinedDF' to a file
  combinedDF %>%
    write_csv("ProcessedData/CIMIS_Processed.csv")
  
  
  
  # Output a completion message
  cat("Done!\n")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



urlBasedCall <- function (StartDate, EndDate) {
  
  # This procedure for getting CIMIS data relies on a URL
  # The parameters in the URL are modified to get data for the desired stations
  
  
  
  # Read in the list of stations
  stationDF <- read_csv("InputData/CIMIS_Stations.csv", show_col_types = FALSE)
  
  
  
  # Iterate through 'stationDF'
  for (i in 1:nrow(stationDF)) {
    
    
    # Skip the Hopland station in this procedure
    if (stationDF$ID[i] == 85) {
      next
    }
    
    
    
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
  
  
  
  return(combinedDF)
  
}



apiBasedCall <- function (StartDate, EndDate) {
  
  # Make a call to CIMIS's dedicated API
  # Get precipitation and temperature data for each station
  
  
  
  # Read in a list of stations
  stationDF <- read_csv("InputData/CIMIS_Stations.csv", show_col_types = FALSE)

  
  
  # Create the request URL
  requestURL <- paste0("https://et.water.ca.gov/api/data?",
                       # State the API Key (CIMIS account required to get these)
                       # This key is tied to an account that uses Aakash's SWRCB email
                       "appKey=", "b0173b26-6de4-48e8-be98-cc21a18dc20a",
                       # Station IDs (comma-separated)
                       "&targets=", stationDF$ID %>% paste0(collapse = ","),
                       # Dataset Start Date
                       "&startDate=", StartDate$date,
                       # Dataset End Date
                       "&endDate=", EndDate$date,
                       # Requesting Daily TMIN, TMAX, and PRECIP
                       "&dataItems=day-air-tmp-min,day-air-tmp-max,day-precip",
                       # Metric units
                       "&unitOfMeasure=M")
  
  
  
  # Ask for a JSON-formatted response
  res <- GET(requestURL,
              add_headers("Accept" = "application/json"))
  
  
  
  # NOTE: The above code can result in an error 
  # (After hanging for a while, you get an error in 
  # curl::curl_fetch_memory(url, handle = handle) that says 
  # "Recv failure: Connection was reset")
  # You may need to clear your browser cache/cookies to fix it 
  # (This is a problem related to our network firewall and CIMIS cookies/cache)
  
  
  
  # Wait a while after making the request
  Sys.sleep(runif(1, min = 1.3, max = 1.8))
  
  
  
  # Check the content of the response
  res <- content(res)
  
  
  
  # The content of 'res' should be a JSON from CIMIS
  # There should be one entry in the "Providers" sub-list (CIMIS)
  stopifnot(res[["Data"]][["Providers"]] %>% length() == 1)
  stopifnot(tolower(res[["Data"]][["Providers"]][[1]][["Name"]]) == "cimis")
  
  
  
  # Also make sure that the "Records" sublist is not empty
  stopifnot(length(res$Data$Providers[[1]]$Records) > 0)
  stopifnot("Date" %in% names(res$Data$Providers[[1]]$Records[[1]]))
  stopifnot("Station" %in% names(res$Data$Providers[[1]]$Records[[1]]))
  stopifnot("DayAirTmpMin" %in% names(res$Data$Providers[[1]]$Records[[1]]))
  stopifnot("DayAirTmpMax" %in% names(res$Data$Providers[[1]]$Records[[1]]))
  stopifnot("DayPrecip" %in% names(res$Data$Providers[[1]]$Records[[1]]))
  
  
  
  # Extract data from different columns within the records in 'res'
  # Store that information in a tibble
  compiledDF <- tibble(DATE = res$Data$Providers[[1]]$Records %>%
                         map_chr(~ .[["Date"]]) %>% as.Date(format = "%Y-%m-%d"),
                       
                       STATION_ID = res$Data$Providers[[1]]$Records %>%
                         map_chr(~ .[["Station"]]) %>% as.numeric(),
                       
                       TMIN = res$Data$Providers[[1]]$Records %>%
                         map_chr(~ .[["DayAirTmpMin"]][["Value"]]) %>% as.numeric(),
                       
                       TMAX = res$Data$Providers[[1]]$Records %>%
                         map_chr(~ .[["DayAirTmpMax"]][["Value"]]) %>% as.numeric(),
                       
                       PRECIP = res$Data$Providers[[1]]$Records %>%
                         map_chr(~ .[["DayPrecip"]][["Value"]]) %>% as.numeric(),
                       
                       TMIN_QC = res$Data$Providers[[1]]$Records %>%
                         map_chr(~ .[["DayAirTmpMin"]][["Qc"]]) %>% trimws(),
                       
                       TMAX_QC = res$Data$Providers[[1]]$Records %>%
                         map_chr(~ .[["DayAirTmpMax"]][["Qc"]]) %>% trimws(),
                       
                       PRECIP_QC = res$Data$Providers[[1]]$Records %>%
                         map_chr(~ .[["DayPrecip"]][["Qc"]]) %>% trimws())
                       

  
  # Slower procedure that uses a loop:
  # 
  # compiledDF <- tibble(DATE = Date(0),
  #                      STATION_ID = numeric(0),
  #                      TMIN = numeric(0),
  #                      TMAX = numeric(0),
  #                      PRECIP = numeric(0),
  #                      TMIN_QC = character(0),
  #                      TMAX_QC = character(0),
  #                      PRECIP_QC = character(0))
  # 
  # 
  # 
  # # Iterate through the records returned by CIMIS
  # for (i in 1:length(res$Data$Providers[[1]]$Records)) {
  #   
  #   compiledDF <- compiledDF %>%
  #     rbind(tibble(DATE = as.Date(res$Data$Providers[[1]]$Records[[i]]$Date, format = "%Y-%m-%d"),
  #                  STATION_ID = as.numeric(res$Data$Providers[[1]]$Records[[i]]$Station),
  #                  TMIN = res$Data$Providers[[1]]$Records[[i]]$DayAirTmpMin$Value,
  #                  TMAX = res$Data$Providers[[1]]$Records[[i]]$DayAirTmpMax$Value,
  #                  PRECIP = res$Data$Providers[[1]]$Records[[i]]$DayPrecip$Value,
  #                  TMIN_QC = res$Data$Providers[[1]]$Records[[i]]$DayAirTmpMin$Qc,
  #                  TMAX_QC = res$Data$Providers[[1]]$Records[[i]]$DayAirTmpMax$Qc,
  #                  PRECIP_QC = res$Data$Providers[[1]]$Records[[i]]$DayPrecip$Qc))
  #   
  # }
  
  
  
  stop("This function has not been completed")
  
  
  
}



#### Script Execution ####


cat("Starting 'CIMIS_Static_Scraper.R'...")


mainProcedure(StartDate, EndDate, includeForecast)


remove(mainProcedure, urlBasedCall, apiBasedCall)
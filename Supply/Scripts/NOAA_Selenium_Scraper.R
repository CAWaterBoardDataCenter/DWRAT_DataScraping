# As a replacement for the downsizer application, collect climate data from NOAA
# Get daily values for Precipitation and Temperature (Min/Max) in mm and degrees Celsius, respectively


library(tidyverse)
library(RSelenium)
library(wdman)
library(binman)
library(netstat)
library(readxl)



mainProcedure <- function (StartDate, EndDate) {
  
  
  # Get an open network port
  openPort <- free_port()
  
  
  
  # Prepare options to set the default download folder location to the "WebData" folder
  # The specified settings are as follows:
  #   - By default, don't allow popups
  #   - Don't open a prompt to ask where to download files
  #   - The default download directory is the "WebData" folder
  # NOTE: The download directory must be specified using backslashes (\), not forward slashes (/)
  exCap <- list(chromeOptions = list(prefs = list("profile.default_content_settings.popups" = 0L,
                                                  "download.prompt_for_download" = FALSE,
                                                  "download.default_directory" = paste0(getwd(), "/WebData") %>%
                                                    str_split("/") %>% unlist() %>% paste0(collapse = "\\"))))
  
  
  
  # Open the server and wait a little bit
  server <- chrome(port = openPort, 
                   version = paste0(app_dir("chromedriver", check = FALSE), "/win32") %>%
                     list.files() %>% tail(1), 
                   verbose = FALSE)
  
  Sys.sleep(1.5)
  
  
  
  # Prepare the Chrome instance and wait a bit
  rd <- remoteDriver(browserName = "chrome", port = openPort,
                     extraCapabilities = exCap)
  
  Sys.sleep(1.5)
  
  
  
  # Open the bot window (and wait a little)
  rd$open(silent = TRUE)
  
  Sys.sleep(1.5)
  
  
  
  # In a separate function, perform the operations to request a file from NOAA
  noaaRequest(rd, StartDate, EndDate)
  
  
  
  # Next, visit Yahoo Mail to download the file
  # (That function will return the Order Number)
  orderNum <- yahooDownload(rd)
  
  
  
  # The next steps do not require dynamic web scraping
  # Terminate the remote driver and server
  try(rd$quit(), silent = TRUE)
  try(server$stop(), silent = TRUE)
  
  
  
  # The downloaded file will have a different format from the typical Downsizer output
  # Adjust its format to be compatible with 'Downsizer_Processor.R'
  paste0("WebData/", orderNum, ".csv") %>%
    fileAdjustment(EndDate)
  
  
  
  # Include a completion message
  cat('Done!\n')
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



clickButton <- function(rd, val, searchType = "xpath") {
  
  # Use an element's attribute/value/xpath to locate it ('xpath' is the default method)
  # Then click on it
  
  
  # Find the element
  foundElement <- rd$findElement(using = searchType, value = val)
  
  
  
  # Error Check
  # Stop if no element is found or if more than one element is found
  stopifnot(length(foundElement) == 1)
  
  
  
  # Click on the element
  foundElement$clickElement()
  
  
  
  # Wait around a second before continuing
  Sys.sleep(runif(1, min = 1, max = 1.5))
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



fillInput <- function (rd, xpathVal, input) {
  
  # Use an element's XPATH to locate it
  # Then type 'input' into it
  
  
  # Find the element
  foundElement <- rd$findElement(using = "xpath", value = xpathVal)
  
  
  
  # Error Check
  # Stop if no element is found or if more than one element is found
  stopifnot(length(foundElement) == 1)
  
  
  
  # Input the text into the element
  foundElement$sendKeysToElement(sendKeys = list(input))
  
  
  
  # Wait up to a second before continuing
  Sys.sleep(runif(1, min = 0.25, max = 1))
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



loopWait <- function (rd, breakStr, sleepTime = 5) {
  
  # Wait in an infinite while loop until 'breakStr' is detected in the page's HTML
  while (TRUE) {
    
    # Suspend operations for several seconds (default is 5)
    Sys.sleep(sleepTime)
    
    
    # If 'breakStr' is detected in the page's HTML, break the loop
    if (rd$getPageSource() %>% str_detect(breakStr)) {
      break
    }
    
  }
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



noaaRequest <- function (rd, StartDate, EndDate) {
  
  # Visit NOAA's website and request temperature and precipitation data
  
  
  
  # The URL that will be visited contains the station IDs for which data will be collected
  # The timeframe and variables of interest must also be specified
  primaryURL <- paste0("https://www.ncei.noaa.gov/access/search/data-search/daily-summaries?dataTypes=PRCP&dataTypes=TMAX&dataTypes=TMIN&startDate=",
                       StartDate$date, 
                       "T00:00:00&endDate=",
                       EndDate$date, 
                       "T23:59:59",
                       read_xlsx("InputData/RR_PRMS_StationList (2023-09-05).xlsx", sheet = "StationList") %>%
                         filter(`Observed and PRISM Station Guide` == "DOWNSIZER") %>%
                         select(...5) %>% unlist() %>% as.vector() %>%
                         paste0("&stations=", .) %>% unique() %>%
                         paste0(collapse = ""))
  
  
  rd$navigate(primaryURL)
  
  Sys.sleep(3)
  
  
  # Wait longer if not all elements of the page have loaded yet
  loopWait(rd, "dataTypes: Minimum temperature", sleepTime = 3)
  
  
  
  # Click the "Select All" button to add all of the listed stations for data export
  clickButton(rd, '//*[@id="resultHeaderSelectAllBtn"]/button[1]')
  
  
  # Additional waiting may be needed if the new menu takes a while to load
  Sys.sleep(2)
  
  
  
  # The exported file should be a CSV. Click on "CSV"
  clickButton(rd, '//*[@id="main-content"]/ng-component/div/div/app-cart-alert/div[1]/form/div[1]/fieldset/div[2]/label')
  
  
  
  # Click the button to open up the export customization settings
  clickButton(rd, '//*[@id="main-content"]/ng-component/div/div/app-cart-alert/div[1]/form/div[2]/div/button[2]')
  
  
  # Wait an extra few seconds to let the new menu open up
  Sys.sleep(2)
  
  
  
  # Click "Yes" for including attributes, station locations, and station names
  clickButton(rd, '//*[@id="main-content"]/ng-component/div/div/app-cart-alert/div[2]/div/app-order-options/div/div/div[2]/div[2]/div[3]/div[2]/app-radio-select/div/div/div/div[2]/div/label')
  clickButton(rd, '//*[@id="main-content"]/ng-component/div/div/app-cart-alert/div[2]/div/app-order-options/div/div/div[2]/div[2]/div[4]/div[2]/app-radio-select/div/div/div/div[2]/div/label')
  clickButton(rd, '//*[@id="main-content"]/ng-component/div/div/app-cart-alert/div[2]/div/app-order-options/div/div/div[2]/div[2]/div[5]/div[2]/app-radio-select/div/div/div/div[2]/div/label')
  
  
  
  # After that, choose Metric units for the output
  clickButton(rd, '//*[@id="main-content"]/ng-component/div/div/app-cart-alert/div[2]/div/app-order-options/div/div/div[2]/div[2]/div[6]/div[2]/app-radio-select/div/div/div/div[1]/div/label')
  
  
  
  # Next, add the order to the cart
  clickButton(rd, '//*[@id="main-content"]/ng-component/div/div/app-cart-alert/div[2]/div/app-order-options/div/div/div[3]/button[2]')
  
  
  # Wait a bit to let the cart update
  Sys.sleep(2)
  
  
  
  # Proceed to cart checkout
  clickButton(rd, '//*[@id="main-content"]/ng-component/div/div/app-cart-alert/div[1]/div[1]/div/button[2]')
  
  
  # Give the page extra time to load
  Sys.sleep(4)
  
  
  
  # For the email address, input the SDA dummy email
  fillInput(rd, '//*[@id="emailInput"]', "sda_noaa_receiver@yahoo.com")
  fillInput(rd, '//*[@id="confirmInput"]', "sda_noaa_receiver@yahoo.com")
  
  
  
  # For the "Submit" button to become active, we need to click out of the "Confirm email" box
  # It doesn't need to be anywhere special; just click on the random icon to the side
  clickButton(rd, '//*[@id="main-content"]/app-cart-view/div/div[2]/div[2]/div/div[2]/form/div[2]/div/div[1]/span/span')
  
  
  
  # Click "Submit"
  clickButton(rd, '//*[@id="main-content"]/app-cart-view/div/div[2]/div[2]/div/div[2]/form/div[3]/button')
  
  
  
  # Keep waiting until the order is successfully submitted
  loopWait(rd, "Your order was successfully submitted and an email with a link")
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



yahooDownload <- function (rd) {
  
  # Visit Yahoo Mail to receive the completion email
  # Then, download the CSV file
  
  
  
  # Navigate to Yahoo Mail next
  rd$navigate("https://login.yahoo.com/?.src=ym&activity=mail-direct&.lang=en-US&.intl=us")
  
  
  # Let the page load
  Sys.sleep(4)
  
  
  
  # Input the SDA email
  fillInput(rd, '//*[@id="login-username"]', "sda_noaa_receiver@yahoo.com")
  
  
  # Wait an extra second
  Sys.sleep(1)
  
  
  
  # Click on "Next"
  clickButton(rd, '//*[@id="login-signin"]')
  
  
  # Wait for the process to complete
  Sys.sleep(2)
  
  
  
  # Input the password
  fillInput(rd, '//*[@id="login-passwd"]', 
            "ItsGenerallyFrownedUponToHaveAPasswordWrittenExplicitlyInAScriptLikeThis")
  
  
  # Wait an extra second
  Sys.sleep(1)
  
  
  
  # Click on "Next"
  clickButton(rd, '//*[@id="login-signin"]')
  
  
  # Let the page load
  loopWait(rd, '<input role="combobox" aria-autocomplete="both" aria-expanded="false"')
  
  
  
  # Input inbox filters into the search bar
  # Filtering to the Order Confirmation emails from NOAA
  fillInput(rd, '//*[@id="mail-search"]/div/div/div[1]/ul/li/div/div/input[1]', 
            "is:unread from:noreply@noaa.gov subject:submit")
  
  
  # Wait an extra second
  Sys.sleep(1)
  
  
  # Click the search button
  clickButton(rd, '//*[@id="mail-search"]/div/button')
  
  
  # Wait another second
  Sys.sleep(1)
  
  
  
  # Get the first <a> element with "Climate Data Online" in its link text
  # (This will be the latest unread email from NOAA)
  clickButton(rd, 'Climate Data Online', searchType = "partial link text")
  
  
  # Give extra time for the email to load
  Sys.sleep(2)
  
  
  
  # Get the Order Number from the email
  orderNum <- rd$getPageSource() %>% str_extract_all("Order <span>[0-9]+</span>") %>%
    str_remove_all("[^0-9]") %>% as.numeric()
  
  
  
  # Return to the Inbox
  clickButton(rd, '//*[@id="app"]/div[2]/div/div[1]/nav/div/div[3]/div[1]/ul/li[1]/div/a/span[1]/span')
  
  
  
  # Input new inbox filters into the search bar
  # Filtering to the Order Completion emails from NOAA with the order number saved to 'orderNum'
  fillInput(rd, '//*[@id="mail-search"]/div/div/div[1]/ul/li/div/div/input[1]', 
            paste0("is:unread from:noreply@noaa.gov subject:complete keyword: ", orderNum))
  
  
  # Wait an extra second
  Sys.sleep(1)
  
  
  
  # Click the search button
  clickButton(rd, '//*[@id="mail-search"]/div/button')
  
  
  # Wait another two seconds
  Sys.sleep(2)
  
  
  
  # Wait until an email is detected with these filters
  while (!(str_detect(rd$getPageSource(), paste0("Climate Data Online request ", orderNum, " comp")))) {
    
    Sys.sleep(4)
    
    # Refresh the page again
    rd$refresh()
    
    # Return to the Inbox
    clickButton(rd, '//*[@id="app"]/div[2]/div/div[1]/nav/div/div[3]/div[1]/ul/li[1]/div/a/span[1]/span')
    
    # Input new inbox filters into the search bar
    # Filtering to the Order Completion emails from NOAA with the order number saved to 'orderNum'
    fillInput(rd, '//*[@id="mail-search"]/div/div/div[1]/ul/li/div/div/input[1]', 
              paste0("is:unread from:noreply@noaa.gov subject:complete keyword: ", orderNum))
    
    # Wait an extra second
    Sys.sleep(1)
    
    # Click the search button
    clickButton(rd, '//*[@id="mail-search"]/div/button')
    
    # Wait another two seconds
    Sys.sleep(2)
    
  }
  
  
  
  # Navigate directly to that webpage (it will automatically initiate a download)
  rd$navigate(paste0("https://www.ncei.noaa.gov/orders/cdo/", orderNum, ".csv"))
  
  
  Sys.sleep(2)
  
  
  
  # Give the file some time to download
  while (sum(grepl("\\.CRDOWNLOAD", list.files("WebData/"), ignore.case = TRUE)) > 0) {
    Sys.sleep(3)
  }
  
  
  
  # Return the order number
  return(orderNum)
  
}



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



#### Executing the Script ####


cat("Starting 'NOAA_Scraper.R'...\n")


mainProcedure(StartDate, EndDate)


remove(clickButton, fileAdjustment, fillInput, loopWait, 
       mainProcedure, noaaRequest, yahooDownload)
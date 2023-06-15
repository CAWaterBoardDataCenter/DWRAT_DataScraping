# The Downsizer application is one of the first steps in running the PRMS Model
# However, it cannot be perfectly automated

# This script aids the user in running that application
# It also prepares the file output for the next step

# Note: This script will only work on devices that use Windows


#### Dependencies ####

require(tidyverse)
require(utils) # This is a basic R package so it doesn't technically need to be specified
require(KeyboardSimulator)

#### Script Procedure ####

mainProcedure <- function (StartDate = data.frame(year = 2023, month = 4, day = 1, date = as_date("2023-04-01")), EndDate = data.frame(year = 2023, month = 5, day = 29, date = as_date("2023-05-29"))) {
  
  # This is the main body of the script
  
  
  
  # First, gather the locations of important files/directories
  
  # Find the locations of two important files:
  #   (1) "downsizer-client-64bit-3.2.4.jar"
  #          This is the executable file for the Downsizer program
  #   (2) "downsizer_sta.csv"
  #          This CSV file contains the stations that will be input into the program
  
  
  # If following the tutorial, the user should have those files in 
  # "RR_PRMS/PRMS/input/data_file_prep/downsizer_raw_data/"
  
  # The file paths should be specified in the Downsizer section of "File_Paths_and_Dirs.txt"
  
  
  path_to_jar <- readLines("InputData/File_Paths_and_Dirs.txt", warn = FALSE) %>%
    str_subset("downsizer client.+\\.jar") %>% str_extract("\\s{3,}.+$") %>% trimws()
  
  
  # Error Check: The length of 'path_to_jar' should be 1
  stopifnot(length(path_to_jar) == 1)
  
  
  # Also, verify that the file exists in the specified path
  stopifnot(file.exists(path_to_jar))
  
  
  # Find "downsizer_sta.csv" next
  path_to_csv <- readLines("InputData/File_Paths_and_Dirs.txt", warn = FALSE) %>%
    str_subset("downsizer station.+\\.csv") %>% str_extract("\\s{3,}.+$") %>% trimws()
  
  
  # Perform similar checks as done before for the ".jar" file
  stopifnot(length(path_to_csv) == 1)
  stopifnot(file.exists(path_to_csv))
  
  
  
  # Also, get the location of the user's GitHub repository (specifically the "WebData" folder)
  # This is where the final output will be stored
  path_Github_Dir <- readLines("InputData/File_Paths_and_Dirs.txt", warn = FALSE) %>%
    str_subset("WebData.+GitHub") %>% str_extract("\\s{3,}.+$") %>% trimws()
  
  
  # Perform error checks for this path as well
  stopifnot(length(path_Github_Dir) == 1)
  stopifnot(dir.exists(path_Github_Dir))
  
  
  
  # The next step is opening the actual Downsizer program
  # Use the system() command to begin execution
  system(paste0("java -jar ", path_to_jar), wait = FALSE, ignore.stdout = TRUE)
  
  
  # Wait until the Downsizer program opens
  # (It will appear among the Windows handles once ready)
  while (!("MoWS Downsizer" %in% names(getWindowsHandles("all")))) {
    Sys.sleep(2)
  }
  
  
  
  # Minimize all open windows before continuing
  minimizeWindows()
  
  
  # Automate the execution of the Downsizer program
  # (The returned value will be the path to the output PRMS file)
  path_to_prms <- automatedDownsizer(path_to_jar, path_to_csv, StartDate, EndDate)
  
  
  
  # The final step is to copy the PRMS file as a CSV file to the Github directory
  # (Specifically, the "WebData" folder)
  # This path should also be specified in "File_Paths_and_Dirs.txt"
  file.copy(path_to_prms,
            paste0(path_Github_Dir, 
                   path_to_prms %>% str_extract("\\\\Downsizer_[0-9\\-]+\\.prms$") %>% str_replace("\\.prms$", "\\.csv")),
            overwrite = TRUE)
  
  
  
  # Finally, restore the RStudio window after these operations
  arrangeWindows("restore", getWindowsHandles("all", pattern = "RStudio", minimized = TRUE))
  
  
  # Return nothing
  return(invisible(NULL))
  
}


minimizeWindows <- function () {
  
  # Minimize all windows except for important system windows
  # (So ignore "Windows Input Experience" and "Program Manager")
  
  arrangeWindows("minimize", 
                 windows = getWindowsHandles("all", 
                                             pattern = c("[^(Windows Input Experience)|(Program Manager)]")))
  
  
  # Wait a little to allow time to minimize the windows
  Sys.sleep(1)
  
  
  # Return nothing
  return(invisible(NULL))
  
}


automatedDownsizer <- function (path_to_jar, path_to_csv, StartDate, EndDate) {
  
  # Navigate through the GUI of Downsizer and perform the necessary actions to execute it
  
  
  
  # Get the Windows handle information for the downsizer program
  downsizerHandle <- getWindowsHandles("all", pattern = "^MoWS Downsizer$", minimized = TRUE)
  
  
  # Currently, all windows should be minimized
  # Restore the window for Downsizer
  arrangeWindows("restore", downsizerHandle)
  Sys.sleep(2)
  
  
  
  # This function relies on hardcoded values for the screen coordinates
  # (i.e., the places where the mouse is clicked)
  
  
  # The dimensions of the downsizer program vary proportionately depending on the screen resolution
  # From prior testing, it seems that, when Downsizer is opened:
  #    The window width is approximately 44.8% of the screen width
  #    The window height is approximately 85.7% of the screen height
  # Therefore, the coordinates will need to be adjusted to run on different devices
  
  
  # To help determine where to click on the screen, this device's screen resolution is needed
  # It will be used to adjust the values that were based on my screen resolution (1535 x 816)
  
  
  # Get the current device's screen resolution (width x height)
  currentScreen <- getScreenSize()
  
  
  
  # Now, the actual automation can begin
  
  
  
  # First, click on "Period"
  mouseMove(643, 94, currentScreen)
  mouseClick()
  
  
  # Input the Start Year (if it isn't the default value 1900)
  if (StartDate$year != 1900) {
    strInput(88, 103, currentScreen, StartDate$year)
  }
  
  
  # Input the Start Month (if it isn't the default value 1)
  if (StartDate$month != 1) {
    strInput(150, 103, currentScreen, StartDate$month)
  }
  
  
  # Input the Start Day (if it isn't the default value 1)
  if (StartDate$day != 1) {
    strInput(205, 103, currentScreen, StartDate$day)
  }
  
  
  # To decide whether the end date parameters must be changed, check today's date
  # (In Downsizer, today's date is the default for the end of the period)
  dateToday <- as_date(Sys.Date())
  
  
  # Change the End Year (if it's different)
  if (EndDate$year != year(dateToday)) {
    strInput(88, 128, currentScreen, EndDate$year)
  }
  
  
  # Change the End Month (if it's different)
  if (EndDate$month != month(dateToday)) {
    strInput(150, 128, currentScreen, EndDate$month)
  }
  
  
  # Change the End Day (if it's different)
  if (EndDate$day != day(dateToday)) {
    strInput(205, 128, currentScreen, EndDate$day)
  }
  
  
  
  # Next, click on the "Output File" button
  mouseMove(643, 155, currentScreen)
  mouseClick()

  
  # Prepare the output file path
  # At first, it will be stored in the same directory as the client executable
  # Its name will be "Downsizer_{EndDate}.prms"
  outputPath <- path_to_jar %>%
    str_remove("downsizer.client.+jar$") %>%
    paste0(., "Downsizer_", EndDate$date, ".prms")
  
  
  # Also, if a file with that name already exists, remove it
  if (file.exists(outputPath)) {
    file.remove(outputPath)
  }
  
  
  # Click on the "File Path" box and input this file path
  strInput(272, 141, currentScreen, outputPath)

  
  
  # Click on the "Station Addition" button next
  mouseMove(642, 238, currentScreen)
  mouseClick()
  
  
  # Click on the "Climate IDs file path" text box underneath "Add stations from file"
  # Then, type in the filepath to the stations CSV file ('path_to_csv')
  strInput(318, 355, currentScreen, path_to_csv)
  
  
  # Click on "Add stations"
  # Wait a little bit extra afterwards
  Sys.sleep(0.5)
  mouseMove(309, 442, currentScreen)
  mouseClick()
  Sys.sleep(2)
  
  
  
  # After that, click on the "Units" button
  mouseMove(642, 386, currentScreen)
  mouseClick()
  
  
  # Select the units that will be used in the program
  
  
  # Choose Celsius for temperature
  mouseMove(143, 205, currentScreen)
  mouseClick()
  
  
  # Click on "mm" for precipitation
  mouseMove(246, 205, currentScreen)
  mouseClick()
  
  
  # Click on "ft" for elevation
  mouseMove(349, 229, currentScreen)
  mouseClick()
  
  
  
  # Click on the "Run" button
  mouseMove(644, 535, currentScreen)
  mouseClick()
  
  
  # Click on the second "Run" button
  mouseMove(508, 554, currentScreen)
  mouseClick()
  
  
  # Wait until the output file is created
  while (!file.exists(outputPath)) {
    Sys.sleep(2)
  }
  
  
  # Wait a little bit extra afterwards
  Sys.sleep(1)
  
  
  # At the end of the operations, close the Downsizer program
  system('taskkill /IM java.exe /f /t', ignore.stdout = TRUE)
  
  
  # Then, return the path to the output file
  return(outputPath)
  
}


getScreenSize <- function () {
  
  # Perform a test to determine this screen's resolution
  # A vector containing the width and height units of this screen will be returned
  
  
  # This goal can be achieved through the KeyboardSimulator package
  # Attempt to move the mouse as far as possible into the bottom-right corner
  # Those coordinates will be the screen size
  
  
  # To do this, give really big numbers to mouse.move()
  
  # Inputting "10^9" twice means move to the screen pixel 
  # with x- and y-coordinates of "1 billion"
  
  # The user's screen is unlikely to be this large, 
  # so it should reach the limits of the device
  
  # From there, mouse.get_cursor() will return the actual maximum screen coordinates
  # (For reference, on my laptop, it's 1535 x 816)
  
  mouse.move(10^9, 10^9)
  screenSize <- mouse.get_cursor()
  

  # Error Check
  # (In case somebody really does have a screen with at least 1,000,000,000 pixels)
  stopifnot(!(10^9 %in% screenSize))
  
    
  # Return 'screenSize'
  return(screenSize)
  
}


mouseMove <- function (x, y, currentScreen) {
  
  # A wrapper function around mouse.move()
  
  # Every call to mouse.move() requires an adjustment based on the screen resolution
  # Write that code once here; then simply call mouseMove() instead of mouse.move()
  
  
  # 'x' and 'y' contain the coordinates used on my device
  # They can be "transformed" into coordinates for the current device
  # A ratio of the widths and heights will transform the x- and y-coordinates, respectively
  
  mouse.move(x / 1535 * currentScreen[1], y / 863 * currentScreen[2])
  
  
  # Return nothing
  return(invisible(NULL))
  
  
}


mouseClick <- function () {
  
  # A wrapper function around mouse.click()
  # After pretty much every click, there should be a small amount of waiting
  
  # Rather than write that out each time, 
  # calling mouseClick() will take care of that
  
  
  mouse.click()
  
  
  # Wait between half a second to a full second before proceeding
  # (Use a uniform distribution to randomly choose that value)
  Sys.sleep(runif(1, min = 0.5, max = 1))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



strInput <- function (x, y, currentScreen, inputStr) {
  
  # In the Downsizer application, type a value into a text box
  
  
  # Click on the text box to open it
  mouseMove(x, y, currentScreen)
  mouseClick()
  
  
  # Type 'inputStr' into the box character-by-character
  # Then press "Enter"
  inputStr %>% as.character() %>% keybd.type_string()
  keybd.press("enter")
  
  
  # Return nothing
  return(invisible(NULL))
  
}


approxDownsizerSize <- function (widthRatio = 0.448, heightRatio = 0.857) {
  
  # This function returns a vector containing the approximate dimensions of the Downsizer application
  
  # From prior testing, it seems that, when Downsizer is opened:
  #    The window width is approximately 45% of the screen width
  #    The window height is approximately 86% of the screen height
  
  #   
  
  
  # First, get the maximum dimensions of the user's screen
  # This can be achieved through the KeyboardSimulator package
  
  
  # By moving the user's cursor to the bottom-right corner and getting the coordinates,
  # the screen size can be verified
  
  # To do this, give really big numbers to mouse.move()
  
  # Inputting "10^9" twice means move to the screen pixel 
  # with x- and y-coordinates of "1 billion"
  
  # The user's screen is unlikely to be this large, 
  # so it should reach the limits of the device
  
  # From there, mouse.get_cursor() will return the actual maximum screen coordinates
  # (For reference, on my laptop, it's 1535 x 816)
  
  mouse.move(10^9, 10^9)
  screenSize <- mouse.get_cursor()
  
  
  # If this screen is large enough, the returned value will be TRUE
  return(screenSize[1] >= minWidth & screenSize[2] >= minHeight)
  
}






#### Executing the Script ####

mainProcedure(StartDate, EndDate)
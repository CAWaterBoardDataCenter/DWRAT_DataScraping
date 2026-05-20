# Plot the watershed streamflow values stored in the PRMS model output file 
# ("rr_budget.out2")

# Include precipitation data in these charts (using historic PRISM precipitation
# data for PRMS's model domain as well as the gage data stored in the PRMS 
# DAT input file)


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'HLP_010_Plot_PRMS_Streamflow.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that a proper directory exists for model input and output files
  # The PRMS model outputs are stored there
  cat("\n[1/2]\tChecking directories and files...\n")
  
  
  # Get the path to that directory
  dirPath <- validateHydroFolder(startDate, endDate)
  
  
  # Check for the "rr_budget.out2" file
  # It was one of the output files from PRMS
  outPath <- paste0(dirPath, "/PRMS/output/rr_budget.out2") |>
    checkForPreviousOutput()
  
  
  # Read in the "out2" file
  # (Note: `read_out2` also performs some data validation in its procedure)
  outDF <- outPath |>
    read_out2()
  
  
  # Get the PRISM average precipitation dataset as well
  # We want as much data for the PRMS model domain as possible
  # That would entail relying on the historic domain CSV, the recently downloaded
  # CSV for the current water year, and additional PRISM data for gaps between
  # the files
  
  # Use functions from another script to handle that process
  c("gatherPrecipPRISM", "validateAndSummarizePRISM") |>
    map(~ functionStealer("Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", .))
  
  
  # Collect PRISM precipitation data that is averaged for 
  # the entire PRMS model domain
  prismDF <- gatherPrecipPRISM(dirPath, endDate, "PRMS")
  
  
  # Import more precipitation data from the DAT file that was used to run PRMS
  datDF <- gatherPrecipDAT(dirPath, startDate, endDate)
  
  
  # For consistency, have both 'prismDF' and 'datDF' use "PRECIP" as the
  # name for their precipitation columns (and "Date" for dates)
  
  # Also, their units should be inches, not millimeters
  # (1 in = 25.4 mm)
  prismDF <- prismDF |>
    mutate(PRECIP = `ppt (mm)` / 25.4)
  
  
  datDF <- datDF |>
    mutate(PRECIP = PRECIP / 25.4)
  
  
  cat("\tDone!\n\n")
  
  
  # The next step is to plot the streamflow and precipitation data
  cat("[2/2]\tPlotting streamflow and precipitation data...\n")
  
  
  # Generate plots for this data
  plotModelResults(dirPath, outDF, prismDF, datDF)
  
  
  # Output a completion message
  cat(col_green("\n'HLP_010_Plot_PRMS_Streamflow.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



gatherPrecipDAT <- function (dirPath, startDate, endDate) {
  
  # Use the DAT file that is input into PRMS
  
  # It contains precipitation data from different gages
  
  # Take the average of these values to get an estimate of basin precipitation 
  
  
  # Get the path to the DAT file and confirm that it exists
  datPath <- paste0(dirPath, "/PRMS/Input/DAT_PRMS_", Sys.info()[["user"]], "_",
                    startDate, "_", endDate, ".dat") |>
    checkForPreviousOutput()
  
  
  # Read in 'datPath'
  datDF <- getFile(datPath)
  
  
  # Check for the location of the header row
  headerIndex <- grep("^\\s*#+", datDF)
  
  
  # Output an error message if it cannot be found
  if (length(headerIndex) != 1) {
    
    paste0("Could Not Locate Column Header\n\n", 
           "This script attempted to find the header row in the PRMS DAT ",
           "file. However, the regular expression that identifies this ",
           "line returned ", length(headerIndex), " matches.\n\n", 
           "Please investigate '", datPath, "'") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Extract the header rows from 'datDF'
  # They don't have column names specifically
  # Instead, each category of variable has the number of instances of those 
  # types of columns
  headerRows <- datDF[1:headerIndex] |>
    str_split("\t") |> unlist() |>
    str_subset("[0-9]")
  
  
  # Extract the categories in 'headerRows' as well as the number of each type
  headerDF <- tibble(TYPE = headerRows |>
                       str_extract("^[a-zA-Z]+"),
                     N = headerRows |>
                       str_extract("[0-9]+$") |>
                       as.numeric())
  
  
  # Create names for each category of variable
  # Use the "N" column to create multiple instances of each type
  headerNames <- map2(headerDF$TYPE, headerDF$N, ~ paste0(.x, "_", 1:.y)) |>
    unlist() |> tolower()
  
  
  # Add datetime column names to 'headerNames'
  headerNames <- c("year", "month", "day", "h", "m", "s",
                   headerNames)
  
  
  # Consider only the rows after 'headerIndex'
  # Then, split the values at each tab space and reformat the data
  # Shape it into a matrix and then a tibble
  # Finally, apply the column headers to it
  datDF <- datDF[(headerIndex + 1):length(datDF)] |>
    str_split("\t") |> unlist() |>
    matrix(ncol = length(headerNames), byrow = TRUE) |>
    as_tibble(.name_repair = "minimal") |>
    set_names(headerNames)
  
  
  # Use 'year', 'month', and 'day' to define a "Date" variable
  # After that, select the new "Date" column and any precipitation columns
  datDF <- datDF |>
    mutate(Date = paste0(year, "-", month, "-", day) |>
             as.Date("%Y-%m-%d")) |>
    select(Date, contains("precip"))
  
  
  # Then, calculate a new "PRECIP" column 
  # Take the average of the precipitation values
  # Make sure the precipitation values are numeric and then reshape the tibble
  # so that all precipitation columns appear in the same column
  # After that, group by "Date" and average precipitation values 
  # that occurred on the same day
  datDF <- datDF |>
    mutate(across(contains("precip"), as.numeric)) |>
    pivot_longer(contains("precip"), 
                 names_to = "STATION", values_to = "PRECIP") |>
    group_by(Date) |>
    summarize(PRECIP = mean(PRECIP), .groups = "drop")
  
  
  # Return 'datDF' afterwards
  return(datDF)
  
}



plotModelResults <- function (dirPath, outDF, prismDF, datDF) {
  
  # Create plots to inspect the streamflow data in 'outDF'
  
  # With both daily and monthly versions of 'outDF', 
  # plot streamflow on these timescales:
  #   (*) 1-year 
  #   (*) 5-year 
  #   (*) 10-year 
  #   (*) Full dataset range 
  
  # Include precipitation data in these plots
  # For precipitation sources, there are two options ('prismDF' and 'datDF')
  
  
  # 'outDF' has daily streamflow (in mean cfs per day)
  # Convert that into acre-feet per day
  # (Add a date column too)
  
  # ft^3/s * 60 s/min * 60 min/hr * 24 hr/day * 1/43559.9 AF/ft^3
  dailyDF <- outDF |>
    mutate(FLOW = `sim (cfs)` * 60 * 60 * 24 / 43559.9,
           DATE = paste0(Year, "-", mo, "-", day) |>
             as.Date(format = "%Y-%m-%d")) |>
    select(DATE, FLOW)
  
  
  # After that, create a new folder in the PRMS "output" directory 
  # This will hold the data output by this function
  newDir <- prepNewDirectory(dirPath)
  
  
  # Once 'newDir' has been established, 
  # create plots for different timescales
  
  
  # Borrow function that can help generate plots
  c("generateStreamflowPlot", "getNiceAxisBreaks", "setPrecipColumnWidths") |>
    map(~ functionStealer("Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", .))
  
  
  # First generate plots for the full dataset
  generatePlots(dailyDF, newDir, "All", prismDF, datDF)
  
  
  # If the dataset contains at least one year of data, 
  # generate a one-year version too
  if (nrow(dailyDF) > 365) {
    
    generatePlots(dailyDF, newDir, "1_yr", prismDF, datDF)
    
  }
  
  
  # If the dataset contains at least five years of data, 
  # generate a five-year version too
  if (nrow(dailyDF) > 365 * 5) {
    
    generatePlots(dailyDF, newDir, "5_yr", prismDF, datDF)
    
  }
  
  
  # If the dataset contains at least ten years of data, 
  # generate a ten-year version too
  if (nrow(dailyDF) > 365 * 10) {
    
    generatePlots(dailyDF, newDir, "10_yr", prismDF, datDF)
    
  }
  
  
  # Save 'outDF' to 'newDir' as well
  outDF |>
    writeOutput(paste0(newDir, "/PRMS_Reformatted_Out2.csv"),
                quietly = TRUE)
  
  
  # 'prismDF' was already archived in `gatherPrecipPRISM`
  # 'datDF' is derived from the DAT that is already stored in this folder
  
  
  # Return nothing
  return(invisible(NULL))
  
}



prepNewDirectory <- function (dirPath) {
  
  # Generate a new folder in the PRMS "output" folder
  # It will contain data from this precipitation comparison
  
  
  # By default, the folder name will be "Streamflow_QAQC"
  newDir <- paste0(dirPath, "/PRMS/output/Streamflow_QAQC")
  
  
  # If the directory already exists, adjust the name to have a number at the end
  while (dir.exists(newDir)) {
    
    # If 'newDir' doesn't have any incrementing number in its name (e.g., "(#2)"),
    # add "_(#2)" to the directory name now
    if (!grepl("_\\(#[0-9]+\\)$", newDir)) {
      
      newDir <- paste0(newDir, "_(#2)")
      
      # (This situation happens only in the first iteration of this loop)
      
    } else {
      
      # If there's already an incrementing number in the folder name, 
      # extract it into 'dirNum'
      dirNum <- newDir |>
        str_extract("[0-9]+(?=\\)$)") |>
        as.numeric()
      
      
      # Increment the number
      dirNum <- dirNum + 1
      
      
      # Update the name in 'newDir' to have the new 'dirNum' instead
      newDir <- newDir |>
        str_replace("_\\(#[0-9]+\\)$",
                    paste0("_(#", dirNum, ")"))
      
    }
    
  } # End of loop to pick a name for the new streamflow folder
  
  
  # Create the new folder for the streamflow data plots
  dir.create(newDir)
  
  
  # Return the path 'newDir'
  return(newDir)
  
}



generatePlots <- function (dailyDF, newDir, timescale, prismDF, datDF) {
  
  # For the input timescale, produce plots and a table
  
  # Save the plots to 'newDir' and return the table as a tibble
  
  # These actions will be performed for both daily and monthly scales
  
  # Plots will contain precipitation data from either PRISM or the PRMS DAT file
  
  
  # Based on the value in 'timescale', apply a different filter to 'dailyDF'
  if (timescale == "1_yr") {
    
    # Keep only data from the past year
    cutoff <- max(dailyDF$DATE) - years(1)
    
    
    dailyDF <- dailyDF |>
      filter(DATE > cutoff)
    
    # This filter is for the last five years
  } else if (timescale == "5_yr") {
    
    cutoff <- max(dailyDF$DATE) - years(5)
    
    
    dailyDF <- dailyDF |>
      filter(DATE > cutoff)
    
    # This filter applies to the last ten years
  } else if (timescale == "10_yr") {
    
    cutoff <- max(dailyDF$DATE) - years(10)
    
    
    dailyDF <- dailyDF |>
      filter(DATE > cutoff)
    
  }
  
  
  # Next, prepare a monthly version of 'dailyDF'
  # Create a "YEAR_MONTH" column and group by that to summarize data
  # (Adding all "AF/day" values in a month will get "AF/month")
  monthlyDF <- dailyDF |>
    mutate(YEAR_MONTH = paste0(year(DATE), "-", month(DATE)) |>
             as_date(format = "%Y-%m")) |>
    group_by(YEAR_MONTH) |>
    summarize(FLOW = sum(FLOW), .groups = "drop")
  
  
  # After that, move on to the charts
  
  
  # Make streamflow plots using PRISM data for precipitation
  dailyDF |>
    generateStreamflowPlot(paste0(newDir, "/Daily_Streamflow_PRISM_Precip_", 
                                  timescale, ".png"),
                           yCol = "FLOW", isDaily = TRUE, 
                           precipDF = prismDF, 
                           precipType = "PRISM Avg")
  
  
  monthlyDF |>
    generateStreamflowPlot(paste0(newDir, "/Monthly_Streamflow_PRISM_Precip_", 
                                  timescale, ".png"),
                           yCol = "FLOW", isDaily = FALSE, 
                           precipDF = prismDF,
                           precipType = "PRISM Avg")
  
  
  dailyDF |>
    generateStreamflowPlot(paste0(newDir, "/Daily_Streamflow_DAT_Precip_", 
                                  timescale, ".png"),
                           yCol = "FLOW", isDaily = TRUE, 
                           precipDF = datDF,
                           precipType = "DAT Avg")
  
  
  monthlyDF |>
    generateStreamflowPlot(paste0(newDir, "/Monthly_Streamflow_DAT_Precip_", 
                                  timescale, ".png"),
                           yCol = "FLOW", isDaily = FALSE, 
                           precipDF = datDF,
                           precipType = "DAT Avg")
  
  
  # After that, use 'datDF' for precipitation data
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

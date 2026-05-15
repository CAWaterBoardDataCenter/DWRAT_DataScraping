# Compare the precipitation stored in the PRMS model output file 
# ("rr_budget.out2") to another source of basin-averaged precipitation data
# (the historic PRISM precipitation data for PRMS's model domain)


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
  cat("Starting 'HLP_009_Compare_PRMS_Output_to_PRISM_Averages.R'!\n")
  
  
  # Import the start and end date
  source("Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
  # Confirm that a proper directory exists for model input and output files
  # The PRMS model outputs are stored there
  cat("\n[1/3]\tChecking directories and files...\n")
  
  
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
  # For all of the 800m grid cells that fall completely within the PRMS model
  # domain, their precipitation data was averaged and compiled into a CSV file
  pastPrecipPath <- getFromControl_RR("PRISM_PRMS_HISTORIC_PRECIP_FOLDER") |>
    getLatestFile(paste0("^RR_Workflow_PRISM_PRMS_Avg_Historic_Precip_",
                         "CY1981_to_WY[0-9]{4}\\.csv$"),
                  "PRMS Historic Precip File")
  
  
  # Read in the latest historic precipitation file for PRMS
  pastPrecip <- pastPrecipPath |>
    getFile()
  
  
  # Then, validate this file
  pastPrecip |>
    validateHistoricPrecipFile(pastPrecipPath,
                               getModeledWY(endDate)[1])
  
  
  cat("\tDone!\n\n")
  
  
  # The next step is to compare the precipitation data in the two datasets
  cat("[2/3]\tComparing precipitation data...\n")
  
  
  # Generate plots and a table of statistical metrics for this data
  compareModelResults(dirPath, outDF, pastPrecip)
  
  
  # Output a completion message
  cat(col_green("\n'HLP_009_Compare_PRMS_Output_to_PRISM_Averages.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



compareModelResults <- function (dirPath, outDF, pastPrecip) {
  
  # Compare the precipitation data from PRMS and PRISM 
  
  # On both daily and monthly timescales, perform comparisons:
  #   (*) 1-year comparisons
  #   (*) 5-year comparisons
  #   (*) 10-year comparisons
  #   (*) Full dataset range comparisons
  
  # Produces plots and calculate statistical metrics too
  # (Nash-Sutcliffe efficiency, P-Bias, etc.)
  
  
  # First, combine 'outDF' and 'pastPrecip' to have both 
  # modeled and PRISM precipitation values in the same units over the same dates
  dailyDF <- combineDatasets(outDF, pastPrecip)
  
  # 'dailyDF' now has units of inches for both datasets
  
  
  # Then, create a new folder in the PRMS "output" directory 
  # This will hold the data output by this function
  newDir <- prepNewDirectory(dirPath)
  
  
  # Once 'newDir' has been established, 
  # create plots and summary statistics for different timescales
  
  
  # First generate plots and a table for the full datasets
  statDF <- generatePlotsAndTable(dailyDF, newDir, "All")
  
  
  # If the dataset contains at least one year of data, 
  # generate a one-year version too
  if (nrow(dailyDF) > 365) {
    
    statDF <- bind_rows(statDF,
                        generatePlotsAndTable(dailyDF, newDir, "1_yr"))
    
  }
  
  
  # If the dataset contains at least five years of data, 
  # generate a five-year version too
  if (nrow(dailyDF) > 365 * 5) {
    
    statDF <- bind_rows(statDF,
                        generatePlotsAndTable(dailyDF, newDir, "5_yr"))
    
  }
  
  
  # If the dataset contains at least ten years of data, 
  # generate a ten-year version too
  if (nrow(dailyDF) > 365 * 10) {
    
    statDF <- bind_rows(statDF,
                        generatePlotsAndTable(dailyDF, newDir, "10_yr"))
    
  }
  
  
  # Write 'statDF' to 'newDir'
  statDF |>
    writeOutput(paste0(newDir, "/Stat_Metrics.csv"))
  
  
  # Save 'outDF' and 'pastPrecip' to 'newDir' as well
  outDF |>
    writeOutput(paste0(newDir, "/PRMS_Reformatted_Out2.csv"),
                quietly = TRUE)
  
  pastPrecip |>
    writeOutput(paste0(newDir, "/PRISM_Precip_PRMS_Model_Domain.csv"),
                quietly = TRUE)
  
  
  # Finally, make a decision based on the values in 'statDF'
  
  # If something is extremely problematic, do NOT proceed with the workflow
  
  
  # Checking the entire data range, 
  # if the monthly precipitation R^2 value is below 0.5, 
  # stop the script and flag it as an error
  if (statDF$MONTHLY_RESULT[grepl("R Sq", statDF$METRIC) & 
                            statDF$TIMESCALE == "All"] < 0.50) {
    
    paste0("Unexpectedly Low R^2 Result for Monthly Precipitation\n\n",
           "In a comparison between the PRMS output and PRISM data, the ",
           "precipitation values appear to be excessively different. ",
           "On a monthly timescale, the calculated R Squared value is ",
           statDF$MONTHLY_RESULT[grepl("R Sq", statDF$METRIC) & 
                                   statDF$TIMESCALE == "All"] |> 
             round(digits = 3), ". Please investigate this issue.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



combineDatasets <- function (outDF, pastPrecip) {
  
  # 'outDF' contains precipitation for the model domain from PRMS (in inches)
  
  # Meanwhile, 'pastPrecip' contains precipitation for the model domain from
  # PRISM (in mm)
  
  # Modify the two datasets and combine them into one tibble
  # It will have a "DATE" column and precipitation values from both datasets
  # (in units of inches)
  
  
  # Calculate a "DATE" value for 'outDF'
  # Extract "DATE" and "ppt (in)"
  # Then, rename the precipitation column
  outDF <- outDF |>
    mutate(DATE = paste0(Year, "-", mo, "-", day) |>
             as.Date(format = "%Y-%m-%d")) |>
    select(DATE, `ppt (in)`) |>
    rename(PRMS_PRECIP = `ppt (in)`)
  
  
  # For 'pastPrecip', calculate precipitation in inches instead of millimeters
  # mm * 1/25.4 in/mm = in
  pastPrecip <- pastPrecip |>
    mutate(PRISM_PRECIP = `ppt (mm)` / 25.4) |>
    rename(DATE = Date) |>
    select(DATE, PRISM_PRECIP)
  
  
  # Filter 'outDF' and 'pastPrecip' to have the same dates
  pastPrecip <- pastPrecip |>
    filter(DATE %in% outDF$DATE)
  
  
  outDF <- outDF |>
    filter(DATE %in% pastPrecip$DATE)
  
  
  # Join the two datasets together using "DATE"
  dailyDF <- outDF |>
    left_join(pastPrecip, by = "DATE")
  
  
  # There should be no missing values in 'dailyDF'
  if (anyNA(dailyDF)) {
    
    paste0("Missing Values in Daily Precipitation Averages\n\n",
           "This script combined precipitation values for the PRMS model ",
           "domain using a file with PRISM grid cell data as well as the ",
           "output from a PRMS model run. However, one or more missing value ",
           "was detected in the result. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return 'dailyDF'
  return(dailyDF)
  
}



prepNewDirectory <- function (dirPath) {
  
  # Generate a new folder in the PRMS "output" folder
  # It will contain data from this precipitation comparison
  
  
  # By default, the folder name will be "Precip_Comparison"
  newDir <- paste0(dirPath, "/PRMS/output/Precip_Comparison")
  
  
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
    
  } # End of loop to pick a name for the new precipitation folder
  
  
  # Create the new folder for the precipitation data comparisons
  dir.create(newDir)
  
  
  # Return the path 'newDir'
  return(newDir)
  
}



generatePlotsAndTable <- function (dailyDF, newDir, timescale) {
  
  # For the input timescale, produce plots and a table
  
  # Save the plots to 'newDir' and return the table as a tibble
  
  # These actions will be performed for both daily and monthly scales
  
  
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
  
  
  # Next, create a monthly version of 'dailyDF' too
  # Rely on "YEAR_MONTH" to help group data
  
  # With data in inches per day, summing the data by month will 
  # result in units of inches per month
  monthlyDF <- dailyDF |>
    mutate(YEAR_MONTH = paste0(year(DATE), "-", month(DATE))) |>
    group_by(YEAR_MONTH) |>
    summarize(PRMS_PRECIP = sum(PRMS_PRECIP),
              PRISM_PRECIP = sum(PRISM_PRECIP)) |>
    mutate(YEAR_MONTH = as_date(YEAR_MONTH, format = "%Y-%m"))
  
  
  # After that, move on to the charts and statistics 
  
  # Start by generating plots
  # Use a separate function for that
  dailyDF |>
    generateComparisonPlot(paste0(newDir, "/Daily_Comparison_", 
                                  timescale, ".png"),
                           isDaily = TRUE)
  
  
  monthlyDF |>
    generateComparisonPlot(paste0(newDir, "/Monthly_Comparison_", 
                                  timescale, ".png"),
                           isDaily = FALSE)
  
  
  # After that, create a tibble that contains different statistical metrics
  statDF <- calculateStats(timescale, dailyDF, monthlyDF)
  
  
  # Return 'statDF'
  return(statDF)
  
}



generateComparisonPlot <- function (precipDF, writePath, isDaily = TRUE,
                                    volUnit = "in") {
  
  # Generate a plot for 'precipDF' 
  # It can contain either daily or monthly precipitation data
  
  
  # If daily precipitation will be plotted, the x-axis will be the "DATE" column
  # Otherwise, for monthly streamflow, it is the "YEAR_MONTH" column
  xCol <- if_else(isDaily, "DATE", "YEAR_MONTH")
  
  
  # Make sure this column exists in 'precipDF' too
  if (!(xCol %in% names(precipDF))) {
    
    paste0("Precipitation Dataset Missing Expected Column\n\n",
           "Because ", if_else(isDaily, "daily", "monthly"), " precipitation ",
           "will be plotted, this function expected the input data frame ",
           "to contain the column \"", xCol, "\". However, it was not found. ",
           "Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The selected 'xCol' column in 'precipDF' should be a "Date" type variable
  if (is.null(class(precipDF[[xCol]])) || class(precipDF[[xCol]]) != "Date") {
    
    paste0("Precipitation Dataset Column Type Issue\n\n",
           "To plot ", if_else(isDaily, "daily", "monthly"), " precipitation, ",
           "this function uses the column \"", xCol, "\". However, it is not ",
           "a \"Date\" type variable. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Get the limits for the y-axis (streamflow)
  yBounds <- c(precipDF$PRISM_PRECIP, precipDF$PRMS_PRECIP) |>
    range()
  
  
  # Prepare the label for the y-axis too
  yLabel <- paste0(if_else(isDaily, "Daily ", "Monthly "),
                   "Precipitation (", volUnit, "/",
                   if_else(isDaily, "Day", "Month"), ")")
  
  
  # Prepare the chart next
  precipPlot <- precipDF |>
    ggplot() +
    geom_line(mapping = aes(x = get(xCol), y = PRISM_PRECIP, color = "PRISM Grid"), 
              lwd = 0.8) +
    geom_line(mapping = aes(x = get(xCol), y = PRMS_PRECIP, color = "PRMS Model"),
              lwd = 0.8, linetype = 2, alpha = 0.9) + 
    xlab("Date") + ylab(yLabel) +
    guides(color = guide_legend(title = "Data Source")) +
    scale_color_manual(values = c("PRISM Grid" = "blue", "PRMS Model" = "red")) + 
    scale_x_date(date_labels = if_else(nrow(precipDF) < 365 * 5, "%Y-%m", "%Y")) + 
    coord_cartesian(ylim = yBounds) +
    theme_gray(base_size = 20)
  
  # The x-axis labels use either "Year-Month" or "Year" depending on the size
  # of 'precipDF'
  
  
  # Next, save 'precipPlot' to a file
  
  # The size of the chart should partially depend on the number of records
  
  
  # If the dates in 'precipDF' cover a period of more than 5,000 days, 
  # a larger chart is needed
  if (difftime(max(precipDF[[xCol]]), min(precipDF[[xCol]]), 
               units = "days") > 5000) {
    
    widthFactor <- 10.5
    heightFactor <- 8.25
    
    # Otherwise, a smaller dataset can use a smaller chart area
  } else {
    
    widthFactor <- 8
    heightFactor <- 6
    
  }
  
  
  # Save 'precipPlot' to 'writePath'
  ggsave(writePath, precipPlot, units = "px", dpi = 600,
         width = 1080 * widthFactor, height = 720 * heightFactor)
  
  
  # If the file was written successfully, output a message
  if (file.exists(writePath)) {
    
    cat("\n\n")
    
    paste0("Saved plot to \"", writePath, "\" successfully!") |>
      errWrap() |> col_blue() |> cat()
    
    cat("\n\n")
    
  } else {
    
    paste0("Could Not Save Chart\n\n",
           "The script failed to save a plot to \"", writePath, "\" for an ",
           "unknown reason. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}



calculateStats <- function (timescale, dailyDF, monthlyDF) {
  
  # Create a tibble with summary statistics
  
  # Calculate NSE and other metrics
  
  
  statDF <- tibble("TIMESCALE" = timescale, 
         "METRIC" = c("Nash-Sutcliffe Efficiency",
                      "P-Bias",
                      paste0("Root Mean Square Error to ",
                             "Standard Deviation Ratio"),
                      "Modified Kling-Gupta Efficiency",
                      "R Squared"),
         "DAILY_RESULT" = NA_real_,
         "DAILY_NOTES" = "--",
         "MONTHLY_RESULT" = NA_real_,
         "MONTHLY_NOTES" = "--")
  
  
  statDF <- statDF |>
    mutate(DAILY_RESULT = 
             case_when(
               grepl("^Nash", METRIC) ~ calcNSE(dailyDF$PRISM_PRECIP, dailyDF$PRMS_PRECIP),
               grepl("Bias$", METRIC) ~ calcPBias(dailyDF$PRISM_PRECIP, dailyDF$PRMS_PRECIP),
               grepl("^Root", METRIC) ~ calcRSR(dailyDF$PRISM_PRECIP, dailyDF$PRMS_PRECIP),
               grepl("^Modif", METRIC) ~ calcMKGE(dailyDF$PRISM_PRECIP, dailyDF$PRMS_PRECIP),
               grepl("^R Sq", METRIC) ~ calcRSqrd(dailyDF$PRISM_PRECIP, dailyDF$PRMS_PRECIP)
             )) |>
    mutate(MONTHLY_RESULT = 
             case_when(
               grepl("^Nash", METRIC) ~ calcNSE(monthlyDF$PRISM_PRECIP, monthlyDF$PRMS_PRECIP),
               grepl("Bias$", METRIC) ~ calcPBias(monthlyDF$PRISM_PRECIP, monthlyDF$PRMS_PRECIP),
               grepl("^Root", METRIC) ~ calcRSR(monthlyDF$PRISM_PRECIP, monthlyDF$PRMS_PRECIP),
               grepl("^Modif", METRIC) ~ calcMKGE(monthlyDF$PRISM_PRECIP, monthlyDF$PRMS_PRECIP),
               grepl("^R Sq", METRIC) ~ calcRSqrd(monthlyDF$PRISM_PRECIP, monthlyDF$PRMS_PRECIP)
             ))
  
  
  # For P-Bias, add to the "NOTES" columns whether the result is an
  # overprediction or underprediction (this interpretation varies depending 
  # on the exact formula used)
  statDF$DAILY_NOTES[statDF$METRIC == "P-Bias"] <- 
    calcPBias(dailyDF$PRISM_PRECIP, dailyDF$PRMS_PRECIP) |> 
    attributes() |> pluck(1)
  
  
  statDF$MONTHLY_NOTES[statDF$METRIC == "P-Bias"] <- 
    calcPBias(monthlyDF$PRISM_PRECIP, monthlyDF$PRMS_PRECIP) |> 
    attributes() |> pluck(1)
  
  
  # Return 'statDF'
  return(statDF)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

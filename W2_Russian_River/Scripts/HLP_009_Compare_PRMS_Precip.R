# Compare the precipitation stored in the PRMS model output file 
# ("rr_budget.out2") to another source of basin-averaged precipitation data
# (the historic PRISM precipitation data for PRMS's model domain)

# As a secondary check, try to use the average of the precipitation values in
# the input DAT file for PRMS

#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source(Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'HLP_009_Compare_PRMS_Precip.R'!\n")
  
  
  # Import the start and end date
  source("W2_Russian_River/Scripts/HLP_002_Validate_and_Import_Data_Scraping_Bounds.R")
  
  
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
  # This data may be split across multiple files
  
  # The historic dataset contains values from 1981 to a recent WY
  # The domain data downloaded in this workflow run covers much of the current WY
  # There may be a gap between them too, meaning that more data could be required
  
  # This procedure was completed in "HLP_011_Compare_SRP_Output_to_USGS_Gage.R"
  # Its functions can be reused here for PRMS
  c("gatherPrecipPRISM", "validateAndSummarizePRISM") |>
    map(~ functionStealer("W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", .))
  
  
  # Gather precipitation data
  pastPrecip <- gatherPrecipPRISM(dirPath, endDate, "PRMS")
  
  
  # Get data from the DAT file as well
  
  # Another function in "HLP_011_Compare_SRP_Output_to_USGS_Gage.R" 
  # can help with that
  functionStealer("W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", 
                  "gatherPrecipDAT")
  
  
  datDF <- gatherPrecipDAT(dirPath, startDate, endDate, model = "PRMS")
  
  
  # For consistency, have both 'pastPrecip' and 'datDF' use "PRECIP" as the
  # name for their precipitation columns and "DATE" for their date columns
  
  # Also, their units should be inches, not millimeters
  # (1 in = 25.4 mm)
  pastPrecip <- pastPrecip |>
    mutate(PRECIP = `ppt (mm)` / 25.4) |>
    rename(DATE = Date)
  
  
  datDF <- datDF |>
    mutate(PRECIP = PRECIP / 25.4) |>
    rename(DATE = Date)
  
  
  cat("\tDone!\n\n")
  
  
  # The next step is to compare the precipitation data in the two datasets
  cat("[2/2]\tComparing precipitation data...\n")
  
  
  # Create a new folder in the PRMS "output" directory 
  # This will hold the data output from this analysis
  # (The required function appears in another script)
  functionStealer("W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", 
                  "prepNewDirectory")
  
  
  # By default, the folder name will be "Precip_Comparison"
  newDir <- prepNewDirectory(dirPath,
                             paste0(dirPath, "/PRMS/output/Precip_Comparison"))
  
  
  # Generate plots and a table of statistical metrics for this data
  compareModelResults(dirPath, outDF, newDir, pastPrecip, "PRISM")
  
  
  # Do the same with DAT precipitation data instead 
  compareModelResults(dirPath, outDF, newDir, datDF, "DAT")
  
  
  # As a final step, archive 'outDF' and 'pastPrecip'
  outDF |>
    writeOutput(paste0(newDir, "/PRMS_Reformatted_Out2.csv"),
                quietly = TRUE)
  
  pastPrecip |>
    writeOutput(paste0(newDir, "/PRISM_Precip_PRMS_Model_Domain.csv"),
                quietly = TRUE)
  
  
  # Leave 'datDF' unarchived since it is easy to derive from the DAT file
  # that's already in the archive folder
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'HLP_009_Compare_PRMS_Precip.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



compareModelResults <- function (dirPath, outDF, newDir, precipDF, 
                                 precipSource = "PRISM") {
  
  # Compare the precipitation data from PRMS and PRISM 
  # (or from PRMS and the input DAT file)
  
  # On both daily and monthly timescales, perform comparisons:
  #   (*) 1-year comparisons
  #   (*) 5-year comparisons
  #   (*) 10-year comparisons
  #   (*) Full dataset range comparisons
  
  # Produces plots and calculate statistical metrics too
  # (Nash-Sutcliffe efficiency, P-Bias, etc.)
  
  
  # First, combine 'outDF' and 'precipDF' to have both 
  # modeled and PRISM/DAT precipitation values 
  # in the same units over the same dates
  dailyDF <- combineDatasets(outDF, precipDF)
  
  # 'dailyDF' now has units of inches for both datasets
  
  
  # Create plots and summary statistics for different timescales
  
  
  # First generate plots and a table for the full datasets
  statDF <- generatePlotsAndTable(dailyDF, newDir, "All", precipSource)
  
  
  # If the dataset contains at least one year of data, 
  # generate a one-year version too
  if (nrow(dailyDF) > 365) {
    
    statDF <- bind_rows(statDF,
                        generatePlotsAndTable(dailyDF, newDir, 
                                              "1_yr", precipSource))
    
  }
  
  
  # If the dataset contains at least five years of data, 
  # generate a five-year version too
  if (nrow(dailyDF) > 365 * 5) {
    
    statDF <- bind_rows(statDF,
                        generatePlotsAndTable(dailyDF, newDir, 
                                              "5_yr", precipSource))
    
  }
  
  
  # If the dataset contains at least ten years of data, 
  # generate a ten-year version too
  if (nrow(dailyDF) > 365 * 10) {
    
    statDF <- bind_rows(statDF,
                        generatePlotsAndTable(dailyDF, newDir, 
                                              "10_yr", precipSource))
    
  }
  
  
  # Write 'statDF' to 'newDir'
  statDF |>
    writeOutput(paste0(newDir, "/Stat_Metrics_", precipSource, ".csv"))
  
  
  # Finally, make a decision based on the values in 'statDF'
  
  # If something is extremely problematic, do NOT proceed with the workflow
  
  
  # Checking the entire data range, 
  # if the monthly precipitation R^2 value is below 0.5, 
  # stop the script and flag it as an error
  if (statDF$MONTHLY_RESULT[grepl("R Sq", statDF$METRIC) & 
                            statDF$TIMESCALE == "All"] < 0.50) {
    
    paste0("Unexpectedly Low R^2 Result for Monthly Precipitation\n\n",
           "In a comparison between the PRMS output and ", precipSource,
           " data, the precipitation values appear to be excessively different. ",
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



combineDatasets <- function (outDF, precipDF) {
  
  # 'outDF' contains precipitation for the model domain from PRMS (in inches)
  
  # 'precipDF' contains precipitation estimates (in inches)
  #
  # It contains either precipitation for the model domain from PRISM, or
  # data from the gages in the watershed (in inches either way)
  
  # Modify the datasets and combine them into one tibble
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
  
  
  # Filter 'outDF' and 'precipDF' to have the same dates
  precipDF <- precipDF |>
    filter(DATE %in% outDF$DATE)
  
  
  outDF <- outDF |>
    filter(DATE %in% precipDF$DATE)
  
  
  # Join the two datasets together using "DATE"
  dailyDF <- outDF |>
    left_join(precipDF, by = "DATE")
  
  
  # There should be no missing values in 'dailyDF'
  if (anyNA(dailyDF)) {
    
    paste0("Missing Values in Daily Precipitation Averages\n\n",
           "This script combined precipitation values for the PRMS model ",
           "domain to QA/QC the output from a PRMS model run. However, one ",
           "or more missing values were detected in the result. Please ",
           "investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Return 'dailyDF'
  return(dailyDF)
  
}



generatePlotsAndTable <- function (dailyDF, newDir, timescale, precipType) {
  
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
              PRECIP = sum(PRECIP)) |>
    mutate(YEAR_MONTH = as_date(YEAR_MONTH, format = "%Y-%m"))
  
  
  # After that, move on to the charts and statistics 
  
  # Start by generating plots
  # Use a separate function for that
  dailyDF |>
    generateComparisonPlot(paste0(newDir, "/Daily_Comparison_", precipType, "_",
                                  timescale, ".png"),
                           precipType, isDaily = TRUE)
  
  
  monthlyDF |>
    generateComparisonPlot(paste0(newDir, "/Monthly_Comparison_", precipType, "_",
                                  timescale, ".png"),
                           precipType, isDaily = FALSE)
  
  
  # After that, create a tibble that contains different statistical metrics
  statDF <- calculateStats(timescale, dailyDF, monthlyDF)
  
  
  # Return 'statDF'
  return(statDF)
  
}



generateComparisonPlot <- function (precipDF, writePath, precipType, 
                                    isDaily = TRUE, volUnit = "in") {
  
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
  yBounds <- c(precipDF$PRECIP, precipDF$PRMS_PRECIP) |>
    range()
  
  
  # Prepare the label for the y-axis too
  yLabel <- paste0(if_else(isDaily, "Daily ", "Monthly "),
                   "Precipitation (", volUnit, "/",
                   if_else(isDaily, "Day", "Month"), ")")
  
  
  # Prepare the chart next
  precipPlot <- precipDF |>
    ggplot() +
    geom_line(mapping = aes(x = get(xCol), y = PRECIP, 
                            color = paste0("Avg ", precipType)), 
              lwd = 0.8) +
    geom_line(mapping = aes(x = get(xCol), y = PRMS_PRECIP, color = "PRMS Model"),
              lwd = 0.8, linetype = 2, alpha = 0.9) + 
    xlab("Date") + ylab(yLabel) +
    guides(color = guide_legend(title = "Data Source")) +
    scale_color_manual(values = c("blue", "red") |> 
                         set_names(c(paste0("Avg ", precipType), "PRMS Model"))) + 
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
               grepl("^Nash", METRIC) ~ calcNSE(dailyDF$PRECIP, dailyDF$PRMS_PRECIP),
               grepl("Bias$", METRIC) ~ calcPBias(dailyDF$PRECIP, dailyDF$PRMS_PRECIP),
               grepl("^Root", METRIC) ~ calcRSR(dailyDF$PRECIP, dailyDF$PRMS_PRECIP),
               grepl("^Modif", METRIC) ~ calcMKGE(dailyDF$PRECIP, dailyDF$PRMS_PRECIP),
               grepl("^R Sq", METRIC) ~ calcRSqrd(dailyDF$PRECIP, dailyDF$PRMS_PRECIP)
             )) |>
    mutate(MONTHLY_RESULT = 
             case_when(
               grepl("^Nash", METRIC) ~ calcNSE(monthlyDF$PRECIP, monthlyDF$PRMS_PRECIP),
               grepl("Bias$", METRIC) ~ calcPBias(monthlyDF$PRECIP, monthlyDF$PRMS_PRECIP),
               grepl("^Root", METRIC) ~ calcRSR(monthlyDF$PRECIP, monthlyDF$PRMS_PRECIP),
               grepl("^Modif", METRIC) ~ calcMKGE(monthlyDF$PRECIP, monthlyDF$PRMS_PRECIP),
               grepl("^R Sq", METRIC) ~ calcRSqrd(monthlyDF$PRECIP, monthlyDF$PRMS_PRECIP)
             ))
  
  
  # For P-Bias, add to the "NOTES" columns whether the result is an
  # overprediction or underprediction (this interpretation varies depending 
  # on the exact formula used)
  statDF$DAILY_NOTES[statDF$METRIC == "P-Bias"] <- 
    calcPBias(dailyDF$PRECIP, dailyDF$PRMS_PRECIP) |> 
    attributes() |> pluck(1)
  
  
  statDF$MONTHLY_NOTES[statDF$METRIC == "P-Bias"] <- 
    calcPBias(monthlyDF$PRECIP, monthlyDF$PRMS_PRECIP) |> 
    attributes() |> pluck(1)
  
  
  # Return 'statDF'
  return(statDF)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

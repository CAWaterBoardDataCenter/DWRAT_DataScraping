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
  # (1 mm = 25.4 in)
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
    as_tibble() |>
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
    makeStreamflowPlot(paste0(newDir, "/Daily_Streamflow_PRISM_Precip_", 
                              timescale, ".png"),
                       isDaily = TRUE, prismDF,
                       precipType = "PRISM Avg")
  
  
  monthlyDF |>
    makeStreamflowPlot(paste0(newDir, "/Monthly_Streamflow_PRISM_Precip_", 
                              timescale, ".png"),
                       isDaily = FALSE, prismDF,
                       precipType = "PRISM Avg")
  
  
  dailyDF |>
    makeStreamflowPlot(paste0(newDir, "/Daily_Streamflow_DAT_Precip_", 
                              timescale, ".png"),
                       isDaily = TRUE, datDF,
                       precipType = "DAT Avg")
  
  
  monthlyDF |>
    makeStreamflowPlot(paste0(newDir, "/Monthly_Streamflow_DAT_Precip_", 
                              timescale, ".png"),
                       isDaily = FALSE, datDF,
                       precipType = "DAT Avg")
  
  
  # After that, use 'datDF' for precipitation data
  
  
  # Return nothing
  return(invisible(NULL))
  
}



makeStreamflowPlot <- function (streamDF, writePath, precipDF,
                                    isDaily = TRUE, volUnit = "AF", 
                                    precipType = "PRISM Avg") {
  
  # Generate a plot for 'streamDF' 
  # It can contain either daily or monthly streamflow data
  
  # 'precipDF' contains precipitation data for the same period, and it
  # will be included as bars in the graph
  
  
  # Adjust 'precipDF' to the bounds of 'streamDF'
  
  # For daily streamflow, filter 'precipDF' to the same range as 'streamDF'
  if (isDaily) {
    
    # Rename "Date" to "DATE" in order to match 'streamDF'
    precipDF <- precipDF |>
      filter(Date >= min(streamDF$DATE) & Date <= max(streamDF$DATE)) |>
      rename(DATE = Date)
    
    
    # Then, filter 'streamDF' to match the date range in 'precipDF'
    streamDF <- streamDF |>
      filter(DATE >= min(precipDF$DATE) & DATE <= max(precipDF$DATE))
    
    
    # Otherwise, for monthly streamflow, 
    # the procedure is a little more complicated
  } else {
    
    # Convert 'precipDF' into a monthly timescale using a "YEAR_MONTH" column
    precipDF <- precipDF |>
      mutate(YEAR_MONTH = paste0(year(Date), "-", month(Date)) |>
               as_date(format = "%Y-%m")) |>
      filter(YEAR_MONTH >= min(streamDF$YEAR_MONTH) & 
               YEAR_MONTH <= max(streamDF$YEAR_MONTH)) |>
      group_by(YEAR_MONTH) |>
      summarize(PRECIP = sum(PRECIP), .groups = "drop")
    
    
    # Then, filter 'streamDF' to match the date range in 'precipDF'
    streamDF <- streamDF |>
      filter(YEAR_MONTH >= min(precipDF$YEAR_MONTH) & 
               YEAR_MONTH <= max(precipDF$YEAR_MONTH))
    
  }
  
  
  # If daily streamflow will be plotted, the x-axis will be the "DATE" column
  # Otherwise, for monthly streamflow, it is the "YEAR_MONTH" column
  xCol <- if_else(isDaily, "DATE", "YEAR_MONTH")
  
  
  # Make sure this column exists in 'streamDF' too
  if (!(xCol %in% names(streamDF))) {
    
    paste0("Streamflow Dataset Missing Expected Column\n\n",
           "Because ", if_else(isDaily, "daily", "monthly"), " streamflow ",
           "will be plotted, this function expected the input data frame ",
           "to contain the column \"", xCol, "\". However, it was not found. ",
           "Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # The selected 'xCol' column in 'streamDF' should be a "Date" type variable
  if (is.null(class(streamDF[[xCol]])) || class(streamDF[[xCol]]) != "Date") {
    
    paste0("Streamflow Dataset Column Type Issue\n\n",
           "To plot ", if_else(isDaily, "daily", "monthly"), " streamflow, ",
           "this function uses the column \"", xCol, "\". However, it is not ",
           "a \"Date\" type variable. Please investigate.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Get the limits for the y-axis (streamflow)
  yBounds <- c(streamDF$FLOW) |>
    range()
  
  
  # Prepare the label for the y-axis too
  yLabel <- paste0(if_else(isDaily, "Daily ", "Monthly "),
                   "Model Streamflow (", volUnit, "/",
                   if_else(isDaily, "Day", "Month"), ")")
  
  
  # If 'precipDF' was provided, a label will be needed for a secondary y-axis too
  yLabel2 <- paste0(if_else(isDaily, "Daily ", "Monthly "),
                    "Precipitation (in/",
                    if_else(isDaily, "Day", "Month"), ")")
  
  
  # The next step is to design the plots
  
  # For 'precipDF', to have vertical bars coming down from the top, 
  # both y-axes must be reversed
  
  # We can't just do this to one axis, and if we try to flip the precipitation
  # data from a regular set of axes, the columns will not draw correctly
  
  # So we have to reverse all the y-axes first, and it will be easier to 
  # reverse the streamflow lines back to a normal appearance
  
  
  # A requirement of this approach is that the primary y-axis breaks 
  # will have to be set manually
  
  # We want nice roundish numbers as the axis breaks
  # However, `ggplot` will default to nice breaks for the reversed primary axis
  
  # When we transform the primary y-axis back into a normal ordering (i.e.,
  # with zero at the bottom), the corresponding values at the axis breaks 
  # will not be nice numbers
  
  
  # Borrow a function that can help get us nice numbers 
  # on the post-transformation axis
  # (There is another function needed later too that will be imported now)
  c("getNiceAxisBreaks", "setPrecipColumnWidths") |>
    map(~ functionStealer("Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", .))

  
  # Get nice breaks in the axis for the primary y-axis
  breakVals <- getNiceAxisBreaks(yBounds[2], yBounds[1])
  
  
  # After that, get the extreme values in 'precipDF'
  precipRange <- range(precipDF$PRECIP)
  
  
  # Start by initializing 'streamPlot' with basic customizations 
  streamPlot <- ggplot(streamDF) +
    
    xlab("Date") + ylab(yLabel) +
    # Axis labels
    
    scale_x_date(date_labels = if_else(nrow(streamDF) < 365 * 5, "%Y-%m", "%Y")) +
    # Set the appearance of the x-axis date labels (see more details below)
    
    coord_cartesian(ylim = yBounds) +
    # Limit the chart's y-axis to the values in 'yBounds'
    
    theme_gray(base_size = 20)
    # Set the default font size to "20" units instead of "11"
  
  
  # The x-axis labels use either "Year-Month" or "Year" depending on the size
  # of 'streamDF' 
  
  # For daily data, plots with at least 5 years worth of data use just
  # years in their labels; smaller plots use "Year-Month"
  
  # For monthly data, essentially all cases use "Year-Month" ('streamDF' 
  # would need at least 365 * 5 = 1825 months of data to switch its labels)
  
  
  # The next set of edits are more complicated due to 'precipDF'
  streamPlot <- streamPlot + 
    
    geom_line(mapping = aes(x = get(xCol), y = yBounds[2] + yBounds[1] - FLOW), 
              lwd = 0.8, color = "blue") +
    # The flow data will be coming from the top down, and this transformation
    # to the "y" variable will correct it to appear as if it came from the 
    # bottom up instead
    
    geom_col(data = precipDF, 
             mapping = aes(x = get(xCol), 
                           y = PRECIP * diff(yBounds) / diff(precipRange), 
                           fill = precipType), 
             width = setPrecipColumnWidths(isDaily, nrow(precipDF)), 
             alpha = 0.35) +
    # Set precipitation values next--a transformation maps the precipitation
    # data to the same scale as the streamflow data (see more details below)
    # Its color is setup to appear in a legend, the width of each column is 
    # determined in a separate function, and the columns are set to be mostly
    # transparent
    
    guides(fill = guide_legend(title = "Precipitation")) + 
    scale_fill_manual(values = c("#0081FF") |> set_names(precipType)) + 
    # Set the colors of the precipitation columns (and the name of their legend)
    
    scale_y_reverse(breaks = breakVals, 
                    labels = ~ yBounds[2] + yBounds[1] - .,
                    sec.axis = 
                      sec_axis(~ . * diff(precipRange) / diff(yBounds), 
                               name = yLabel2))
  # This is what actually flips the y-axis to come down from the top
  # 
  # The breaks are set using 'breakVals' (described earlier)
  # 
  # The labels have a transformation applied so that they reflect the
  # bottom-up streamflow data correctly (and their numbers are actually 
  # nice thanks to the efforts in creating 'breakVals')
  # 
  # The secondary y-axis for precipitation is also setup here
  # 
  # Its values *should* come from the top down, so the default axis values
  # will already be nice numbers
  # 
  # The only requirement is specifying the transformation correctly 
  # (since all secondary y-axes are purely decorative, and the data is 
  #  actually still plotted relative to the streamflow axis)
  # 
  # This is why a transformation was applied to the precipitation data in
  # the `geom_col` call
  # 
  # The data was rescaled to follow the reversed streamflow axis properly
  # 
  # The secondary axis has the opposite of this transformation so that 
  # the streamflow y-axis values can be rescaled in the secondary y-axis and 
  # properly reflect the original precipitation values
  # 
  
  
  # ...Who knew the plotting would get so complicated? (>.<)
  
  # To summarize, the streamflow and precipitation values are a lie
  # As are both y-axes' labels
  
  # The streamflow lines and precipitation columns get their values 
  # by assuming that y = 0 is at the top of the graph
  
  # This is still true
  
  # However, their values have been rescaled to create the illusion that: 
  # (1) the streamflow data is coming from the bottom
  # (2) the precipitation data is relative to the secondary axis
  
  
  # The formula applied to the streamflow data made it so that the values we 
  # want to show are indeed scaled correctly (and relative to the bottom of 
  # the graph--as if y = 0 was at the bottom of the plot!)
  
  # Meanwhile, the main y-axis labels are also reversing the y-axis reverse, 
  # with breaks in the graph set at "nice numbers" when considered from the 
  # bottom-up (i.e., y = 0 at the bottom of the plot)
  
  # These breaks are likely ugly if we consider their "true" top-down values
  
  # And the precipitation data is intended to be top-down, but it is plotted
  # against the streamflow data's y-axis, which has a different scaling
  
  # So the precipitation data is transformed (mapping its extremes to the 
  # extremes of the streamflow data)
  
  # Then, to support this illusion, the labels have the reverse of that 
  # transformation applied (scaling the streamflow y-axis values to the  
  # precipitation values' actual range)
  
  # In this case, since we are maintaining the top-down axis labeling, the
  # breaks set by `ggplot` end up being nice numbers for the precipitation
  # values
  
  
  # Next, save 'streamPlot' to a file
  
  # The size of the chart should partially depend on the number of records
  
  
  # If the dates in 'streamDF' cover a period of more than 5,000 days, 
  # a larger chart is needed
  if (difftime(max(streamDF[[xCol]]), min(streamDF[[xCol]]), 
               units = "days") > 5000) {
    
    widthFactor <- 10
    heightFactor <- 8
    
    # Otherwise, a smaller dataset can use a smaller chart area
  } else {
    
    widthFactor <- 8
    heightFactor <- 6
    
  }
  
  
  # Save 'streamPlot' to 'writePath'
  ggsave(writePath, streamPlot, units = "px", dpi = 600,
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



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

# Forecasting water availability is a crucial part of SDA's work

# We need some method to predict future precipitation and temperature data
# for the rest of the water year

# These values will affect the streamflow values output by our hydrology models
# as well as the allocations made by DWRAT


# Our current methodology is split into two approaches

# Depending on the amount of data available for the current water year, we apply
# one of these methods


# First, in the beginning of the water year, we use the 
# Standard Precipitation Index (SPI)
# This identifies the driest period on record for each month

# In a time series of precipitation data, we can extract the worst-case months
# and use these records as our forecast for the current water year


# Beginning in March of a water year, once we have accumulated data for 5 months 
# (October - February), we can predict conditions for the rest of the water year 
# with a greater level of accuracy

# The "Partial Precipitation" observed in a water year can be correlated to the 
# "Total WY Precipitation" using a linear regression model

# With this model, we can then identify a previous water year with a similar 
# total precipitation as the predicted total

# This "Similar Water Year" can be the source of precipitation and temperature
# data for the forecast in the rest of the current water year


# With each additional month of data added to the "Partial Precipitation" value,
# we can predict conditions for the rest of the water year with greater certainty

# For that reason, the Russian River workflow relies on three different linear
# regression models for forecasting:

# "October to February" Partial Precipitation (used in March)
# "October to March" Partial Precipitation (used in April)
# "October to April" Partial Precipitation (used from May onwards)


# Putting these two methods together, the forecasting procedure looks like this 
# in a given water year:

#  (1) October - February
#      SPI (Worst Case Months)
#
#  (2) March
#      Similar Water Year ("October to February" Model)
#
#  (3) April
#      Similar Water Year ("October to March" Model)
#
#  (4) May - September
#      Similar Water Year ("October to April" Model)


# For this analysis, in both cases, we need data on a watershed scale--daily 
# precipitation values that represent conditions over the entire watershed

# For that reason, we gather PRISM data from cells located entirely *within* the 
# model domain

# We then calculate the average precipitation across these cells, giving us a 
# time series (from January 1981 onwards) with average precipitation data for the
# entire model domain

# Note: Precipitation data from PRISM often differs from real gage data in terms of
#       magnitude, but since this comparison involves PRISM data vs PRISM data,
#       it is okay (i.e., the timing of precipitation in PRISM is not a problem)


# On 2026-03-30, a raster of PRISM 800m grid cells was obtained
# Grid cells located completely within the PRMS and SRP model domains were identified

# Then, on 2026-04-01, PRISM data was gathered between 1981-01-01 and 2026-03-10
# Using every complete water year in this dataset (i.e., WY1982 to WY2025), 
# the three linear regression models ("Oct-Feb", "Oct-Mar", and "Oct-Apr")
# were developed


# This script contains the procedures used to obtain the relevant PRISM grid cells
# for both PRMS and SRP

# In addition, the code that developed calibrated and validated regression models
# for PRMS and SRP are included here as well

# To run this script, the following inputs are required:

#  (1) PRISM Raster
#
#      Place this in the "Input" folder!
#
#  (2) Model Domain GIS Layer (PRMS)
#  (3) Model Domain GIS Layer (SRP)
#
#      Place them in the "Input" folder!
#
#  (4) WY1982-WY2025 PRISM Data (PRMS)
#  (5) WY1982-WY2025 PRISM Data (SRP)
#
#      Place them in the "Intermediate" folder!

# Please obtain this data from SDA staff


# The results from this script are integrated into every run of the automated 
# Russian River workflow (for forecasting), but this script itself was only 
# ran once in the initial configuration phase


# The procedure in this script can be applied for other watersheds and model
# domains--or with additional PRISM data--by modifying the inputs


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")
require(sf)
require(stars)
require(mapview)
require(writexl)


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####


mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_EX1_Similar_Water_Year_Models.R'!\n\n")
  
  
  cat("[1/2]\tIdentifying relevant PRISM grid cells...\n\n")
  
  
  # Read in all required inputs for the grid cell selection process
  
  
  # PRISM Raster (800m resolution)
  # (Technically, any recent 800m raster is fine to use)
  # (As long as the grid cells haven't changed shape or position, it's all the same)
  prismRaster <- "W2_Russian_River/Input/prism_ppt_us_30s_20260329.tif" |>
    read_stars()
  
  
  # Model Domain Layers
  prmsBound <- st_read("W2_Russian_River/Input/Russian River.gdb",
                       layer = "PRMS_Subbasins") |>
    st_transform("epsg:3488") |>
    st_union()
  
  
  srpBound <- st_read("W2_Russian_River/Input/Russian River.gdb",
                      layer = "Santa_Rosa_Plains_Subbasin") |>
    st_transform("epsg:3488") |>
    st_union()
  
  
  # Notify the user of the first 
  
  
  # Use a separate function to extract the relevant grid cells 
  # for each model domain
  prmsCells <- prmsBound |>
    getCellCentroids(prismRaster, includePartial = FALSE)
  
  
  srpCells <- srpBound |>
    getCellCentroids(prismRaster, includePartial = FALSE)
  
  
  # Write this coordinate data to a file
  prmsCells |>
    writeOutput("W2_Russian_River/Output/RR_Supply_PRISM_PRMS_Grid_Cells.csv")
  
  
  srpCells |>
    writeOutput("W2_Russian_River/Output/RR_Supply_PRISM_SRP_Grid_Cells.csv")
  
  
  cat("\tDone!\n\n")
  
  
  cat("[2/2]\tPerforming linear regression with PRISM data...\n\n")
  
  
  # At this step, PRISM data would be gathered at these locations
  # Using the CSV file as a reference, the PRISM scraping function would
  # be applied from 1981-01-01 to 2026-03-10, gathering data in millimeters
  # and WITHOUT grid cell interpolation
  
  # However, since this script is meant to recreate the original run, PRISM
  # data is NOT gathered by this script again because that data can change 
  # over time, and the values obtained now may be different from the past
  
  # That is why the PRISM data CSV files must be obtained from SDA staff for this
  
  # Still, the code that could download this information is included below
  # It is just commented out
  
  startDate <- "1981-01-01" |>
    as.Date(format = "%Y-%m-%d")
  
  endDate <- "2026-03-10" |>
    as.Date(format = "%Y-%m-%d")
  
  
  prmsPath <- paste0("W2_Russian_River/Intermediate/PRISM_PRMS_Domain_Data_", 
                     startDate, "_", endDate, ".csv")
  
  srpPath <- paste0("W2_Russian_River/Intermediate/PRISM_SRP_Domain_Data_", 
                    startDate, "_", endDate, ".csv")
  
  
  # c("scrapePRISM", "validateReqResults", "splitRequest", "combineRawOutputs") |>
  #   map(~ functionStealer("W2_Russian_River/Scripts/RRW_001_PRISM_HTTP_Scraper.R", .))
  # 
  # 
  # scrapePRISM(prmsCells, startDate, endDate, prmsPath,
  #             useHighRes = TRUE, interpCells = FALSE,
  #             getPrecip = TRUE, getTemp = FALSE, useMetric = TRUE)
  # 
  # 
  # scrapePRISM(srpCells, startDate, endDate, srpPath,
  #             useHighRes = TRUE, interpCells = FALSE,
  #             getPrecip = TRUE, getTemp = FALSE, useMetric = TRUE)
  
  
  if (!file.exists(prmsPath) || !file.exists(srpPath)) {
    
    stop("Raw PRISM CSV files for the model domains are required!")
    
  }
  
  
  # After that, use the two PRISM CSV files to produce linear regression models
  prmsPath |>
    modelPartialPrecip(analysisYear = 2026)
  
  srpPath |>
    modelPartialPrecip(analysisYear = 2026)
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("'RRW_EX1_Similar_Water_Year_Models.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



getCellCentroids <- function (boundary, prismRaster, includePartial = FALSE) {
  
  # Given a boundary polygon, get all PRISM grid cells that intersect with it
  
  # This function returns a data frame of latitude and longitude coordinates (WGS84)
  
  # These correspond to the centroids of the PRISM grid cells that intersect
  # with the input boundary polygon
  
  # If 'includePartial' is TRUE, grid cells with partial overlap can be included
  # Otherwise, only grid cells fully contained within the boundary will be returned
  
  
  # Apply a 10 km buffer to the input boundaries
  bufferBound <- boundary |>
    st_buffer(10 * 1000)
  
  
  # Clip 'prismRaster' to the buffered area
  prismClip <- prismRaster |>
    st_crop(bufferBound |> st_transform(st_crs(prismRaster)))
  
  
  # Convert 'prismClip' into an sf object
  prismSF <- st_as_sf(prismClip)
  
  
  # Find all grid cells that intersect the boundaries
  
  # If 'includePartial' is TRUE, grid cells that partially touch the boundary polygon
  # will be included
  
  # Otherwise, only cells that are FULLY CONTAINED within the boundaries will be used
  
  
  if (includePartial) {
    
    prismSF <- prismSF[st_intersects(prismSF, boundary |> st_transform(st_crs(prismSF))) |> 
                         lengths() > 0, ]
    
  } else {
    
    prismSF <- prismSF[st_within(prismSF, boundary |> st_transform(st_crs(prismSF))) |> 
                         lengths() > 0, ]
    
  }
  
  
  print(mapview(boundary, col.regions = "gray") + mapview(prismSF))
  
  
  # Get the centroids of each grid cell
  # Transform the data into WGS84
  # Then save these coordinates to a matrix
  centroidDF <- prismSF |>
    st_centroid() |>
    st_transform("WGS84") |>
    st_coordinates() |>
    data.frame() |>
    set_names(c("LONGITUDE", "LATITUDE")) |>
    mutate("STATION_ID" = row_number()) |>
    select(LATITUDE, LONGITUDE, STATION_ID)
  
  
  # Return 'centroidDF'
  return(centroidDF)
  
}



modelPartialPrecip <- function (prismPath, analysisYear = NULL) {
  
  # Given a path to unprocessed PRISM data, 
  # develop linear regression models 
  
  # Establish a calibrated and validated relationship between a water year's
  # partial precipitation values and their total water year precipitation values
  
  # For Partial Precipitation, consider three options:
  #   (*) October to February
  #   (*) October to March
  #   (*) October to April
  
  
  partialColumns <- c("OCT_TO_FEB", "OCT_TO_MAR", "OCT_TO_APR")
  
  
  # First, make sure 'analysisYear' is not NULL
  # If it is, use the current water year
  if (is.null(analysisYear)) {
    
    if (month(Sys.Date()) < 10) {
      
      analysisYear <- year(Sys.Date())
      
    } else {
      
      analysisYear <- year(Sys.Date()) + 1
      
    }
    
  }
  
  
  # Parse the PRISM file as a data frame
  prismDF <- getFile(prismPath)
  
  
  # Prepare a subset of 'prismDF' for analysis of the precipitation data
  precipDF <- preparePrecipDF(prismDF, analysisYear)
  
  
  # Create summary columns for the precipitation data
  # Calculate total water year precipitation and partial precipitation columns
  summaryDF <- summarizePrecip(precipDF, partialColumns)
  
  
  # Perform linear regression on the data
  # Output information on the fit of these models
  linAnalysis(summaryDF, analysisYear, partialColumns, prismPath)
  
  
  # Return nothing
  return(invisible(NULL))
  
}



preparePrecipDF <- function (prismDF, analysisYear) {
  
  # Adjust 'prismDF' to contain monthly precipitation values
  # Also check for potential issues and try to address them
  
  
  # Keep only date and precipitation columns
  # Add "YEAR", "MONTH", and "WATER_YEAR" columns as well
  prismDF <- prismDF |> 
    select(Date, `ppt (mm)`) |>
    mutate(MONTH = month(Date),
           YEAR = year(Date)) |>
    mutate(WATER_YEAR = if_else(MONTH < 10, YEAR, YEAR + 1))
  
  
  # Only use water years with a complete set of data
  countDF <- prismDF |>
    group_by(YEAR, MONTH) |>
    summarize(COUNT = n(), .groups = "drop") |>
    mutate(WATER_YEAR = if_else(MONTH < 10, YEAR, YEAR + 1))
  
  
  # Check that 12 months of data is present for every water year
  yearDF <- countDF |>
    group_by(WATER_YEAR) |>
    summarize(MONTHS = n()) |>
    filter(MONTHS < 12)
  
  
  # 'yearDF' may contain years with fewer than 12 months of data
  # Remove those records from 'prismDF'
  if (nrow(yearDF) > 0) {
    
    prismDF <- prismDF |>
      filter(!(WATER_YEAR %in% yearDF$WATER_YEAR))
    
  }
  
  
  # This contains the expected number of days for each month in 'countDF'
  refDF <- tibble("Date" = seq(from = min(prismDF$Date), 
                               to = max(prismDF$Date), 
                               by = "days")) |>
    mutate(MONTH = month(Date),
           YEAR = year(Date)) |>
    group_by(YEAR, MONTH) |>
    summarize(REF_COUNT = n(), .groups = "drop")
  
  
  # Identify any months in 'countDF' with missing data
  countDF <- countDF |>
    left_join(refDF,
              by = c("YEAR", "MONTH")) |>
    filter(COUNT != REF_COUNT)
  
  
  # Exclude those water years from 'prismDF'
  if (nrow(countDF) > 0) {
    
    prismDF <- prismDF |>
      filter(!(WATER_YEAR %in% countDF$WATER_YEAR))
    
  }
  
  
  # Check for any missing dates in 'prismDF' next
  missingEntries <- seq(from = min(prismDF$Date), 
                        to = max(prismDF$Date), 
                        by = "days")
  
  
  missingEntries <- prismDF$WATER_YEAR[!(prismDF$Date %in% missingEntries)]
  
  
  # Remove water years that are incomplete
  if (length(missingEntries) > 0) {
    
    prismDF <- prismDF |>
      filter(!(WATER_YEAR %in% missingEntries))
    
  }
  
  
  # Finally, remove 'analysisYear' if it appears in 'prismDF' 
  # (as well as any subsequent years)
  prismDF <- prismDF |>
    filter(WATER_YEAR < analysisYear)
  
  
  # Once these checks are complete, 
  # sum the precipitation values for each year-month pair
  prismDF <- prismDF |>
    group_by(WATER_YEAR, YEAR, MONTH) |>
    summarize(TOTAL_PRECIP_MM = sum(`ppt (mm)`), .groups = "drop")
  
  
  return(prismDF)
  
}



summarizePrecip <- function (precipDF, partialColumns = c("OCT_TO_FEB")) {
  
  # Create a summarized version of 'precipDF'
  
  # It will contain total precipitation for each water year as well as
  # total precipitation for subsets of that year
  
  # The subsets are defined in 'partialColumns'
  
  
  # First, create a summary table for each water year
  summaryDF <- precipDF |>
    group_by(WATER_YEAR) |>
    summarize(WY_TOTAL_PRECIP_MM = sum(TOTAL_PRECIP_MM), .groups = "drop")
  
  
  # Prepare the subset columns next
  
  # Create new data frames with columns for these subset sums
  # Join them to 'summaryDF'
  for (i in 1:length(partialColumns)) {
    
    newDF <- createPartialDF(precipDF, partialColumns[i])
    
    summaryDF <- summaryDF |>
      left_join(newDF, by = "WATER_YEAR", relationship = "one-to-one")
    
  }
  
  
  # Return 'summaryDF'
  return(summaryDF)
  
}



createPartialDF <- function (precipDF, colName) {
  
  # Create a column that sums a portion of the months in each water year
  
  # 'colName' should be in a format like "AAA_TO_AAA", with abbreviations 
  # of months appearing at the beginning and end of the name
  
  
  # Extract from 'colName' the range of months to summarize
  rangeStart <- which(toupper(month.abb) == str_extract(colName, "^[A-Z]+"))
  rangeEnd <- which(toupper(month.abb) == str_extract(colName, "[A-Z]+$"))
  
  
  # Define a vector that contains all months within this range
  # If 'rangeStart' is a later month than 'rangeEnd', the procedure will differ slightly
  if (rangeStart > rangeEnd) {
    
    # For a range like October to February, the relevant months are 
    # 10, 11, 12, 1, and 2
    rangeVec <- c(rangeStart:12, 1:rangeEnd)
    
  } else {
    
    rangeVec <- rangeStart:rangeEnd
    
  }
  
  
  # Define another variable like 'summaryDF' that sums precipitation data for
  # only the months in 'rangeVec'
  partialDF <- precipDF |>
    filter(MONTH %in% rangeVec) |>
    group_by(WATER_YEAR) |>
    summarize(!! paste0(colName, "_PARTIAL_PRECIP_MM") := 
                sum(TOTAL_PRECIP_MM), .groups = "drop")
  
  
  # Return 'partialDF'
  return(partialDF)
  
}



linAnalysis <- function (summaryDF, analysisYear, partialColumns, prismPath) {
  
  # Make a linear fit for the partial precipitation sums 
  # and the total water year sum
  
  # Then, output information about the models and their fit
  
  
  # First separate 'summaryDF' into calibration and validation datasets
  # Randomly select 2/3 of the water years to be used in calibration
  # The remaining 1/3 shall be used to validate the models
  seedVal <- 10
  
  
  set.seed(seedVal)
  
  
  calibrationIndices <- sample(nrow(summaryDF), round(2/3 * nrow(summaryDF)))
  validationIndices <- base::setdiff(1:nrow(summaryDF), calibrationIndices)
  
  
  # Split 'summaryDF' into two datasets
  caliDF <- summaryDF[sort(calibrationIndices), ]
  valiDF <- summaryDF[sort(validationIndices), ]
  
  
  # Build a linear model using the calibration dataset next
  # Then, assess the accuracy of the model in predicting values for 'valiDF'
  cat("\n")
  cat("Testing linear models...")
  cat("\n\n")
  
  
  # Define a regression summary data frame to hold results
  lmDF <- tibble("PREDICTOR_VARIABLE" = names(summaryDF) |>
                   str_subset("_PARTIAL_PRECIP"),
                 "SLOPE" = NA_real_,
                 "INTERCEPT" = NA_real_,
                 "CALIBRATION_R_SQUARED" = NA_real_,
                 "VALIDATION_R_SQUARED" = NA_real_)
  
  
  # Iterate through each of the partial precipitation columns
  for (i in 1:length(partialColumns)) {
    
    # Generate a linear regression model using the calibration dataset
    # Get the model parameters and calculate R^2 for both the calibration
    # and validation datasets
    modelRes <- modelAndTest(caliDF, valiDF, 
                             names(summaryDF) |>
                               str_subset("_PARTIAL") |> pluck(i), 
                             summaryDF, prismPath)
    
    
    lmDF$SLOPE[i] <- modelRes[2]
    lmDF$INTERCEPT[i] <- modelRes[1]
    lmDF$CALIBRATION_R_SQUARED[i] <- modelRes[3]
    lmDF$VALIDATION_R_SQUARED[i] <- modelRes[4]
    
  }
  
  
  # Write 'summaryDF' and 'lmDF' to a spreadsheet
  list(PRECIP_SUMMARY = summaryDF,
       LIN_REG = lmDF,
       SOURCE_FILE = data.frame("SOURCE" = prismPath),
       RANDOM_SEED = data.frame("RANDOM_SEED" = seedVal)) |>
    writeOutput(paste0("W2_Russian_River/Output/",
                       prismPath |> str_remove("^.+[/\\\\]") |>
                         str_remove("_Domain.+$"),
                       "_Precip_Lin_Regression.xlsx"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



modelAndTest <- function (caliDF, valiDF, xVarName, summaryDF, prismPath) {
  
  # Create a linear model using 'caliDF'
  # The response variable is "WY_TOTAL_PRECIP_MM"
  # The predictor variable is the column whose name is specified by 'xVarName'
  
  linModel <- lm(WY_TOTAL_PRECIP_MM ~ get(xVarName), caliDF)
  
  
  # Get the R Squared value for the calibration dataset
  caliRSq <- summary(linModel)$r.squared
  
  
  # Next, apply the model to the validation dataset
  # Calculate the predicted values and the residuals
  valiDF <- valiDF |>
    mutate(PREDICTED_VALUES = predict(linModel, valiDF),
           RESIDUALS = WY_TOTAL_PRECIP_MM - PREDICTED_VALUES)
  
  
  # Calculate the components for the SSR and SST as well
  valiDF <- valiDF |>
    mutate(SSR = (RESIDUALS)^2,
           SST = (WY_TOTAL_PRECIP_MM - mean(valiDF$WY_TOTAL_PRECIP_MM))^2)
  
  
  # After that, get the R^2 squared value for the validation dataset
  valiRSq <- 1 - sum(valiDF$SSR) / sum(valiDF$SST)
  
  
  # Output information about the regression results
  cat(paste0("Regression between 'WY_TOTAL_PRECIP_MM' and '", xVarName, "':\n\n",
             "y = ", round(linModel$coefficients[2], 2), " * x + ", round(linModel$coefficients[1], 2), "\n\n",
             "Calibration R^2: ", round(caliRSq, 4), "\n",
             "Validation R^2:  ", round(valiRSq, 4), "\n\n"))
  
  
  # Also construct a plot of the dataset
  
  # Define the x-axis label based on 'xVarName'
  xlabel <- xVarName |>
    str_split("_") |> unlist() |>
    paste0(collapse = " ") |>
    str_replace(" TO ", " to ") |>
    str_replace("PARTIAL PRECIP MM", "Precipitation (mm)")
  
  
  # Set the axis bounds based on the minimum and maximum values in the dataset
  axisBounds <- c(summaryDF[[xVarName]], 
                  predict(linModel, summaryDF), 
                  summaryDF$WY_TOTAL_PRECIP_MM) |>
    range()
  
  
  axisBounds[1] <- 10 * floor(axisBounds[1] / 10)
  axisBounds[2] <- 10 * ceiling(axisBounds[2] / 10)
  
  
  # A text box for the linear fit information
  fitText <- sprintf("Linear Fit\ny = %.3f * x + %.3f\nCali R^2 is %.3f\nVali R^2 is %.3f", 
                     linModel$coefficients[2], 
                     linModel$coefficients[1], 
                     caliRSq,
                     valiRSq)
  
  
  # The position of the linear fit text box
  boxPos <- c(x = 0.83 * axisBounds[2],
              y = 1.50 * (axisBounds[1] + 8))
  
  
  # Define the plot
  plotRes <- ggplot() +
    geom_point(mapping = aes(x = summaryDF[[xVarName]], y = summaryDF$WY_TOTAL_PRECIP_MM)) +
    geom_line(mapping = aes(x = summaryDF[[xVarName]], y = predict(linModel, summaryDF))) +
    xlab(xlabel) + ylab("Total Water Year Precipitation (mm)") +
    xlim(axisBounds[1], axisBounds[2]) + ylim(axisBounds[1], axisBounds[2]) +
    annotate("label", x = boxPos[1], y = boxPos[2], 
             label = fitText, fontface = "bold", size = 12 / .pt) +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(face = "bold", size = 12))
  
  
  # Define its filename
  fileName <- paste0("W2_Russian_River/Output/",
                     prismPath |> str_remove("^.+[/\\\\]") |>
                       str_remove("_Domain.+$"),
                     "_Total_Precip_vs_",
                     xVarName |> str_remove("_MM"), 
                     "_", Sys.Date()) |> 
    toupper() |>
    paste0(., ".png")
  
  
  # Save this file to the current working directory
  ggsave(fileName, plotRes, width = 3000, height = 2000, units = "px")
  
  
  # Also prepare a plot for the predicted total vs the actual total
  
  
  # Redefine the axis bounds
  axisBounds <- c(predict(linModel, summaryDF), 
                  summaryDF$WY_TOTAL_PRECIP_MM) |>
    range()
  
  
  axisBounds[1] <- 10 * floor(axisBounds[1] / 10)
  axisBounds[2] <- 10 * ceiling(axisBounds[2] / 10)
  
  
  # Generate the new plot
  plotRes <- ggplot() +
    geom_point(mapping = aes(x  = predict(linModel, summaryDF),
                             y = summaryDF$WY_TOTAL_PRECIP_MM)) +
    xlab("Predicted Total WY Precipitation (mm)") + 
    ylab("Actual Total WY Precipitation (mm)") +
    xlim(axisBounds[1], axisBounds[2]) + ylim(axisBounds[1], axisBounds[2]) +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(face = "bold", size = 12),
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
    ggtitle(paste0(xVarName |> str_remove("PARTIAL_PRECIP_MM"), 
                   "Model") |>
              str_replace_all("_", " ") |>
              str_replace(" TO ", " to "))
  
  
  # Save it to a file as well
  # First set the filename
  fileName <- paste0("W2_Russian_River/Output/",
                     prismPath |> str_remove("^.+[/\\\\]") |>
                       str_remove("_Domain.+$"),
                     "_Total_Precip_(Actual_vs_Predicted)_",
                     xVarName |> str_remove("PARTIAL_PRECIP_MM"), 
                     "MODEL_", Sys.Date()) |>
    toupper() |>
    paste0(., ".png")
  
  
  # Save this file to the current working directory
  ggsave(fileName, plotRes, width = 3000, height = 2000, units = "px")
  
  
  # Finally, return the coefficients and R^2 values in a vector
  return(c(linModel$coefficients, caliRSq, valiRSq))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

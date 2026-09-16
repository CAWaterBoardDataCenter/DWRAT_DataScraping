# Plot the watershed streamflow values stored in the PRMS model output file 
# ("rr_budget.out2")

# Include precipitation data in these charts (using historic PRISM precipitation
# data for PRMS's model domain as well as the gage data stored in the PRMS 
# DAT input file)


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("Additional_Scripts/Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####

mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'HLP_010_Plot_PRMS_Streamflow.R'!\n")
  
  
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
  # We want as much data for the PRMS model domain as possible
  # That would entail relying on the historic domain CSV, the recently downloaded
  # CSV for the current water year, and additional PRISM data for gaps between
  # the files
  
  # Use functions from another script to handle that process
  c("gatherPrecipPRISM", "validateAndSummarizePRISM") |>
    map(~ functionStealer("W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", .))
  
  
  # Collect PRISM precipitation data that is averaged for 
  # the entire PRMS model domain
  prismDF <- gatherPrecipPRISM(dirPath, endDate, "PRMS")
  
  
  # Import more precipitation data from the DAT file that was used to run PRMS
  # "HLP_011_Compare_SRP_Output_to_USGS_Gage.R" has a function for that too
  functionStealer("W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", 
                  "gatherPrecipDAT")
  
  
  datDF <- gatherPrecipDAT(dirPath, startDate, endDate, model = "PRMS")
  
  
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
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'HLP_010_Plot_PRMS_Streamflow.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
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
  functionStealer("W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", 
                  "prepNewDirectory")
  
  
  # By default, the folder name will be "Streamflow_QAQC"
  newDir <- prepNewDirectory(dirPath,
                             paste0(dirPath, "/PRMS/output/Streamflow_QAQC"))
  
  
  # Once 'newDir' has been established, 
  # create plots for different timescales
  
  
  # Borrow function that can help generate plots
  c("generateStreamflowPlot", "getNiceAxisBreaks", "setPrecipColumnWidths") |>
    map(~ functionStealer("W2_Russian_River/Scripts/HLP_011_Compare_SRP_Output_to_USGS_Gage.R", .))
  
  
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
                           precipType = "PRISM Avg",
                           extraYLabelFragment = "Model ")
  
  
  monthlyDF |>
    generateStreamflowPlot(paste0(newDir, "/Monthly_Streamflow_PRISM_Precip_", 
                                  timescale, ".png"),
                           yCol = "FLOW", isDaily = FALSE, 
                           precipDF = prismDF,
                           precipType = "PRISM Avg",
                           extraYLabelFragment = "Model ")
  
  
  # After that, use 'datDF' for precipitation data
  dailyDF |>
    generateStreamflowPlot(paste0(newDir, "/Daily_Streamflow_DAT_Precip_", 
                                  timescale, ".png"),
                           yCol = "FLOW", isDaily = TRUE, 
                           precipDF = datDF,
                           precipType = "DAT Avg",
                           extraYLabelFragment = "Model ")
  
  
  monthlyDF |>
    generateStreamflowPlot(paste0(newDir, "/Monthly_Streamflow_DAT_Precip_", 
                                  timescale, ".png"),
                           yCol = "FLOW", isDaily = FALSE, 
                           precipDF = datDF,
                           precipType = "DAT Avg",
                           extraYLabelFragment = "Model ")
  
  
  # Return nothing
  return(invisible(NULL))
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())

# To replace missing data and outliers in a PRMS gage's precipitation dataset,
# we have typically used PRISM data

# However, this can be unreliable at times
# For example, see: https://link.springer.com/article/10.1007/s00704-019-03012-6

# In the Russian River's case, precipitation is often overestimated by PRISM

# This script seeks to develop a different approach:
# Let's consider using data from other gages within the watershed

# If we can establish the correlation between precipitation data among different
# gages in the watershed, we can use linear regression models with well-correlated 
# gages to replace missing and extreme values in a gage's dataset

# Or, we could take the average of values from well-correlated gages and use that

# PRISM would be the fallback option if no well-correlated gages are available

# Even in that situation, rather than directly substituting in PRISM values,
# having a regression model that "corrects" the PRISM values for the gage 
# may give better results


# For each of the 15 precipitation gages used by PRMS, we will develop linear 
# regression models between each gage 

# In addition, each gage will be modeled against its counterpart PRISM dataset


# The automated workflow for the Russian River will rely on the output of this
# procedure, but this particular script will not be run regularly 

# Its purpose is to document the process that developed these models


# Gage data was downloaded on May 21, 2026 for the period between 
# January 1, 1990 and December 31, 2025

# The intermediate QA/QC meteorological CSV will be required for this script
# ("PRMS_Meteorological_QC_CIMIS_Intermediate_1990-01-01_2025-12-31.csv")

# Obtain this file from the appropriate SDA staff and add it to the 
# "Output" folder 

# The PRISM PRMS dataset will be required as well
# ("PRISM_PRMS_Data_1990-01-01_2025-12-31.csv")

# This file must also be procured; however, it should be stored in the
# "Intermediate" folder instead

# Similarly, the list of PRISM PRMS stations is required as well

# It should be specified under "PRISM_PRMS_STATIONS_CSV" of the RR Workflow
# control file spreadsheet


# NOTE

# Calibration and validation are not used in these linear models 
# (there are too many models >_>)

# However, it will be good to test this out in the future 
# (i.e., compare calibrated and validated model parameters to the ones developed 
#  in this procedure)



#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
source("W2_Russian_River/Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Functions ####


mainProcedure <- function () {
  
  cat("\n\n")
  cat("Starting 'RRW_EX3_Gage_Correlation.R'!\n")
  
  
  # In this script, 'startDate' and 'endDate' are hard-coded
  startDate <- "1990-01-01" |>
    as.Date(format = "%Y-%m-%d")
  
  endDate <- "2025-12-31" |>
    as.Date(format = "%Y-%m-%d")
  
  
  # Start by reading the gage data and PRISM CSV
  cat("\n[1/3]\tGetting gage and PRISM data...\n")
  
  
  # Get the path to the "Pre-QAQC" meteorological CSV
  meteorPath <- paste0("W2_Russian_River/Output/PRMS_Meteorological_QC_Intermediate_",
                       startDate, "_", endDate, ".csv")
  
  
  # Make sure the file exists
  if (!file.exists(meteorPath)) {
    
    paste0("Missing Required Meteorological File\n\n",
           "Please obtain the \"Intermediate QA/QC\" meteorological file that ",
           "contains gage data from ", startDate, " to ", endDate, ". Place ",
           "it in the \"Output\" folder.\n\n",
           "(\"", normalizePath(meteorPath, mustWork = FALSE), "\" does ",
           "not exist)") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Get the PRISM PRMS CSV's path next
  prismPath <- paste0("W2_Russian_River/Intermediate/PRISM_PRMS_Data_",
                      startDate, "_", endDate, ".csv")
  
  
  # Make sure the file exists
  if (!file.exists(prismPath)) {
    
    paste0("Missing Required PRISM File\n\n",
           "Please obtain the PRISM CSV file for PRMS stations that ",
           "contains gage data from ", startDate, " to ", endDate, ". Place ",
           "it in the \"Intermediate\" folder.\n\n",
           "(\"", normalizePath(prismPath, mustWork = FALSE), "\" does ",
           "not exist)") |>
      errWrap() |>
      stop()
    
  }
  
  
  # After that, get the path to the PRISM PRMS CSV 
  # that contains station information
  stationPath <- getFromControl_RR("PRISM_PRMS_STATIONS_CSV") |>
    sharepointPathCheck(isFolder = FALSE)
  
  
  # Read in 'meteorPath'
  meteorDF <- getFile(meteorPath)
  
  
  # Import 'prismPath' as well
  prismDF <- getPRISM(prismPath)
  
  
  # Finally, read in 'stationPath'
  stationDF <- getFile(stationPath)
  
  
  # Before proceeding, validate the PRISM files
  validateWebData(prismDF, "PRISM", prismPath, stationDF$STATION_ID, 
                  siPRISM = TRUE)
  
  
  validateStationInputs(stationDF, prismPath, "PRMS", 
                        names(meteorDF) |> str_subset("PRECIP") |> 
                          length(), 
                        names(meteorDF) |> str_subset("^TM((AX)|(IN))") |> 
                          length())
  
  
  cat("\tDone!\n\n")
  
  
  # Use 'meteorDF' and 'prismDF' to develop models
  cat(paste0("[2/3]\tDeveloping ", 
             choose(names(meteorDF) |> 
                      str_subset("PRECIP") |> length(), 
                    2) + 
               (names(meteorDF) |> str_subset("^PRECIP") |> length()), 
             " linear regression models...\n"))
  
  
  # Edit 'prismDF' to prepare it for regression with 'meteorDF'
  prismDF <- prismDF |>
    reformatPRISM(stationDF)
  
  
  # Remove outliers from 'meteorDF'
  functionStealer("W2_Russian_River/Scripts/RRW_006_Process_PRMS_Weather_Data.R",
                  "removeOutliers")
  
  meteorDF <- meteorDF |>
    removeOutliers(getFromControl_RR("PRMS_PRECIP_GAGE_OUTLIER_BOUNDS") |> getFile())

  
  # Use another function to develop each model
  modelDF <- generateModels(meteorDF, prismDF)
  
  
  # meteorDF |>
  #   select(-contains("TMIN")) |>
  #   select(-contains("TMAX")) |>
  #   pivot_longer(contains("PRECIP"), names_to = "GAGE", values_to = "PRECIP") |>
  #   mutate(YEAR = year(DATE), MONTH = month(DATE)) |>
  #   group_by(YEAR, MONTH, GAGE) |>
  #   summarize(AVAILABILITY = (n() - sum(is.na(PRECIP) | PRECIP < 0))/ n()) |>
  #   filter(AVAILABILITY >= 0.80)
  
  
  cat("\tDone!\n\n")
  
  
  # Save the outputs to a file
  cat(paste0("[3/3]\tSaving results...\n"))
  
  
  modelDF |>
    writeOutput("W2_Russian_River/Output/RR_Workflow_PRMS_Gage_Regression.csv")
  
  
  cat("\tDone!\n\n")
  
  
  # Output a completion message
  cat(col_green("\n'RRW_EX3_Gage_Correlation.R' is complete!\n\n"))
  
  
  # Return nothing
  return(invisible(NULL))
  
}



reformatPRISM <- function (prismDF, stationDF) {
  
  # Synthetic PRISM data is present for every PRMS gage in 'prismDF'
  
  # This data will be modeled against the actual gage data
  
  # To make this process easier, reformat the station data in 'prismDF' 
  # to give them the same column names as each gage
  
  
  # Use the "Name" field in 'prismDF' to join 'stationDF'
  # This will add "PRMS_PRECIP_NAME" to 'prismDF'
  prismDF <- prismDF |>
    rename(STATION_ID = Name) |>
    left_join(stationDF, by = "STATION_ID",
              relationship = "many-to-one")
  
  
  # Remove rows with "NA" for "PRMS_PRECIP_NAME"
  prismDF <- prismDF |>
    filter(!is.na(PRMS_PRECIP_NAME))
  
  
  # Pivot 'prismDF' into a wide format
  # The values in "ppt (mm)" become columns with the PRMS column names 
  # as their new names
  # The "Date" column uniquely identifies each row of data
  prismDF <- prismDF |>
    pivot_wider(names_from = PRMS_PRECIP_NAME,
                values_from = `ppt (mm)`,
                id_cols = Date)
  
  
  # Rename "Date" into "DATE" and return 'prismDF'
  return(prismDF |>
           rename(DATE = Date))
  
}



generateModels <- function (meteorDF, prismDF) {
  
  # Develop precipitation models between every gage included in 'meteorDF'
  
  # Record the model parameters and R^2 values
  
  
  # Get a list of precipitation columns in 'meteorDF'
  precipNames <- names(meteorDF) |>
    str_subset("PRECIP[0-9]+")
  
  
  # Iterate through the precipitation columns
  for (i in 1:(length(precipNames) - 1)) {
    
    # (The last column is skipped in the 'i' loop because it will already be 
    #  modeled against all other precipitation columns in their own loops)
    
    # Within each 'i' loop, iterate through the remaining precipitation columns
    # after column 'i'
    
    # (The prior columns will have already been modeled with in earlier loops)
    # (Also, no columns will be modeled against themselves)
    
    
    # Loop through the subsequent precipitation columns
    for (j in (i + 1):length(precipNames)) {
      
      # Make a temporary tibble with both precipitation columns
      tempDF <- meteorDF |>
        select(DATE, all_of(precipNames[c(i, j)]))
      
      
      # Remove rows with NA or missing entries (-999)
      tempDF <- tempDF |>
        filter(!is.na(get(precipNames[i])) & !is.na(get(precipNames[j]))) |>
        filter(get(precipNames[i]) >= 0 & get(precipNames[j]) >= 0)
      
      
      # Do not generate a model if less than one year of data is available
      # Similarly, every month should be represented in the dataset (with at
      # least one record)
      # In both cases, set 'resDF' to an empty model
      if (nrow(tempDF) < 365 * 1 || !all(1:12 %in% month(tempDF$DATE))) {
        
        resDF <- tibble(PREDICTOR = precipNames[i],
                        RESPONSE = precipNames[j],
                        SLOPE = NA_real_,
                        INTERCEPT = NA_real_,
                        R_SQUARED = NA_real_)
        
      } else {
        
        # Otherwise, generate a precipitation model between the two gage datasets
        resDF <- modelPrecip(tempDF[[precipNames[i]]], tempDF[[precipNames[j]]],
                             precipNames[i], precipNames[j])
        
      }
      
      
      # If this is the first iteration of 'i' and 'j', define a compiled tibble
      # using 'resDF'
      if (i == 1 && j == 2) {
        
        compiledDF <- resDF
        
      # Otherwise, append it to 'compiledDF'
      } else {
        
        compiledDF <- compiledDF |>
          bind_rows(resDF)
        
      }
      
    } # End of 'j' loop
    
  } # End of 'i' loop
  
  
  # Next, generate linear regression models between 'meteorDF' and 'prismDF'
  for (i in 1:length(precipNames)) {
    
    # Use "DATE" and join the same corresponding precipitation columns
    # into a new temporary tibble
    tempDF <- full_join(prismDF |> select(DATE, all_of(precipNames[i])) |>
                          rename(PRISM = precipNames[i]),
                        meteorDF |> select(DATE, all_of(precipNames[i])) |>
                          rename(GAGE = precipNames[i]),
                        by = "DATE", relationship = "one-to-one")
    
    
    # Remove entries where either value is NA or missing (-999)
    tempDF <- tempDF |>
      filter(!is.na(PRISM) & !is.na(GAGE)) |>
      filter(PRISM >= 0 & GAGE >= 0)
    
    
    # Generate a precipitation model between the two datasets
    resDF <- modelPrecip(tempDF$PRISM, tempDF$GAGE,
                         "PRISM", precipNames[i])
    
    
    # Add 'resDF' to 'compiledDF'
    compiledDF <- compiledDF |>
      bind_rows(resDF)
    
  }
  
  
  # Return 'compiledDF'
  return(compiledDF)
  
}



modelPrecip <- function (x, y, xName, yName) {
  
  # Between two gages' precipitation datasets ('x' and 'y'), 
  # generate a linear regression model
  
  
  # Generate a linear regression model between 'x' and 'y'
  precipRes <- try(lm(y ~ x), silent = TRUE)
  
  
  # If no model could be developed, return an empty row
  if ("try-error" %in% class(precipRes) ||
      is.nan(summary(precipRes)[["r.squared"]]) ||
      nrow(summary(precipRes)[["coefficients"]]) < 2) {
    
    resDF <- tibble(PREDICTOR = xName,
                    RESPONSE = yName,
                    SLOPE = NA_real_,
                    INTERCEPT = NA_real_,
                    R_SQUARED = NA_real_)
    
  } else {
    
    # Otherwise, create a tibble with information about the model
    resDF <- tibble(PREDICTOR = xName,
                    RESPONSE = yName,
                    SLOPE = summary(precipRes)[["coefficients"]][2, 1],
                    INTERCEPT = summary(precipRes)[["coefficients"]][1, 1],
                    R_SQUARED = summary(precipRes)[["r.squared"]])
    
  }
  
  
  
  
  
  # Return 'resDF'
  return(resDF)
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())


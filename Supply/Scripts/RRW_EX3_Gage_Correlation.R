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
# "ProcessedData" folder 

# The PRISM PRMS dataset will be required as well
# ("PRISM_PRMS_Data_1990-01-01_2025-12-31.csv")

# This file must also be procured; however, it should be stored in the
# "WebData" folder instead

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
source("Scripts/HLP_000_Load_Packages.R")


# Import shared functions
source("Scripts/HLP_001_Shared_Functions_Supply.R")
source("Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


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
  meteorPath <- paste0("ProcessedData/PRMS_Meteorological_EX_QC_CIMIS_Intermediate_",
                       startDate, "_", endDate, ".csv")
  
  
  # Make sure the file exists
  if (!file.exists(meteorPath)) {
    
    paste0("Missing Required Meteorological File\n\n",
           "Please obtain the \"Intermediate QA/QC\" meteorological file that ",
           "contains gage data from ", startDate, " to ", endDate, ". Place ",
           "it in the \"ProcessedData\" folder.\n\n",
           "(\"", normalizePath(meteorPath, mustWork = FALSE), "\" does ",
           "not exist)") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Get the PRISM PRMS CSV's path next
  prismPath <- paste0("WebData/PRISM_PRMS_Data_",
                      startDate, "_", endDate, ".csv")
  
  
  # Make sure the file exists
  if (!file.exists(prismPath)) {
    
    paste0("Missing Required PRISM File\n\n",
           "Please obtain the PRISM CSV file for PRMS stations that ",
           "contains gage data from ", startDate, " to ", endDate, ". Place ",
           "it in the \"WebData\" folder.\n\n",
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
             choose(names(meteorDF) |> str_subset("^PRECIP") |> length(), 
                    2) + 
               (names(meteorDF) |> str_subset("^PRECIP") |> length()) *
               (names(meteorDF) |> str_subset("^EX") |> length()), 
               length(sort(stationDF$PRMS_PRECIP_NAME, na.last = NA)), 
             " linear regression models...\n"))
  
  
  # Edit 'prismDF' to prepare it for regression with 'meteorDF'
  prismDF <- prismDF |>
    reformatPRISM(stationDF)
  
  
# Get CDEC data for use in 'meteorDF'
cdecDF <- getFile(paste0("WebData/CDEC_API_Data_", startDate, "_", endDate, ".csv")) |>
  filter(is.na(DATA_FLAG) | !(DATA_FLAG %in% c("A", "N", "v")))

cdecRef <- "Admin + Management/1. Staff Folders/APrashar/2026-05-28_RR_PRMS_Precip_QAQC/Option_4_Additional_Nearby_Gages/Maps_and_New_Stations/PRMS_Precipitation_QAQC_Candidate_Stations.csv" |>
  makeSharePointPath() |>
  getFile() |>
  filter(SOURCE == "CDEC") |>
  select(NAME, PRMS_PRECIP_NAME) |>
  rename(STATION_ID = NAME)


cdecDF <- cdecDF |>
  left_join(cdecRef, by = "STATION_ID", relationship = "many-to-one")


cdecDF <- cdecDF |> select(`DATE TIME`, VALUE, UNITS, PRMS_PRECIP_NAME) |>
  pivot_wider(id_cols = `DATE TIME`, names_from = PRMS_PRECIP_NAME,
              values_from = VALUE) |>
  rename(DATE = `DATE TIME`) |>
  mutate(DATE = as_date(DATE))


  meteorDF <- meteorDF |>
    full_join(cdecDF, by = "DATE")

  # Remove outliers from 'meteorDF'
  meteorDF <- meteorDF |>
    removeOutliers()

  
  # Use another function to develop each model
  modelDF <- generateModels(meteorDF, prismDF)
  
  
  
  # Try out a methodology using the regression data in 'modelDF'
  #methodDF <- testMethodology(meteorDF, prismDF, modelDF[[1]])
  
  
  cat("\tDone!\n\n")
  
  
  # Save the outputs to a file
  cat(paste0("[3/3]\tSaving results...\n"))
  
  
  modelDF[[1]] |>
    writeOutput("ProcessedData/RR_Workflow_PRMS_Gage_Regression.csv")
  
  
  modelDF[[2]] |>
    writeOutput("ProcessedData/RR_Workflow_PRMS_Best_Correlations.csv")
  
  
  modelDF[[3]] |>
    writeOutput("ProcessedData/RR_Workflow_PRMS_Remediation_Correlations.csv")
  
  
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
    str_subset("^PRECIP[0-9]+$")
  
  
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
        select(all_of(precipNames[c(i, j)]))
      
      
      # Remove rows with NA or missing entries (-999)
      tempDF <- tempDF |>
        filter(!is.na(get(precipNames[i])) & !is.na(get(precipNames[j]))) |>
        filter(get(precipNames[i]) >= 0 & get(precipNames[j]) >= 0)
      
      
      # Generate a precipitation model between the two gage datasets
      resDF <- modelPrecip(tempDF[[precipNames[i]]], tempDF[[precipNames[j]]],
                           precipNames[i], precipNames[j])
      
      
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
  
  
  # After that, generate linear regression models between "PRECIP" columns
  # in 'meteorDF' and the "EX_PRECIP" columns in that table
  exNames <- names(meteorDF) |>
    str_subset("^EX_PRECIP")
  
  
  for (i in 1:length(precipNames)) {
    
    for (j in 1:length(exNames)) {
      
      # Get data from both gages
      tempDF <- meteorDF |>
        select(DATE, all_of(precipNames[i]), all_of(exNames[j]))
      
      
      # Remove entries where either value is NA or missing (-999)
      tempDF <- tempDF |>
        filter(!is.na(get(precipNames[i])) & !is.na(get(exNames[j]))) |>
        filter(get(precipNames[i]) >= 0 & get(exNames[j]) >= 0)
      
      
      # Skip the model if less than two years of data is available
      if (nrow(tempDF) < 365 * 2) {
        next
      }
      
      # Skip the model unless every month is represented in the dataset
      # (with at least one record)
      if (anyFalse(1:12 %in% month(tempDF$DATE))) {
        next
      }
      
      
      # Generate a precipitation model between the two datasets
      resDF <- modelPrecip(tempDF[[precipNames[i]]], tempDF[[exNames[j]]],
                           precipNames[i], exNames[j])
      
      
      if (is.null(resDF)) {
        next
      }
      
      
      # Add 'resDF' to 'compiledDF'
      compiledDF <- compiledDF |>
        bind_rows(resDF)
      
    }
    
  }
  
  
  
  # After that, generate linear regression models between EX gages and 'prismDF'
  for (i in 1:length(exNames)) {
    
    # Use "DATE" and join the same corresponding precipitation columns
    # into a new temporary tibble
    tempDF <- full_join(prismDF |> select(DATE, all_of(exNames[i])) |>
                          rename(PRISM = exNames[i]),
                        meteorDF |> select(DATE, all_of(exNames[i])) |>
                          rename(GAGE = exNames[i]),
                        by = "DATE", relationship = "one-to-one")
    
    
    # Remove entries where either value is NA or missing (-999)
    tempDF <- tempDF |>
      filter(!is.na(PRISM) & !is.na(GAGE)) |>
      filter(PRISM >= 0 & GAGE >= 0)
    
    
    if (nrow(tempDF) == 0) {
      next
    }
    
    
    # Generate a precipitation model between the two datasets
    resDF <- modelPrecip(tempDF$PRISM, tempDF$GAGE,
                         "PRISM", exNames[i])
    
    
    # Add 'resDF' to 'compiledDF'
    compiledDF <- compiledDF |>
      bind_rows(resDF)
    
  }
  
  
  
  for (i in 1:length(precipNames)) {
    
    sortedDF <- compiledDF |>
      filter(PREDICTOR == precipNames[i] | RESPONSE == precipNames[i]) |>
      arrange(desc(R_SQUARED))
    
    print(sortedDF)
    
    sortedDF <- sortedDF |>
      mutate(MODEL_FOR = precipNames[i]) |>
      relocate(MODEL_FOR)
    
    if (i == 1) {
      finalDF <- sortedDF
    } else {
      finalDF <- bind_rows(finalDF, sortedDF)
    }
    
    
  }
  
  
  # stationDF <- stationDF |>
  #   filter(!is.na(PRMS_PRECIP_NAME)) |>
  #   st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = "WGS84") |>
  #   st_transform("epsg:3488")
  # 
  # distMatrix <- st_distance(stationDF, stationDF)
  # 
  # 
  # for (i in 1:length(precipNames)) {
  #   
  #   if (!(i %in% c(2, 5))) {
  #     next
  #   }
  #   
  #   
  #   matchingStation <- which(stationDF$PRMS_PRECIP_NAME == precipNames[i])
  #   
  #   distRanking <- distMatrix[matchingStation, ] |>
  #     min_rank()
  #   
  #   
  #   nearestStations <- stationDF$PRMS_PRECIP_NAME[distRanking] |>
  #     base::setdiff(precipNames[i]) |>
  #     base::setdiff(c("PRECIP1", "PRECIP4", "PRECIP7", "PRECIP6", "PRECIP12")) |>
  #     head(3)
  #   
  #   nearestStations <- c("PRECIP14", "PRECIP2", "PRECIP8")
  #   
  #   
  #   nearbyDF <- meteorDF |>
  #     select(DATE, all_of(nearestStations))
  #   
  #   
  #   nearbyDF <- nearbyDF |>
  #     mutate(AVG_PRECIP = nearbyDF[nearestStations] |>
  #              rowMeans()) |>
  #     filter(!is.na(AVG_PRECIP) & AVG_PRECIP > 0)
  #   
  #   
  #   
  #   tempDF <- meteorDF |>
  #     select(DATE, all_of(precipNames[i])) |>
  #     left_join(nearbyDF |> select(DATE, AVG_PRECIP),
  #               by = "DATE", relationship = "one-to-one") |>
  #     filter(!is.na(AVG_PRECIP))
  #   
  #   
  #   resDF <- modelPrecip(tempDF[[precipNames[i]]], tempDF$AVG_PRECIP,
  #                        precipNames[i], paste0("AVG OF ",
  #                                               paste0(nearestStations, collapse = ", ")))
  #   
  #   
  #   
  # }
  # 
  
  for (i in 1:length(precipNames)) {
    
    # if (!(i %in% c(2, 5))) {
    #   next
    # }
    
    
    # excludeVec <- c("PRECIP1", "PRECIP4", "PRECIP7", "PRECIP13") |>
    #   base::setdiff(paste0("PRECIP", i))
    excludeVec <- ""
    
    
    # Find which gages correlated well with this iteration's gage
    # Take the three gages with the highest R^2 values
    # (Ignore PRISM and certain problematic gages)
    similarGages <- compiledDF |>
      filter(PREDICTOR == precipNames[i] | RESPONSE == precipNames[i]) |>
      #filter(PREDICTOR != "PRISM") |>
      filter(!(PREDICTOR %in% excludeVec)) |>
      filter(!(RESPONSE %in% excludeVec)) |>
      arrange(desc(R_SQUARED)) |>
      #head(3) |>
      select(PREDICTOR, RESPONSE) |>
      t() |> as.vector()|> unique() |>
      base::setdiff(precipNames[i])
    
    
    # Calculate the average values among the three selected gages
    avgDF <- meteorDF |>
      select(DATE, all_of(similarGages[similarGages != "PRISM"])) |>
      full_join(prismDF |> select(DATE, all_of(precipNames[i])) |> rename(PRISM = precipNames[i]),
                by = "DATE", relationship = "one-to-one") |>
      mutate(AVG_PRECIP = NA_real_)
    
    
    reqGages <- c()
    
    # avgDF <- avgDF |>
    #   mutate(NA_COUNT = is.na(avgDF[similarGages]) |> rowSums()) |>
    #   mutate(AVG_PRECIP = if_else(NA_COUNT < 2, 
    #                               avgDF[similarGages] |> rowMeans(na.rm = TRUE),
    #                               avgDF[similarGages] |> rowMeans(na.rm = FALSE)))
    
    for (j in 1:nrow(avgDF)) {
      
      rowVals <- avgDF[j, similarGages] |>
        unlist(use.names = TRUE)
      
      
      rowVals <- rowVals[!is.na(rowVals)]
      
      rowVals <- rowVals[rowVals >= 0]
      
      
      if (!anyNA(rowVals[1:3])) {
        
        avgDF$AVG_PRECIP[j] <- mean(rowVals[1:3])
        
        reqGages <- c(reqGages, names(rowVals[1:3])) |>
          unique()
        
        
      } else {
        
        avgDF$AVG_PRECIP[j] <- mean(rowVals[1:2])
        
        reqGages <- c(reqGages, names(rowVals[1:2])) |>
          unique()
        
        reqGages <- reqGages[!is.na(reqGages)]
        
      }
      
    }
    
    
    
    # Plot 'avgDF' and 'meteorDF'
    ggplot() +
      geom_line(data = meteorDF |> filter(!is.na(get(precipNames[i]))), 
                mapping = aes(x = DATE, y = get(precipNames[i]),
                              color = precipNames[i])) +
      geom_line(data = avgDF |> filter(!is.na(AVG_PRECIP)),
                mapping = aes(x = DATE, y = AVG_PRECIP,
                              color = "AVG")) +
      scale_color_manual(values = c("blue", "red") |> set_names(c(precipNames[i], "AVG")))
    
    
    # Define 'tempDF' using the iteration's precipitation column and 'avgDF'
    tempDF <- meteorDF |>
      select(DATE, all_of(precipNames[i])) |>
      full_join(avgDF |> select(DATE, AVG_PRECIP),
                by = "DATE", relationship = "one-to-one") |>
      filter(!is.na(AVG_PRECIP)) |>
      filter(!is.na(get(precipNames[i]))) |>
      filter(get(precipNames[i]) >= 0)
    
    
    # 
    resDF <- modelPrecip(tempDF$AVG_PRECIP, tempDF[[precipNames[i]]], 
                         paste0("AVG of ", paste0(similarGages, collapse = ", ")), 
                         precipNames[i])
    
    
    tempDF <- tempDF |>
      mutate(PREDICTED_PRECIP = AVG_PRECIP * resDF$SLOPE[1] + resDF$INTERCEPT[1])
    
    
    maxBound <- range(c(tempDF[precipNames[i]], tempDF$PREDICTED_PRECIP))
    
    
    ggplot() +
      geom_point(data = tempDF, 
                mapping = aes(x = get(precipNames[i]), y = PREDICTED_PRECIP)) +
      coord_cartesian(xlim = maxBound, ylim = maxBound) +
      geom_line(data = tempDF,
                mapping = aes(x = get(precipNames[i]), y = get(precipNames[i])), linetype = 2) +
      xlab(precipNames[i]) +
      annotate("text", label = paste0("R Squared: ", resDF$R_SQUARED |> round(digits = 3)),
               x = 0.10 * maxBound[2], y = 0.85 * maxBound[2])
    

    # if (i %in% c(13)) {
    #   meteorDF |>
    #     select(DATE, all_of(precipNames[i])) |>
    #     full_join(avgDF, by = "DATE", relationship = "one-to-one") |>
    #     mutate(!! paste0("PREDICTED_PRECIP_", i) := if_else(is.na(AVG_PRECIP), NA_real_, AVG_PRECIP * resDF$SLOPE + resDF$INTERCEPT)) |>
    #     write_xlsx(paste0("PRECIP_", i, "_Analysis.xlsx"))
    # }

    if (i == 1) {
      
      avgResDF <- resDF |>
        mutate(REQ_GAGES = reqGages |> paste0(collapse = ", ")) |>
        relocate(REQ_GAGES)
      
    } else {
      
      avgResDF <- bind_rows(avgResDF,
                            resDF |>
                              mutate(REQ_GAGES = reqGages |> paste0(collapse = ", ")) |>
                              relocate(REQ_GAGES))
      
    }
    
    
    print(resDF)
    
  }
  
  
  
  return(list(compiledDF, finalDF, avgResDF))
  
  ## Return 'compiledDF'
  #return(compiledDF)
  
}



modelPrecip <- function (x, y, xName, yName) {
  
  # Between two gages' precipitation datasets ('x' and 'y'), 
  # generate a linear regression model
  
  
  # Generate a linear regression model between 'x' and 'y'
  precipRes <- lm(y ~ x)
  
  
  # If no model could be developed, return nothing
  if (is.nan(summary(precipRes)[["r.squared"]]) ||
      nrow(summary(precipRes)[["coefficients"]]) < 2) {
    return(NULL)
  }
  
  
  # Create a tibble with information about the model
  resDF <- tibble(PREDICTOR = xName,
                  RESPONSE = yName,
                  SLOPE = summary(precipRes)[["coefficients"]][2, 1],
                  INTERCEPT = summary(precipRes)[["coefficients"]][1, 1],
                  R_SQUARED = summary(precipRes)[["r.squared"]])
  
  
  # Return 'resDF'
  return(resDF)
  
}



removeOutliers <- function (meteorDF) {
  
  # Start by removing outliers from each gage in 'meteorDF'
  
  # This includes the remediation gages
  
  
  # Get a list of PRECIP and EX_PRECIP gages
  precipNames <- names(meteorDF) |>
    str_subset("^PRECIP")
  
  
  exNames <- names(meteorDF) |>
    str_subset("^EX_PRECIP")
  
  
  outlierBounds <- tibble(GAGE = c(precipNames, exNames))
  
  outlierBounds[month.abb] <- NA_real_
  
  
  # Iterate through the precipitation gages and calculate outlier boundaries
  for (i in 1:length(precipNames)) {
    
    subsetDF <- meteorDF |>
      select(DATE, all_of(precipNames[i])) |>
      rename(PRECIP = precipNames[i]) |>
      filter(!is.na(PRECIP)) |>
      filter(PRECIP > 1 * 25.4) |> # 1 inch
      mutate(MONTH = month(DATE))
    
    
    outlierDF <- subsetDF |>
      group_by(MONTH) |>
      summarize(BOUND = quantile(PRECIP, 0.75) + 3.5 * IQR(PRECIP),
                .groups = "drop")
    
    
    if (nrow(outlierDF) < 12) {
      outlierDF <- outlierDF |>
        bind_rows(tibble(MONTH = base::setdiff(1:12, outlierDF$MONTH),
                         BOUND = NA_real_))
    }
    
    
    outlierDF$BOUND[is.na(outlierDF$BOUND)] <- 3.5 * 25.4
    
    
    outlierDF <- outlierDF |>
      arrange(MONTH)
    
    
    outlierBounds[outlierBounds$GAGE == precipNames[i], month.abb] <- outlierDF$BOUND |> t()
    
    
  }
  
  
  # Calculate bounds for the EX gages next
  for (i in 1:length(exNames)) {
    
    subsetDF <- meteorDF |>
      select(DATE, all_of(exNames[i])) |>
      rename(PRECIP = exNames[i]) |>
      filter(!is.na(PRECIP)) |>
      filter(PRECIP > 1 * 25.4) |> # 1 inch
      mutate(MONTH = month(DATE))
    
    
    outlierDF <- subsetDF |>
      group_by(MONTH) |>
      summarize(BOUND = quantile(PRECIP, 0.75) + 3.5 * IQR(PRECIP),
                .groups = "drop")
    
    
    if (nrow(outlierDF) < 12) {
      outlierDF <- outlierDF |>
        bind_rows(tibble(MONTH = base::setdiff(1:12, outlierDF$MONTH),
                         BOUND = NA_real_))
    }
    
    
    outlierDF$BOUND[is.na(outlierDF$BOUND)] <- 3.5 * 25.4
    
    
    outlierDF <- outlierDF |>
      arrange(MONTH)
    
    
    outlierBounds[outlierBounds$GAGE == exNames[i], month.abb] <- outlierDF$BOUND |> t()
    
    
  }
  
  
  
  # Add a month column
  meteorDF <- meteorDF |>
    mutate(MONTH = month(DATE))
  
  
  # Apply all outlier bounds to 'meteorDF'
  for (i in 1:nrow(outlierBounds)) {
    
    # Find the location of the relevant gage in 'meteorDF'
    gageCol <- which(names(meteorDF) == outlierBounds$GAGE[i])
    
    
    # Remove negative precip
    removeRows <- meteorDF |>
      mutate(ROW = row_number()) |>
      filter(!is.na(get(outlierBounds$GAGE[i]))) |>
      filter(get(outlierBounds$GAGE[i]) < 0) |>
      select(ROW)
    
    
    if (nrow(removeRows) > 0) {
      meteorDF[removeRows[[1]], gageCol] <- NA_real_
    }
    
    
    # Check each month for outliers
    for (j in 1:12) {
      
      removeRows <- meteorDF |>
        mutate(ROW = row_number()) |>
        filter(!is.na(get(outlierBounds$GAGE[i]))) |>
        filter(MONTH == j) |>
        filter(get(outlierBounds$GAGE[i]) > outlierBounds[[month.abb[j]]][i]) |>
        select(ROW)
      
      
      if (nrow(removeRows) > 0) {
        meteorDF[removeRows[[1]], gageCol] <- NA_real_
      }
      
    }
    
  }
  
  # Once 'meteorDF' has been cleared of outliers, return it
  return(meteorDF)
  
}



testMethodology <- function (meteorDF, prismDF, compiledDF) {
  
  
  # Apply a methodology for replacing QA/QC data
  
  # Compare its performance to the original dataset
  
  
  # Wherever gage data is available, apply the procedure with the
  # assumption that it is actually missing
  
  # Then compare the results of the two (remediation results vs the real data)
  
  
  precipNames <- meteorDF |>
    str_subset("^PRECIP")
  
  
  
  
  
}



#### Script Execution ####

mainProcedure()


# Clean up
base::remove(list = ls())


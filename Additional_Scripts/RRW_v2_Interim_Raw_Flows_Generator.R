# Combine the outputs of RRIHM and SRPHM

# (This is a stripped-down version of RRW_016_Generate_Raw_Flows.R" for interim use)
# (The more formal v2 version will be written later)


# Clear the environment
base::remove(list = ls())


# Please specify the folders where the RRIHM and SRPHM output files are stored

prmsOutDir <- "C:/Users/.../060426_baseline_postspinup_052626DAT/modflow/RRIHM_outputs_060426"

srpOutDir <- "C:/Users/.../FCorella_model_start_10-01-20_060326/modflow"


# Make sure the starting date of the files is specified correctly too
startDate <- "2020-10-01" |>
  as.Date(format = "%Y-%m-%d")


# Then, click "Source" to run the script!



#### Setup ####

# Make sure the "Supply" RProj is active
source("W2_Russian_River/Scripts/HLP_004_Check_Working_Directory.R")


# Import packages
require(data.table)
require(tidyverse)
require(readxl)
require(cli)
require(httr)
require(rvest)
require(fs)
require(SPEI)
require(writexl)
require(RSelenium)
require(wdman)
require(netstat)
require(binman)
require(jsonlite)


# Import shared functions
source("Shared_Scripts/!Shared_Functions_Importer.R")
source("W2_Russian_River/Scripts/HLP_003_RR_Workflow_Validation_Functions.R")


#### Procedure ####

print("Starting script...")


# Check that the directories exist
if (!dir.exists(prmsOutDir)) {
  stop("RRIHM Directory Not Found")
}


if (!dir.exists(srpOutDir)) {
  stop("SRPHM Directory Not Found")
}


# Define the paths to the 22 RRIHM and 6 SRPHM files
goPaths <- paste0(prmsOutDir, "/subbasin_", 1:22, ".go") |>
  normalizePath(mustWork = FALSE)


gagPaths <- paste0(srpOutDir, "/SRP_inflow_", 1:6, ".gag") |>
  normalizePath(mustWork = FALSE)

if (!all(file.exists(gagPaths))) {
  stop("All 6 SRPHM gag files NOT FOUND")
}

if (!all(file.exists(goPaths))) {
  stop("All 22 SRPHM go files NOT FOUND")
}


# Iteratively read in the GO files
for (i in 1:length(goPaths)) {
  
  # Read in the file 
  tempDF <- read_gag(goPaths[i])
  
  
  # Get a vector of dates starting from 'startDate'
  dateVec <- seq(from = startDate, by = "days", length.out = nrow(tempDF))
  
  
  # Convert the flow values from m^3/day to AF/day
  # Rename "Flow" into the sub-basin number too
  tempDF <- tempDF |>
    mutate(Date = dateVec) |>
    select(Date, Flow) |>
    mutate(Flow = Flow / 1233.48) |>
    rename(!! paste0("GO_", i) := Flow)
  
  
  # Combine flow values from each file into a single tibble
  if (i == 1) {
    combinedDF <- tempDF
  } else {
    combinedDF <- combinedDF |>
      full_join(tempDF, by = c("Date"))
  }
  
  
}


# Iteratively read in the GAG files
# Join them together into a separate variable
for (i in 1:length(gagPaths)) {
  
  # Read in the file 
  tempDF <- read_gag(gagPaths[i])
  
  
  # Get a vector of dates starting from 'startDate'
  dateVec <- seq(from = startDate, by = "days", length.out = nrow(tempDF))
  
  
  # Convert the flow values from cfd to AF/day
  # Rename "Flow" into the sub-basin number too
  tempDF <- tempDF |>
    mutate(Date = dateVec) |>
    select(Date, Flow) |>
    mutate(Flow = Flow / 43559.9) |>
    rename(!! as.character(i) := Flow)
  
  
  if (i == 1) {
    gagDF <- tempDF
  } else {
    gagDF <- gagDF |>
      full_join(tempDF, by = c("Date"))
  }
  
}


# Calculate the subbasin flow values
combinedDF <- combinedDF |>
  mutate(`1` = GO_1,
         `2` = GO_2,
         `3` = GO_3 - GO_2,
         `4` = GO_4 - GO_3 - GO_1,
         `5` = GO_5 - GO_4,
         `6` = GO_6 - GO_5,
         `7` = GO_7,
         `8` = GO_8 - GO_7,
         `9` = GO_9 - GO_6 - GO_8,
         `10` = GO_10 - GO_9,
         `11` = GO_11,
         `12` = GO_12 - GO_10 - GO_11,
         `13` = GO_13 - GO_12,
         `14` = GO_14 - GO_22,
         `15` = GO_15 - GO_14,
         `16` = GO_16 - GO_15,
         `17` = GO_17 - GO_13 - GO_16,
         `18` = GO_18 - GO_17,
         `19` = GO_19 - GO_18,
         `20` = GO_20,
         `21` = GO_21 - GO_19 - GO_20,
         `22` = GO_22) |>
  select(Date, as.character(1:length(goPaths)))

gagDF <- gagDF |>
  mutate(`23` = `1`,
         `24` = `6` - `1` - `5`,
         `25` = `5` - `4` - `3`,
         `26` = `4` - `2`,
         `27` = `2`,
         `28` = `3`) |>
  select(Date, as.character(23:(22 + length(gagPaths))))


# Append 'gagDF' to 'combinedDF'
if (nrow(gagDF) != nrow(combinedDF)) {
  stop("Mismatch between RRIHM and SRPHM datasets--different number of rows")
}


# Bind the two tibbles together
# (Exclude the "Time" column from them)
combinedDF <- combinedDF |>
  full_join(gagDF, by = c("Date"))


# Convert the values from AF/day into AF/month
combinedDF <- combinedDF |>
  mutate(MONTH = month(Date), YEAR = year(Date)) |>
  group_by(YEAR, MONTH) |>
  summarize(across(where(is.numeric), sum), .groups = "drop")


# Address negative flows next
# Use old functions for that


adjustNegativeFlows <- function (flowDF) {
  
  # In each year-month pair of 'flowDF', 
  # if there are negative flows in a sub-basin, 
  # borrow flow from upstream locations to zero out those negative values
  
  
  # First check if there are any negative flows in 'flowDF'
  if (!any(select(flowDF, -Date) < 0)) {
    
    # If there are none, return 'flowDF' without any changes
    return(flowDF)
    
  }
  
  
  # Otherwise, read in a connectivity table for the watershed
  # It needs some preparations before it can be used
  # Those operations are performed in `getBasinConnectivity`
  connDF <- getBasinConnectivity()
  
  
  # Next, iterate through each of the rows in 'flowDF'
  for (i in 1:nrow(flowDF)) {
    
    # Each row of 'flowDF' is a different year-month pair
    
    # Check if any flows are negative for this iteration
    # If there are no negative flows, skip it
    if (!any(select(flowDF[i, ], -Date) < 0)) {
      
      next
      
    }
    
    
    # While there are negative flows in this row of 'flowDF',
    # keep going through this loop
    while (any(select(flowDF[i, ], -Date) < 0)) {
      
      # Check which columns in 'flowDF' have negative flows
      colIndex <- which(flowDF[i, ] < 0 & names(flowDF) != "Date")
      
      
      # Get the sub-basin numbers that correspond to the 
      # 'colIndex' values with negative flows
      basinNum <- names(flowDF)[colIndex] |>
        as.numeric()
      
      
      # Iterate through these negative flow basins
      for (j in 1:length(basinNum)) {
        
        # For this iteration's sub-basin,
        # Identify which sub-basin is upstream of it
        upstreamBasin <- connDF$BASIN_NUM[connDF$FLOWS_TO_NUM == basinNum[j]]
        
        
        # If no sub-basin is upstream of this negative flow sub-basin,
        if (length(upstreamBasin) == 0) {
          
          # Just zero out the sub-basin's flow
          flowDF[i, colIndex[j]] <- 0
          
          
          # Otherwise, if exactly one sub-basin is upstream of the basin
        } else if (length(upstreamBasin) == 1) {
          
          # Get the index in 'flowDF' where this upstream sub-basin is located
          upstreamIndex <- which(names(flowDF) == upstreamBasin)
          
          
          # Double-check that the index is valid
          if (length(upstreamIndex) == 0) {
            
            paste0("Sub-Basin ID Not Present in Raw Flows\n\n",
                   "For ", flowDF$Date[i], ", the script attempted to extract ",
                   "data related to Sub-Basin \"", upstreamBasin, "\". ",
                   "However, the procedure failed to find a matching column ",
                   "in the flow dataset.\n\n",
                   "This could be an error in the flow data or in the ",
                   "sub-basin connectivity table. Please investigate.") |>
              errWrap() |>
              stop()
            
          } else if (length(upstreamIndex) > 1) {
            
            paste0("Sub-Basin ID Not Present More Than Once In Raw Flows\n\n",
                   "For ", flowDF$Date[i], ", the script attempted to extract ",
                   "data related to Sub-Basin \"", upstreamBasin, "\". ",
                   "However, the procedure encountered multiple columns with ",
                   "this name in the flow dataset.\n\n",
                   "This is likely a script error. Please investigate.") |>
              errWrap() |>
              stop()
            
          }
          
          
          # Add the negative flow value to the upstream basin's value
          # (This is like "borrowing" flow from upstream 
          #  to zero out the negative flow)
          flowDF[i, upstreamIndex] <- 
            flowDF[i, upstreamIndex] + flowDF[i, colIndex[j]]
          
          
          # Then, set the negative flow to zero in the flagged basin
          flowDF[i, colIndex[j]] <- 0
          
          
          # If a sub-basin has multiple direct upstream sub-basins
        } else {
          
          # Split the negative flow between all upstream sub-basins
          # (i.e., "borrow" some flow from every sub-basin)
          
          # For each upstream sub-basin, get the TOTAL flow available in 
          # those sub-basins (including their respective upstream sub-basins)
          
          # Then calculate a ratio for each upstream sub-basin and use that 
          # to determine the flow contribution from each sub-basin in 
          # 'upstreamBasin' (i.e., the amount of flow from each sub-basin that
          # will offset the negative flow value)
          totalAvailability <- upstreamBasin |>
            map_dbl(~ calcTotalAvailableFlow(connDF, flowDF[i, ], .))
          
          
          # If the total available upstream flow is zero, just set the 
          # negative flow entry in 'flowDF' to zero
          if (all(totalAvailability == 0)) {
            
            # (This applies only if every upstream sub-basin has zero flow available)
            flowDF[i, colIndex[j]] <- 0
            
          } else {
            
            # Otherwise, calculate ratios based on 'totalAvailability'
            contributionRatios <- totalAvailability / sum(totalAvailability)
            
            
            # From each of the sub-basins that are upstream of 'basinNum',
            # take flow based on its corresponding ratio
            for (k in 1:length(upstreamBasin)) {
              
              # Locate one of the immediately upstream sub-basins in 'flowDF'
              upstreamIndex <- which(names(flowDF) == upstreamBasin[k])
              
              
              # Double-check that the index is valid
              if (length(upstreamIndex) == 0) {
                
                paste0("Sub-Basin ID Not Present in Raw Flows\n\n",
                       "For ", flowDF$Date[i], ", the script attempted to extract ",
                       "data related to Sub-Basin \"", upstreamBasin[k], "\". ",
                       "However, the procedure failed to find a matching column ",
                       "in the flow dataset.\n\n",
                       "This could be an error in the flow data or in the ",
                       "sub-basin connectivity table. Please investigate.") |>
                  errWrap() |>
                  stop()
                
              } else if (length(upstreamIndex) > 1) {
                
                paste0("Sub-Basin ID Not Present More Than Once In Raw Flows\n\n",
                       "For ", flowDF$Date[i], ", the script attempted to extract ",
                       "data related to Sub-Basin \"", upstreamBasin[k], "\". ",
                       "However, the procedure encountered multiple columns with ",
                       "this name in the flow dataset.\n\n",
                       "This is likely a script error. Please investigate.") |>
                  errWrap() |>
                  stop()
                
              }
              
              
              # Adjust the upstream sub-basin and "borrow" some flow to offset
              # the negative value in that sub-basin
              flowDF[i, upstreamIndex] <- 
                flowDF[i, upstreamIndex] + 
                contributionRatios[k] * flowDF[i, colIndex[j]]
              
              
              # The contribution ratios consider the flow available in the 
              # entire upstream path for each immediately upstream sub-basin
              
              # So, it is possible that the immediately upstream sub-basin will
              # get a negative flow value after this operation
              
              # That's okay though, because the "while" loop will keep going
              # as long as there are any negative flows
              
            }
            
            
            # After "borrowing" flow from each of the immediately upstream
            # sub-basins, set the negative flow in the flagged basin to zero 
            flowDF[i, colIndex[j]] <- 0
            
          } 
          
        } # End of conditional for multiple upstream sub-basins
        
      } # End of loop through negative sub-basins
      
    } # End of while loop for negative flows in a row of 'flowDF'
    
  } # End of loop through each year-month pair in 'flowDF'
  
  
  # Finally, return 'flowDF'
  return(flowDF)
  
}



getBasinConnectivity <- function () {
  
  # Read in a table that expresses the connectivity between sub-basins
  # in the watershed
  
  # Some additional columns will be included in this dataset to assist
  # with the procedure
  
  
  # Get the path to this file
  basinPath <- getFromControl_RR("SUBBASIN_CONNECTIVITY_CSV")
  
  
  # Use a validation function from another script
  functionStealer("W2_Russian_River/Scripts/RRW_019_Finalize_DWRAT_Inputs.R",
                  "validateBasins")
  
  
  # Read in the sub-basin file and apply the validation function too
  connDF <- basinPath |>
    getFile() |>
    validateBasins(basinPath)
  
  
  # 'flowDF' has its sub-basins as numbers (in the column names)
  # Meanwhile, 'connDF' has its sub-basin numbers stored within strings
  
  # On top of that, 'connDF' can have both mainstem and non-mainstem 
  # entries for a sub-basin
  
  
  # To make 'connDF' easier to work with, add three new columns:
  
  # (*) The first column will have the numeric portion of the "BASIN" label
  
  # (*) The second column will have the numeric portion of the "FLOWS_TO" label
  
  # (*) The third column will be an integer that states whether a "BASIN" value
  #     is mainstem or not. If it's a mainstem sub-basin, its value will be "1". 
  #     Otherwise, its value will be "0". 
  
  #     This will be useful when there are two rows for the same sub-basin
  #     (and one is mainstem while the other is not)
  
  #     When there are two options like this, we will take the *mainstem* one
  #     This is because the tributary option usually flows into its mainstem 
  #     counterpart anyways
  
  
  # Add the new columns to 'connDF'
  connDF <- connDF |>
    mutate(BASIN_NUM = str_extract(BASIN, "[0-9]+") |> as.numeric(),
           FLOWS_TO_NUM = str_extract(FLOWS_TO, "[0-9]+") |> as.numeric(),
           MAINSTEM_INT = if_else(MAINSTEM == "Y", 1, 0))
  
  
  # Then, group by sub-basin number and filter to one "BASIN" per "BASIN_NUM"
  # (So if there is a mainstem and tributary sub-basin option, consider the
  #  connectivity of the mainstem sub-basin only)
  # (Generally speaking, the tributaries drain into their mainstems anyways,
  #  so they wouldn't be helpful in this procedure)
  connDF <- connDF |>
    group_by(BASIN_NUM) |>
    filter(MAINSTEM_INT == max(MAINSTEM_INT)) |>
    ungroup() 
  
  
  # One more filter is needed to avoid circular connectivity
  # (i.e., a sub-basin shouldn't flow into itself)
  # It will trap the procedure in an infinite loop otherwise
  connDF <- connDF |>
    filter(BASIN_NUM != FLOWS_TO_NUM)
  
  
  # Another check for the Russian River is whether 'connDF' contains a 
  # connection between the Upper Russian River (sub-basins 1 through 13) and
  # the Lower Russian River (sub-basins 14 through 28)
  
  # Sub-basin 13 should flow into Sub-basin 17, 
  # if that connection is not already present
  if (which(connDF$BASIN_NUM == 13 & connDF$FLOWS_TO_NUM == 17) |> 
      length() == 0 &&
      !any("Y" %in% connDF$UPPER_RUSSIAN[connDF$BASIN_NUM > 13]) &&
      !any("N" %in% connDF$UPPER_RUSSIAN[connDF$BASIN_NUM < 14])) {
    
    # The second and third conditions add extra restrictions to make it 
    # clearer what this addition is about
    
    # In 'connDF', where the first 13 sub-basins are URR, and the remaining
    # sub-basins are LRR, connect sub-basin 13 to sub-basin 17
    connDF <- connDF |>
      bind_rows(tibble(BASIN_NUM = 13, FLOWS_TO_NUM = 17))
    
  }
  
  
  # One final check is needed before correcting the negative flows
  # Make sure there are no missing values in "BASIN_NUM" and "FLOWS_TO_NUM"
  if (anyNA(connDF$BASIN_NUM) || anyNA(connDF$FLOWS_TO_NUM)) {
    
    # Print out the problematic rows in 'connDF'
    print(connDF |> 
            filter(is.na(BASIN_NUM) | is.na(FLOWS_TO_NUM)))
    
    
    paste0("Missing Data in Sub-Basin Connectivity Matrix\n\n",
           "\"NA\" values were detected in the tibble containing sub-basin ",
           "connectivity information. The problematic row(s) are shown ",
           "above.\n\n",
           "This error is generally an indication of a script procedure error. ",
           "Please review the code and make adjustments as needed.") |>
      errWrap() |>
      stop()
    
  }
  
  
  # Finally, return 'connDF'
  return(connDF)
  
}



calcTotalAvailableFlow <- function (connDF, flowRow, startingBasin) {
  
  # Given 'flowRow', which contains flow values for each sub-basin,
  # and 'connDF', which expresses the connectivity between sub-basins,
  # determine the total available flow from 'startinBasin' and ALL of 
  # its upstream sub-basins
  
  
  # Define 'upstreamVec' to hold all upstream sub-basins
  # (starting with the one defined in 'startingBasin')
  upstreamVec <- startingBasin
  
  
  # This variable will contain the total available upstream flow
  # Start with the sum of the flow available in 'startingBasin'
  totalFlow <- flowRow[1, which(names(flowRow) == startingBasin)] |>
    sum()
  
  
  # While 'upstreamVec' still contains sub-basins
  while (length(upstreamVec) > 0) {
    
    # Identify which sub-basins are upstream of the one(s) that appear 
    # currently in 'upstreamVec'
    upstreamVec <- connDF$BASIN_NUM[which(connDF$FLOWS_TO_NUM %in% upstreamVec)]
    
    
    # Add to 'totalFlow' the flows of each of the sub-basins in 'upstreamVec'
    # (If 'upstreamVec' is empty, it will contribute "0" to 'totalFlow')
    totalFlow <- totalFlow +
      sum(flowRow[1, which(names(flowRow) %in% upstreamVec)])
    
  }
  
  
  # If 'totalFlow' is negative, return 0 as the total available upstream flow
  if (totalFlow < 0) {
    return(0)
  }
  
  
  # Otherwise, return 'totalFlow' if it's a positive number
  return(totalFlow)
  
}



# Make sure there are no missing values before proceeding
if (anyNA(combinedDF)) {
  stop("Missing data entries detected")
}


# Add back a "Date" column to 'combinedDF' and correct negative flows
combinedDF <- combinedDF |>
  mutate(Date = paste0(YEAR, "-", MONTH) |> as_date(format = "%Y-%m")) |>
  adjustNegativeFlows()


# Finally, write 'combinedDF' to a file
combinedDF |>
  select(-Date) |>
  writeOutput("Raw_Flows.csv")


# Output a completion message
print("Done!")


# Clean up
base::remove(list = ls())

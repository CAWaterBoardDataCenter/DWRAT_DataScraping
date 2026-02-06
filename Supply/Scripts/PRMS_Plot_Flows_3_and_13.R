# Create several charts for daily flows using PRMS outflow data 
# (with focus on Subbasins 3 and 13 of the Russian River watershed)

# With data up to WY2025, create bounds based on flow percentiles for each day:

# The categories are: 
#   (*) Below 10%
#   (*) 10-25% percent
#   (*) 25-75% percentile
#   (*) 75-90% percentile
#   (*) Above 90% percentile

# For Subbasin 3, release flows from Mendocino Lake will be included 
# as an area line on the top of the chart


#### Setup ####

remove(list = ls())


require(data.table)
require(tidyverse)
require(readxl)
require(cli)
require(leaflet)
require(writexl)
require(scales)


source("Scripts/HLP_001_Shared_Functions_Supply.R")


# This parameter defines the water year of interest for the charts
# (When this script was written, WY2025 was the focus)
# (Therefore, the script comments reference this water year)
# (But the script is written generically)
focusWY <- 2025


# Path to the PVP flow data spreadsheet (in CFS)
pvpFilePath <- "C:/Users/aprashar/Downloads/PVP_Estimates 1.xlsx"


# Don't use scientific notation in axis labels
options(scipen = 999)

#### Procedure ####

# Read in data from an inq file
flowDF <- makeSharePointPath("DWRAT/SDU_Runs/Hydrology/2026-01-20/PRMS/Output/PRMS_Output_RunDate_2026-01-20_FCorellasub_inq.csv") |>
  getDelim(", ")


# Keep data for Subbasins 3 and 13 only
# (Apply more useful column names as well)
flowDF <- flowDF |>
  select(Date, `3`, `13`) |>
  rename(DATE = Date,
         SUBBASIN_3 = `3`,
         SUBBASIN_13 = `13`)


# Keep only data up to the end of WY2025 ("2025-09-30")
flowDF <- flowDF |>
  filter(DATE <= paste0(focusWY, "-09-30"))


# Make sure there are no 'NA' entries (missing data in the CSV file)
if (anyNA(flowDF)) {
  
  stop("Missing data in the input file")
  
}


# There should be at least 30 years of data for this procedure too
if (nrow(flowDF) <= 30 * 365) {
  
  stop("Insufficient flow data (should be at least 30 years of daily CFS data)")
  
}


# No dates should be missing 
# Every date between the start and end of 'flowDF' should have a value
if (nrow(flowDF) != length(seq(from = min(flowDF$DATE), to = max(flowDF$DATE), by = "days"))) {
  
  stop("Missing rows (One or more days are absent from the input file)")
  
}


# Define the next part of the procedure as a function
# It will be applied to both subbasins
createChart <- function (subbasinFlows, focusWY = 2025, title = "", 
                         wetYearThreshold = 10, pvpDF = NULL,
                         monthFilter = NULL) {
  
  # Given a data frame with two columns ("DATE" and "BASIN_FLOW"),
  # generate a chart with percentiles and the WY2025 flow
  
  # (These comments mention WY2025, but the code is written generically)
  # (The highlighted water year can be changed by giving a 
  #  different value for 'focusWY')
  
  
  # If 'monthFilter' is not NULL, take a subset of 'subbasinFlows'
  # Only certain months' data will be included
  if (!is.null(monthFilter)) {
    
    subbasinFlows <- subbasinFlows |>
      filter(month(DATE) %in% monthFilter)
    
  }
  
  
  # First, get the bounds for the percentile ranges
  
  # The eventual chart categories are:
  #   (*) Below 10%
  #   (*) 10-25% percent
  #   (*) 25-75% percentile
  #   (*) 75-90% percentile
  #   (*) Above 90% percentile
  
  # Therefore, percentiles are required for 0%, 10%, 25%, 75%, 90%, and 100%
  percentileDF <- subbasinFlows |>
    mutate(MONTH_DAY = format(DATE, "%m-%d")) |>
    group_by(MONTH_DAY) |>
    summarize(PERCENTILE_0 = quantile(BASIN_FLOW, 0.00),
              PERCENTILE_10 = quantile(BASIN_FLOW, 0.10),
              PERCENTILE_25 = quantile(BASIN_FLOW, 0.25),
              PERCENTILE_75 = quantile(BASIN_FLOW, 0.75),
              PERCENTILE_90 = quantile(BASIN_FLOW, 0.90),
              PERCENTILE_100 = quantile(BASIN_FLOW, 1.00))
  
  
  # Prior years' data will also be considered in determining the average behavior of wet years
  recentYears <- data.frame(WY = (focusWY - 9):focusWY,
                            WET_YEAR = FALSE) |>
    arrange(WY)
  
  
  # Make sure all years have data available
  if (min(subbasinFlows$DATE) > paste0(min(recentYears$WY) - 1, "-10-01")) {
    
    stop("There should be at least 8 water years with data available prior to 'focusWY'")
    
  }
  
  
  # Determine which years are wet years
  for (i in 1:nrow(recentYears)) {
    
    # Calculate the percentage of this year's flows that are 
    # below the respective 25th percentiles for each month and day
    dryPercent <- subbasinFlows |>
      filter(DATE >= paste0(recentYears$WY[i] - 1, "-10-01") &
               DATE <= paste0(recentYears$WY[i], "-09-30")) |>
      mutate(MONTH_DAY = format(DATE, "%m-%d")) |>
      left_join(percentileDF,
                by = "MONTH_DAY",
                relationship = "one-to-one") |>
      mutate(BELOW_25 = BASIN_FLOW < PERCENTILE_25) |>
      summarize(PERCENTAGE = 100 * sum(BELOW_25) / n())
    
    
    # If that number is below 'wetYearThreshold', assume it is a wet year
    recentYears$WET_YEAR[i] <- dryPercent$PERCENTAGE < wetYearThreshold
    
  }
  
  
  # Filter 'recentYears' to only wet years
  recentYears <- recentYears |>
    filter(WET_YEAR == TRUE)
  
  
  # If there are fewer than 3 years in 'recentYears', output an error
  if (nrow(recentYears) < 3) {
    stop("Insufficient number of wet years in the 8 water years prior to 'focusWY'")
  }
  
  
  # Output which years are assumed to be wet years
  cat(paste0("\n\nAssuming that these are wet years:\n\t",
             paste0(recentYears$WY, collapse = "\n\t")))
  cat("\n\n")
  
  
  # Calculate the average daily flows using these water years
  averageWet <- subbasinFlows |>
    mutate(WY = if_else(month(DATE) < 10, year(DATE), year(DATE) + 1)) |>
    filter(WY %in% recentYears$WY) |>
    mutate(MONTH_DAY = format(DATE, "%m-%d")) |>
    group_by(MONTH_DAY) |>
    summarize(AVERAGE_FLOW = mean(BASIN_FLOW))
  
  
  # Get the daily flows for the water year of interest too
  wyFlows <- subbasinFlows |>
    filter(DATE >= paste0(focusWY - 1, "-10-01") &
             DATE <= paste0(focusWY, "-09-30")) |>
    mutate(MONTH_DAY = format(DATE, "%m-%d"))
  
  
  # Link 'percentileDF' to 'wyFlows' by adding "DATE" from 
  # 'wyFlows' to 'percentileDF'
  # (Also, ensure that all rows in 'percentileDF' correspond to a day in
  #  'wyFlows')
  percentileDF <- percentileDF |>
    filter(MONTH_DAY %in% wyFlows$MONTH_DAY) |>
    left_join(wyFlows |> select(MONTH_DAY, DATE),
              by = "MONTH_DAY",
              relationship = "one-to-one")
  
  
  # Do the same for 'averageWet'
  averageWet <- averageWet |>
    filter(MONTH_DAY %in% wyFlows$MONTH_DAY) |>
    left_join(wyFlows |> select(MONTH_DAY, DATE),
              by = "MONTH_DAY",
              relationship = "one-to-one")
  
  
  # Prepare the charts next
  
  
  # Define the threshold levels that will appear in the chart
  percentileLevels <-  c("Extremely High (>90th)",
                         "Above Normal (75th - 90th)",
                         "Normal (25th - 75th)", 
                         "Below Normal (10th - 25th)",
                         "Extremely Low (<10th)")
  
  
  # Also, create a label that combines the water years 
  avgWetLabel <- recentYears$WY |> sort() |>
    paste0(collapse = ", ") |>
    paste0("Average of WYs ", ... = _)
  
  
  # Define the base flow percentile portion of the chart
  baseChart <- percentileDF |>
    ggplot(mapping = aes(x = DATE)) +
    geom_ribbon(mapping = aes(ymin = PERCENTILE_90, 
                              ymax = PERCENTILE_100,
                              fill = factor(percentileLevels[1],
                                            levels = percentileLevels)), 
                alpha = 0.3) +
    geom_ribbon(mapping = aes(ymin = PERCENTILE_75, 
                              ymax = PERCENTILE_90,
                              fill = factor(percentileLevels[2],
                                            levels = percentileLevels)), 
                alpha = 0.3) +
    geom_ribbon(mapping = aes(ymin = PERCENTILE_25, 
                              ymax = PERCENTILE_75,
                              fill = factor(percentileLevels[3],
                                            levels = percentileLevels)), 
                alpha = 0.3) +
    geom_ribbon(mapping = aes(ymin = PERCENTILE_10, 
                              ymax = PERCENTILE_25,
                              fill = factor(percentileLevels[4],
                                            levels = percentileLevels)), 
                alpha = 0.3) +
    geom_ribbon(mapping = aes(ymin = PERCENTILE_0, 
                              ymax = PERCENTILE_10,
                              fill = factor(percentileLevels[5],
                                            levels = percentileLevels)), 
                alpha = 0.3) +
    #coord_cartesian(ylim = c(0, 10 + 10 * round(ceiling(max(averageWet$AVERAGE_FLOW)) / 10))) +
    labs(x = "Date (Month-Day)", y = "Flow (cfs)") +
    scale_x_date(date_labels = "%m-%d") +
    guides(fill = guide_legend(title = "Natural Flow Percentile"), 
           colour = guide_legend(title = paste0("Above Normal Water Years Since ", focusWY - 9))) +
    scale_fill_manual(values = c("#002FFF", "#72C1FF", "#00730D", "#E6A100", "#FF0000")) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_y_log10(labels = label_comma(accuracy = 0.01))
  
  
  # Add a line with the average wet year flows
  avgWetChart <- baseChart +
    geom_line(data = averageWet, mapping = aes(x = DATE, y = AVERAGE_FLOW,
                                               color = avgWetLabel), lwd = 1) +
    scale_color_manual(values = "black")
    
  
  
  # Next, produce a chart with separate lines for each wet year
  # (Rather than having the average)
  
  
  # To start, create a data frame that contains these years' data
  # To aid with the creation of separate line types and colors, 
  # all years will appear in the same rows with the same DATE labels
  # However, a "WY" column will distinguish them 
  for (i in 1:nrow(recentYears)) {
    
    # Define a temporary data frame that gives the flow volumes
    # of this iteration's water year
    tempFlows <- subbasinFlows |>
      filter(DATE >= paste0(recentYears$WY[i] - 1, "-10-01") &
               DATE <= paste0(recentYears$WY[i], "-09-30")) |>
      mutate(MONTH_DAY = format(DATE, "%m-%d")) |>
      select(-DATE) |>
      left_join(wyFlows |> select(DATE, MONTH_DAY),
                by = "MONTH_DAY", relationship = "one-to-one") |>
      filter(!is.na(DATE)) |>
      mutate(WY = paste0("WY ", recentYears$WY[i]))
    
    
    if (i == 1) {
      combinedFlows <- tempFlows
    } else {
      combinedFlows <- bind_rows(combinedFlows, tempFlows)
    }
    
    
  }
  
  
  
  allWetYearChart <- baseChart +
    geom_line(data = combinedFlows, 
              mapping = aes(x = DATE, y = BASIN_FLOW, group = WY, color = WY, linetype = WY), lwd = 0.8) +
    scale_linetype_manual(values = rep(c(1:2, 6), 8)[1:nrow(recentYears)], name = paste0("Above Normal Water Years Since ", focusWY - 9)) +
    scale_color_manual(values = colorQuantile(colorRamp(c("#000000", "#909090"),
                                                        interpolate = "spline"),
                                              recentYears$WY,
                                              n = nrow(recentYears))(recentYears$WY),
                       name = paste0("Above Normal Water Years Since ", focusWY - 9))
  
  
  
  # For Subbasin 3 (downstream of Lake Mendocino), add PVP flows to the chart
  if (!is.null(pvpDF)) {
    
    # Read in the PVP flow information for the 
    pvpAvgs <- pvpDF |>
      mutate(MONTH_DAY = format(DATE, "%m-%d")) |>
      group_by(MONTH_DAY) |>
      summarize(AVG_PVP_CFS = mean(PVP_CFS)) |>
      left_join(wyFlows, by = "MONTH_DAY", relationship = "one-to-one") |>
      filter(!is.na(DATE))
    
    
    # Append this information to both charts
    
    avgWetChart <- avgWetChart +
      geom_line(data = pvpAvgs, mapping = aes(x = DATE, y = 10^(log10(max(percentileDF$PERCENTILE_100)) - (log10(AVG_PVP_CFS) - log10(min(percentileDF$PERCENTILE_0)))), lwd = paste0("Historic ", floor(time_length(max(pvpDF$DATE) - min(pvpDF$DATE), unit = "years")), "-Year Average")), color = "#D35FB7", alpha = 0.25) +
      #geom_col(data = pvpAvgs, mapping = aes(x = DATE, y = 10^(log10(max(percentileDF$PERCENTILE_100)) - (log10(AVG_PVP_CFS) - log10(min(percentileDF$PERCENTILE_0))))), alpha = 0.1, fill = "#D35FB7") +
      #geom_col(data = pvpAvgs, mapping = aes(x = DATE, y = max(percentileDF$PERCENTILE_100)), alpha = 0.1, fill = "#D35FB7") +
      scale_linewidth_manual(values = 1.2) + 
      guides(linewidth = guide_legend("PVP Flows")) + 
      #geom_ribbon(data = pvpAvgs,
      #            mapping = aes(x = DATE, ymin = 10^(log10(max(percentileDF$PERCENTILE_100)) - (log10(AVG_PVP_CFS) - log10(min(percentileDF$PERCENTILE_0)))),
      #                          ymax = max(percentileDF$PERCENTILE_100)),
      #            alpha = 0.2, fill = "black") +
      scale_y_log10(labels = label_comma(accuracy = 0.01),
                    sec.axis = sec_axis(~ rev(.), name = "Average Daily PVP Flows (cfs)", labels = label_comma(accuracy = 0.01))) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    
    
    allWetYearChart <- allWetYearChart +
      geom_line(data = pvpAvgs, mapping = aes(x = DATE, y = 10^(log10(max(percentileDF$PERCENTILE_100)) - (log10(AVG_PVP_CFS) - log10(min(percentileDF$PERCENTILE_0)))), lwd = paste0("Historic ", floor(time_length(max(pvpDF$DATE) - min(pvpDF$DATE), unit = "years")), "-Year Average")), color = "#D35FB7", alpha = 0.25) +
      #geom_col(data = pvpAvgs, mapping = aes(x = DATE, y = 10^(log10(max(percentileDF$PERCENTILE_100)) - (log10(AVG_PVP_CFS) - log10(min(percentileDF$PERCENTILE_0))))), alpha = 0.1, fill = "#D35FB7") +
      #geom_col(data = pvpAvgs, mapping = aes(x = DATE, y = max(percentileDF$PERCENTILE_100)), alpha = 0.1, fill = "#D35FB7") +
      scale_linewidth_manual(values = 1.2) + 
      guides(linewidth = guide_legend("PVP Flows")) + 
      #geom_ribbon(data = pvpAvgs,
      #            mapping = aes(x = DATE, ymin = 10^(log10(max(percentileDF$PERCENTILE_100)) - (log10(AVG_PVP_CFS) - log10(min(percentileDF$PERCENTILE_0)))),
      #                          ymax = max(percentileDF$PERCENTILE_100)),
      #            alpha = 0.2, fill = "black") +
      scale_y_log10(labels = label_comma(accuracy = 0.01),
                    sec.axis = sec_axis(~ rev(.), name = "Average Daily PVP Flows (cfs)", labels = label_comma(accuracy = 0.01))) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    
    
    
  }
  
  
  
  
  # Save the plots
  ggsave(paste0("Avg_Wet_Year_", title |> str_replace_all("\\s", "_"), "_WY", focusWY, ".png"), 
         plot = avgWetChart, width = 1080 * 3.2 * 2, height = 720 * 2.2 * 2, units = "px", dpi = 600)
  
  
  ggsave(paste0("All_Wet_Year_", title |> str_replace_all("\\s", "_"), "_WY", focusWY, ".png"), 
         plot = allWetYearChart, width = 1080 * 3.2 * 2, height = 720 * 2.2 * 2, units = "px", dpi = 600)
  

  
  # Output all data as a spreadsheet too
  writeList <- list("FLOW" = bind_rows(combinedFlows,
                                       averageWet |>
                                         rename(BASIN_FLOW = AVERAGE_FLOW) |>
                                         mutate(WY = "Average of Above Normal Years")),
                    "THRESHOLDS" = percentileDF)
  
  if (!is.null(pvpDF)) {
    
    writeList[[3]] <- pvpAvgs
    
    names(writeList)[3] <- "PVP"
    
  }
  
  write_xlsx(writeList,
             paste0(title |> str_replace_all("\\s", "_"), 
                    "_Data_Output_", focusWY, ".xlsx"))
  
}




# For Subbasin 3, include PVP information in the chart

# Read in the PVP flow information from the spreadsheet
# Summarize the data 
pvpDF <- read_xlsx(pvpFilePath) |>
  select(Date, `Mean CFS (Final)`) |>
  rename(DATE = Date,
         PVP_CFS = `Mean CFS (Final)`) |>
  mutate(DATE = ymd(DATE)) |>
  filter(DATE %in% flowDF$DATE) |>
  filter(!is.na(PVP_CFS))


# There should be a value for every date in 'flowDF'
if (nrow(pvpDF) != nrow(flowDF)) {
  
  stop("Insufficient PVP Flows data provided (need a value for every date in the range)")
  
}



flowDF |>
  select(DATE, SUBBASIN_3) |>
  rename(BASIN_FLOW = SUBBASIN_3) |>
  createChart(focusWY, title = "Calpella (Downstream of Mendocino Lake)", 
              wetYearThreshold = 8, pvpDF = pvpDF,
              monthFilter = c(12, 1, 2, 3))



flowDF |>
  select(DATE, SUBBASIN_13) |>
  rename(BASIN_FLOW = SUBBASIN_13) |>
  createChart(focusWY, title = "Healdsburg",
              wetYearThreshold = 10, pvpDF = pvpDF,
              monthFilter = c(12, 1, 2, 3))
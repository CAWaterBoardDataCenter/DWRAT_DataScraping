library(tidyverse)
library(readxl)
library(openxlsx)


# Read in the Expected Demand spreadsheet
expDemand <- read_xlsx("OutputData/ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx")



# Assign the proper column headers to the tibble
expDemand <- expDemand[-c(1:2), ] %>%
  set_names(expDemand[2, ] %>% unlist() %>% as.vector())



# Narrow down 'expDemand' to the table with monthly values
expDemand <- expDemand[, seq(from = str_which(names(expDemand), "COUNT"),
                             to = str_which(names(expDemand), "CALENDAR_YEAR_TOTAL"))]


# Keep only three columns in 'expDemand' ("APPLICATION_NUMBER", "YEAR", and "CALENDAR_YEAR_TOTAL")
# (Also, convert "CALENDAR_YEAR_TOTAL" into a numeric column)
expDemand <- expDemand %>% select(APPLICATION_NUMBER, YEAR, CALENDAR_YEAR_TOTAL) %>%
  mutate(CALENDAR_YEAR_TOTAL = as.numeric(CALENDAR_YEAR_TOTAL))



# Create a summary tibble with median values of "CALENDAR_YEAR_TOTAL" for each "APPLICATION_NUMBER"
medVals <- expDemand %>%
  group_by(APPLICATION_NUMBER) %>%
  summarize(MEDIAN_TOTAL_AF = median(CALENDAR_YEAR_TOTAL, na.rm = TRUE),
            Q1_TOTAL_AF = quantile(CALENDAR_YEAR_TOTAL, na.rm = TRUE)[2],
            IQR_TOTAL_AF = IQR(CALENDAR_YEAR_TOTAL, na.rm = TRUE),
            Q3_TOTAL_AF = quantile(CALENDAR_YEAR_TOTAL, na.rm = TRUE)[4],
            AVG_TOTAL_AF = mean(CALENDAR_YEAR_TOTAL, na.rm = TRUE),
            SD_TOTAL_AF = sd(CALENDAR_YEAR_TOTAL, na.rm = TRUE))



# Append these median values back to 'expDemand'
expDemand <- expDemand %>%
  left_join(medVals, by = "APPLICATION_NUMBER", relationship = "many-to-one")



# Calculate ratios between "CALENDAR_YEAR_TOTAL" and "MEDIAN_TOTAL_AF"
# Keep records that are more than two orders of magnitude away from the median (in either direction)
expDemand %>%
  filter((MEDIAN_TOTAL_AF > 0 & CALENDAR_YEAR_TOTAL / MEDIAN_TOTAL_AF > 100) |
           (MEDIAN_TOTAL_AF > 0 & CALENDAR_YEAR_TOTAL > 0 & CALENDAR_YEAR_TOTAL / MEDIAN_TOTAL_AF < 1/100)) %>%
  select(APPLICATION_NUMBER, YEAR, CALENDAR_YEAR_TOTAL, MEDIAN_TOTAL_AF) %>%
  write.xlsx("OutputData/Expected_Demand_Units_QAQC_Median_Based.xlsx", overwrite = TRUE)



# As an alternative method, use two standard statistical definitions for outliers
# Outliers are usually identified by being:
#   (1) More than 2 standard deviations away from the mean
#   (2) Outside of the range defined by the IQR and first/third quartiles [Q1 - 1.5 IQR, Q3 + 1.5 IQR]
# expDemand %>%
#   mutate(OUTLIER_BOUND_L_MEDIAN = Q1_TOTAL_AF - 1.5 * IQR_TOTAL_AF,
#          OUTLIER_BOUND_R_MEDIAN = Q3_TOTAL_AF + 1.5 * IQR_TOTAL_AF,
#          OUTLIER_BOUND_L_MEAN = AVG_TOTAL_AF - 2 * SD_TOTAL_AF,
#          OUTLIER_BOUND_R_MEAN = AVG_TOTAL_AF + 2 * SD_TOTAL_AF) %>%
#   filter(CALENDAR_YEAR_TOTAL > 0) %>%
#   filter(CALENDAR_YEAR_TOTAL < OUTLIER_BOUND_L_MEDIAN | 
#            CALENDAR_YEAR_TOTAL < OUTLIER_BOUND_L_MEAN |
#            CALENDAR_YEAR_TOTAL > OUTLIER_BOUND_R_MEDIAN | 
#            CALENDAR_YEAR_TOTAL > OUTLIER_BOUND_R_MEAN) %>%
#   write.xlsx("OutputData/Expected_Demand_Units_QAQC_Statistical_Outliers.xlsx", overwrite = TRUE)
# 



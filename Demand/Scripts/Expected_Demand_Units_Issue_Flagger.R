require(tidyverse)
require(readxl)
require(openxlsx)


print("Starting 'Expected_Demand_Units_Issue_Flagger.R'...")


# Read in the Expected Demand spreadsheet
expDemand <- read_xlsx(paste0("OutputData/", ws$ID, "_ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx"))



# Assign the proper column headers to the tibble
# expDemand <- expDemand[-c(1:2), ] %>%
#   set_names(expDemand[2, ] %>% unlist() %>% as.vector())



# Narrow down 'expDemand' to the table with monthly values
# expDemand <- expDemand[, seq(from = str_which(names(expDemand), "COUNT"),
#                              to = str_which(names(expDemand), "CALENDAR_YEAR_TOTAL"))]


# Keep only three columns in 'expDemand' ("APPLICATION_NUMBER", "YEAR", and "CALENDAR_YEAR_TOTAL")
# (Also, convert "CALENDAR_YEAR_TOTAL" into a numeric column)
expDemand <- expDemand %>% group_by(APPLICATION_NUMBER, YEAR) %>%
  summarize(CALENDAR_YEAR_TOTAL = sum(JAN_DIRECT_DIVERSION, JAN_STORAGE_DIVERSION,
                                      FEB_DIRECT_DIVERSION, FEB_STORAGE_DIVERSION,
                                      MAR_DIRECT_DIVERSION, MAR_STORAGE_DIVERSION,
                                      APR_DIRECT_DIVERSION, APR_STORAGE_DIVERSION,
                                      MAY_DIRECT_DIVERSION, MAY_STORAGE_DIVERSION,
                                      JUN_DIRECT_DIVERSION, JUN_STORAGE_DIVERSION,
                                      JUL_DIRECT_DIVERSION, JUL_STORAGE_DIVERSION,
                                      AUG_DIRECT_DIVERSION, AUG_STORAGE_DIVERSION,
                                      SEP_DIRECT_DIVERSION, SEP_STORAGE_DIVERSION,
                                      OCT_DIRECT_DIVERSION, OCT_STORAGE_DIVERSION,
                                      NOV_DIRECT_DIVERSION, NOV_STORAGE_DIVERSION,
                                      DEC_DIRECT_DIVERSION, DEC_STORAGE_DIVERSION, 
                                      na.rm = TRUE), .groups = "drop") #%>%
#  select(APPLICATION_NUMBER, YEAR, CALENDAR_YEAR_TOTAL) %>%
#  mutate(CALENDAR_YEAR_TOTAL = as.numeric(CALENDAR_YEAR_TOTAL))



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
expDemand <- expDemand %>%
  filter((MEDIAN_TOTAL_AF > 0 & CALENDAR_YEAR_TOTAL / MEDIAN_TOTAL_AF > 100) |
           (MEDIAN_TOTAL_AF > 0 & CALENDAR_YEAR_TOTAL > 0 & CALENDAR_YEAR_TOTAL / MEDIAN_TOTAL_AF < 1/100)) %>%
  select(APPLICATION_NUMBER, YEAR, CALENDAR_YEAR_TOTAL, MEDIAN_TOTAL_AF) 



# Read in the other Units QA/QC spreadsheet and remove rows that are already present in that spreadsheet
mainSheet <- list.files("OutputData", 
                        pattern = paste0(ws$ID[1], "_Expected_Demand_Units_QAQC.xlsx$"), 
                        full.names = TRUE) %>%
  read_xlsx() %>%
  mutate(APP_YEAR_KEY = paste0(APPLICATION_NUMBER, "|", YEAR))



# Remove the rows present in 'mainSheet' from 'expDemand'
expDemand <- expDemand %>%
  mutate(APP_YEAR_KEY = paste0(APPLICATION_NUMBER, "|", YEAR)) %>%
  filter(!(APP_YEAR_KEY %in% mainSheet$APP_YEAR_KEY)) %>%
  select(-APP_YEAR_KEY)
  


# Similarly, remove the rows from a previous version of this review sheet, if it exists
if (!is.na(ws$QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH)) {
  
  reviewDF <- getXLSX(ws, "IS_SHAREPOINT_PATH_QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET",
                      "QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH",
                      "QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_WORKSHEET_NAME") %>%
    select(APPLICATION_NUMBER, YEAR) %>%
    mutate(KEY = paste0(APPLICATION_NUMBER, "_", YEAR))

  
  
  # Remove those already-reviewed rows from 'expDemand'
  expDemand <- expDemand %>%
    mutate(KEY = paste0(APPLICATION_NUMBER, "_", YEAR)) %>%
    filter(!(KEY %in% reviewDF$KEY)) %>%
    select(-KEY)
  
  
  
  remove(reviewDF)
  
}



# Check the other review sheet too, if it exists
if (!is.na(ws$QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH)) {
  
  reviewDF <- getXLSX(ws, "IS_SHAREPOINT_PATH_QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET",
                      "QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH",
                      "QAQC_UNIT_CONVERSION_ERRORS_WORKSHEET_NAME") %>%
    select(APPLICATION_NUMBER, YEAR) %>%
    mutate(KEY = paste0(APPLICATION_NUMBER, "_", YEAR))
  
  
  
  # Remove those already-reviewed rows from 'expDemand'
  expDemand <- expDemand %>%
    mutate(KEY = paste0(APPLICATION_NUMBER, "_", YEAR)) %>%
    filter(!(KEY %in% reviewDF$KEY)) %>%
    select(-KEY)
  
  
  
  remove(reviewDF)
  
}



# As a final step, if an APPLICATION_NUMBER value is flagged in both 'expDemand' and 'mainSheet',
# move all rows of that right into 'expDemand'
if (sum(expDemand$APPLICATION_NUMBER %in% mainSheet$APPLICATION_NUMBER) > 0) {
  
  # Extract rows from 'mainSheet' (by filtering to rows whose APPLICATION_NUMBER values appear in 'expDemand')
  # Append them to 'expDemand' and then sort the tibble
  expDemand <- mainSheet %>%
    filter(APPLICATION_NUMBER %in% expDemand$APPLICATION_NUMBER) %>%
    select(APPLICATION_NUMBER, YEAR, CALENDAR_YEAR_TOTAL) %>%
    bind_rows(expDemand) %>%
    arrange(APPLICATION_NUMBER, YEAR)
  
  
  # Filter down 'mainSheet' (removing those rows)
  # Then, overwrite the review spreadsheet for it
  mainSheet %>%
    filter(!(APPLICATION_NUMBER %in% expDemand$APPLICATION_NUMBER)) %>%
    write.xlsx(paste0("OutputData/", ws$ID[1], "_Expected_Demand_Units_QAQC.xlsx"))
  
}



# Write 'expDemand' to a spreadsheet
write.xlsx(expDemand,
           paste0("OutputData/", ws$ID, "_Expected_Demand_Units_QAQC_Median_Based.xlsx"), overwrite = TRUE)



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



cat("Done!\n")

remove(expDemand, medVals, mainSheet)
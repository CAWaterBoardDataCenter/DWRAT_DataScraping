library(readxl)
library(tidyr)
library(dplyr)

# Read the Excel file
df <- read.csv("S:/DWR/VOL1/WQC-PT/SDU/PowerBI Data & Dashboards/Mattole River/MT_2017_2023_DemandDataset_MonthlyValues.csv")

# create target dataframe and increase resolution
df_long <- df[, c("APPLICATION_NUMBER","YEAR")] %>%
  expand(APPLICATION_NUMBER, YEAR, MONTH = 1:12) %>%
  tidyr::uncount(2, .id = "TYPE") %>%
  mutate(TYPE = ifelse(row_number() %% 2 == 1, "DIRECT", "STORAGE"))

df_long$DIVERSION <- NA
month_names <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

# For loop to populate DIVERSION
for(i in 1:nrow(df_long)) {
  id <- df_long$APPLICATION_NUMBER[i]
  year <- df_long$YEAR[i]
  month <- df_long$MONTH[i]
  month_abbreviation <- month_names[month]
  type <- df_long$TYPE[i]
  
  # Define the column name in the horizontal dataframe to pull from
  column_name <- paste0(month_abbreviation, "_", type, "_DIVERSION")
  
  # Get the diversion value from the horizontal dataframe
  diversion_value <- df[df$APPLICATION_NUMBER == id & df$YEAR == year, column_name]
  
  if (length(diversion_value) > 0 && !is.na(diversion_value)) {
    df_long$DIVERSION[i] <- diversion_value
  } else {
    df_long$DIVERSION[i] <- 0  # Or any default value you want in case of missing data
  }
}
  
# wait a hot minute, its 350k rows
# and export
write.csv(df_long, "TD_Monthly_Demand.csv", row.names = FALSE)

# Constructing a new SRP Dat File
  # Run the first portion of Master_Script_PRMS.R where you define StartDate, EndDate, and includeForecast,
    # through line 52
  # and you download the SRP raw data
  # Import Pre-2023 WY SRP DAT file
  # Import SPI WY 2023-2024 SRP DAT File
  # Import SRP_Processed.csv

# Have some logic that overrides overlapping portion of SPI file with observed data if observed data exists. 
# Never use SPI data if you have observed data available.
# combine the 3 datasets (Pre-2023 WY SRP DAT File, SPI data, and observed data) to generate final SRP DAT file
# for a specific month
# Save the SRP_Dat file for a specific month to the ProcessedData folder with a timestamp. The timestamp is the EndDate;
# EndDate is the last day of the observed data range. 

# Import the precursor files----

# Import Pre-2023 WY SRP CSV file
SRP_Blueprints_Path = makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT SRP Blueprints\\")

Pre2023_SRP = read.csv(file = paste0(SRP_Blueprints_Path, "DAT_SRP_1947_to_WY2023.csv"))

#Convert Date field from character to date format 
Pre2023_SRP$Date = as.Date(Pre2023_SRP$Date, format = "%Y-%m-%d")

# Import SPI WY 2023-2024 SRP CSV file
SPI_Forecast_SRP = read.csv(paste0(SRP_Blueprints_Path, "SPI_SRP_WY_2023_2024.csv"))

# Convert 1st 6 columns to integer data type to match Pre2023_SRP
SPI_Forecast_SRP = SPI_Forecast_SRP %>% 
  mutate_at(
    .vars = vars(1:6), #selects the 1st 6 columns
    .funs = as.integer #converts the selected columns to integers
  )

# Convert Date field from character to date format
SPI_Forecast_SRP$Date = as.Date(SPI_Forecast_SRP$Date, format = "%Y-%m-%d")

# Import Processed SRP data
SRP_Processed = read.csv(file = "ProcessedData/SRP_Processed.csv")

#Rename the columns in SRP_Processed
SRP_Processed_names = c("Date","precip01","tmin01","tmax01", "precip02", "tmin02", "tmax02")
colnames(SRP_Processed) = SRP_Processed_names

# Convert Date field from character to date format
SRP_Processed$Date = as.Date(SRP_Processed$Date, format = "%Y-%m-%d")

# Rearrange SRP_Processed column order to match the other 2 dataframes
SRP_Processed <- SRP_Processed %>%
  select(Date, precip01, precip02, tmax01, tmax02, tmin01, tmin02)

# Add the year, month, day, hour, min, sec columns as the first 6 columns in SRP_Processed
SRP_Processed <- SRP_Processed %>%
  mutate(year = as.integer(format(Date, "%Y")),
         month = as.integer(format(Date, "%m")),
         day = as.integer(format(Date, "%d")),
         hour =as.integer(0),
         min = as.integer(0),
         sec = as.integer(0)) %>%
  select(year, month, day, hour, min, sec, everything())


# Error Check for Pre2023_SRP and SRP_Preprocessed----
# Perform an error check to ensure that no rows from SRP_Processed overlap with Pre-2023_SRP
if (Pre2023_SRP %>% filter(Date %in% SRP_Processed$Date) %>% nrow() > 0) {
  
  print(c("The scraped SRP meteorological dataset contains rows for dates that appear in the Pre2023_SRP dat file.", 
          "The data for those dates in 'PRe2023_SRP' will be replaced with the data in the meteorological dataset."))
  
  # Remove those rows in 'DAT_Initial'
  Pre2023_SRP <- Pre2023_SRP %>%
    filter(!(Date %in% Pre2023_SRP$Date))

  #bcheck for continuity of dataset--the earliest date in SRP_Processed should be 1 day after the
  # latest date in Pre2023_SRP
} else if (as.Date(min(SRP_Processed$Date), "%Y-%m-%d") != as.Date(max(Pre2023_SRP$Date) + 1, "%Y-%m-%d")) {
  print("The earliest date in SRP_Processed is not exactly 1 day after the latest date in Pre2023_SRP.")
  
} else {
print("No errors exist because SRP_Preprocessed and Pre2023_SRP have no overlapping records AND no date gap.")
}
  
# Merge Pre2023_SRP and SRP_Pre-processed and arrange in ascending order by Date
Dat_SRP_Merged = bind_rows(Pre2023_SRP, SRP_Processed) %>%
  arrange(Date)

# Error check for SRP_Preprocessed and SPI_Forecast_SRP
if (SPI_Forecast_SRP %>% filter(Date  %in% SRP_Processed$Date) %>% nrow() >0) {
  
  print("The scraped SRP meteorological dataset contains rows for dates that appear 
          in the SPI_Forecast_SRP dat file.")
  
} else { 
  print("No errors exist because SPI_Forecast_SRP and Pre2023_SRP have no overlapping records")
    
}

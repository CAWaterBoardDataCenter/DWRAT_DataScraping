library(here)
default_folder = here("DWRAT_DataScraping3")
#load dat_shell dataframe, RAW_processed dataset, and PRISM dataset
Stations = read.csv(here("InputData/Raws_Stations.csv"))
df1<-read.csv(here("InputData/Dat_shell.csv"))
df2<-read.csv(here("ProcessedData/RAWS_Processed.csv"))
df3<-read.csv(here("ProcessedData/PRISM_Processed.csv"))

#find the unique station names
station_names <- unique(df1$station_name)

# Merge the two data frames based on the station name column
merged_data <- merge(df1, df2, by = "station_name")

# Loop through the rows and columns of the merged data frame and add the values
for (i in 1:nrow(merged_data)) {
  for (j in 2:ncol(merged_data)) {  # skip the station name column
    if (!is.na(merged_data[i, j])) {
      df[merged_data[i, 1], colnames(df)[j]] <- df[merged_data[i, 1], colnames(df)[j]] + merged_data[i, j]
    }
  }
}
 
# Replace missing values and -999 with values from df3
for (i in 1:nrow(df1)) {
  for (j in 2:ncol(df1)) {  # skip the first column (assuming it contains station names)
    if (is.na(df1[i, j]) || df1[i, j] == -999) {
      df1[i, j] <- ifelse(df2[i, j] != -999, df2[i, j], df3[i, j])
    }
  }
}

# Save the updated data frame to a new CSV file
write.csv(df1, file = 'path/to/updated_data.csv', row.names = FALSE)
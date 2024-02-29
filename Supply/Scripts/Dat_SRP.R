#Load Packages
require(dplyr)
require(readr)
require(here)

# Import the existing SRP Stuff
Dat_SRP_header <- read.delim(file = "InputData/Dat_SRP_header.dat", header = F)
Dat_SRP_Body= read.csv(file = "InputData/SRP_Updater.csv")

#Convert date column in Dat_SRP_Body to yyyy-mm-dd date format
Dat_SRP_Body$date = as.Date(x = Dat_SRP_Body$date,  format = "%m/%d/%Y")

#Whittle Dat_SRP_Body to the observed time range

# Import Processed SRP data
SRP_Processed = read.csv(file = "ProcessedData/SRP_Processed.csv")

# Rename Date to date
SRP_Processed = rename(SRP_Processed, date = Date)
head(SRP_Processed)

# Convert SRP_Processed$date to date format
SRP_Processed$date = as.Date(x = SRP_Processed$date,  format = "%Y-%m-%d")
str(SRP_Processed)

# Update the column names in SRP_Processed to match Dat_SRP_Body
SRP_Processed_names = c("date","precip01","tmin01","tmax01", "precip02", "tmin02", "tmax02")
colnames(SRP_Processed) = SRP_Processed_names
colnames(SRP_Processed)

# Whittle Dat_SRP_Body to our timeframe of interest

Dat_SRP_Shell <- Dat_SRP_Body %>%
  filter(date >= StartDate$date & date <= EndDate$date)

#Remove extra columns from Dat_SRP_Shell
Dat_SRP_Shell2 =  Dat_SRP_Shell[,-c(1:6, 14)]
Dat_SRP_Shell2


#Relocate date column to the 1st position of DAT_SRP_Shell
Dat_SRP_Shell3 <- Dat_SRP_Shell2 %>% relocate(date, .before = precip01) %>%
  arrange(date)
head(Dat_SRP_Shell3)

#Inner Join Dat_SRP_Shell3 to SRP_Processed
Dat_SRP_Shell4 = inner_join(x = SRP_Processed, 
                            y = select(Dat_SRP_Shell3, date), 
                            by = "date")

Dat_SRP_Shell4

# Restore the first 6 columns of Dat_SRP_Body (datetime fieldS)
Dat_SRP_Shell5 <- cbind(Dat_SRP_Shell[1:6], Dat_SRP_Shell4)
Dat_SRP_Shell5

# Remove the timeframe of interest from the old DAT_SRP_Body dataframe
Dat_SRP_Final = anti_join(x = Dat_SRP_Body, 
                          y = SRP_Processed,
                          by = "date")


# Combine the metereological data with the old SRP data----

# bind_row adds the dates that we removed in the previous step but this time those
# dates have the meteorological data that we have downloaded; this completes the data
# substitution for the PRMS Dat file
Dat_SRP_Final <- bind_rows(Dat_SRP_Final, Dat_SRP_Shell5)
Dat_SRP_Final

#Arrange by date in ascending order
Dat_SRP_Final = Dat_SRP_Final %>% arrange(date)

#Remove date column from Dat_SRP_Final
Dat_SRP_Final$date = NULL

# Ensure all numeric columns have at least 4 decimal places
Dat_SRP_Final[, c("precip01", "precip02", "tmax01", "tmax02", "tmin01", "tmin02")] <- 
  format(Dat_SRP_Final[, c("precip01", "precip02", "tmax01", "tmax02", "tmin01", "tmin02")], nsmall = 4)

# Save the spacing between columns required by the SRP Dat file into a vector
spacing_vector <- c(" ", "  ", "  ", "  ", " ", "    ", "     ", "    ", "    ", "    ", "    ", "    ")

# Get the first 12 column indices of Dat_SRP_final
all_column_indices <- seq_along(Dat_SRP_Final)[1:12]

# Create a list with concatenated columns
concatenated_columns <- lapply(all_column_indices, function(i) {
  paste(Dat_SRP_Final[[i]], spacing_vector[i])
})

# Unite all columns into a new column
Dat_SRP_Final$Concatenated_Column <- do.call(paste, c(concatenated_columns, sep = ""))

#Remove columns 1-12 from Dat_SRP_Final
Dat_SRP_Final = Dat_SRP_Final[, -c(1:12)] %>% as.data.frame()
Dat_SRP_Final
names(Dat_SRP_Final) = "SRP"
names(Dat_SRP_header) = "SRP"

#Combine Dat_SRP_Final with Dat_SRP_Header
Dat_SRP_Final = rbind(Dat_SRP_header, Dat_SRP_Final)

# Export Dat_SRP_Final----
# Export Dat_SRP_Final to ProcessedData folder
write.table(x = Dat_SRP_Final, 
            file = paste0("ProcessedData/Dat_SRP_Final_Forecast_", End_Date, ".dat"),
            sep = "/t", row.names =  F, quote =  F, col.names = F)

# Export Dat_SRP_Final to SRP Model folder
# SRP_Path = "C:\\SRPHM_update_ag\\"
# write.table(x = Dat_SRP_Final, file = paste0(SRP_Path, "Dat_SRP_Final_Forecast_End_Date_", End_Date, ".dat"),
#             sep = "\t", row.names = F, quote = F, col.names = F)


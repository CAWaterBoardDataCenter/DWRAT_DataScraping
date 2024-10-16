#Install and load libraries----
library(dplyr)
library(tidyverse)
library(here)
library(lubridate) #for make_date function
library(data.table) #for fread function

# Rely on the shared functions from the Demand scripts
source("../Demand/Scripts/Shared_Functions_Demand.R")

# Import Corrected QAQC records and pre-QAQC Dat PRMS File----
Dat_PRMS_Original = read.csv("ProcessedData/DAT_PRMS_QAQC_Flags_2024-06-26.csv")
Corrections = read.csv("ProcessedData/Corrected_Records.csv")

Corrections = Corrections %>% select(1:17)

# Create the Dat_PRMS_Updated dataframe----
# Replaces incorrect values in original Dat PRMS with corrected values by 
# looking at the Date; should replace 43 records, rest should be untouched
Dat_PRMS_Updated <- Dat_PRMS_Original %>%
  left_join(Corrections, by = "Date", suffix = c("", "_Correction")) %>%
  mutate(
    NOAA_TMAX1 = coalesce(NOAA_TMAX1_Correction, NOAA_TMAX1),
    NOAA_TMAX2 = coalesce(NOAA_TMAX2_Correction, NOAA_TMAX2),
    CIMIS_TMAX3 = coalesce(CIMIS_TMAX3_Correction, CIMIS_TMAX3),
    CIMIS_TMAX4 = coalesce(CIMIS_TMAX4_Correction, CIMIS_TMAX4),
    RAWS_TMAX5 = coalesce(RAWS_TMAX5_Correction, RAWS_TMAX5),
    NOAA_TMAX6 = coalesce(NOAA_TMAX6_Correction, NOAA_TMAX6),
    RAWS_TMAX7 = coalesce(RAWS_TMAX7_Correction, RAWS_TMAX7),
    RAWS_TMAX8 = coalesce(RAWS_TMAX8_Correction, RAWS_TMAX8),
    NOAA_TMIN1 = coalesce(NOAA_TMIN1_Correction, NOAA_TMIN1),
    NOAA_TMIN2 = coalesce(NOAA_TMIN2_Correction, NOAA_TMIN2),
    CIMIS_TMIN3 = coalesce(CIMIS_TMIN3_Correction, CIMIS_TMIN3),
    CIMIS_TMIN4 = coalesce(CIMIS_TMIN4_Correction, CIMIS_TMIN4),
    RAWS_TMIN5 = coalesce(RAWS_TMIN5_Correction, RAWS_TMIN5),
    NOAA_TMIN6 = coalesce(NOAA_TMIN6_Correction, NOAA_TMIN6),
    RAWS_TMIN7 = coalesce(RAWS_TMIN7_Correction, RAWS_TMIN7),
    RAWS_TMIN8 = coalesce(RAWS_TMIN8_Correction, RAWS_TMIN8)
  ) %>%
  select(-ends_with("_Correction"))


# Rely on the shared functions from the Demand scripts
source("../Demand/Scripts/Shared_Functions_Demand.R")
DAT_Metadata <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_Metadata.dat") %>%
  read_lines()

# Temporarily write 'DAT_Merged' to a file
Dat_PRMS_Updated %>%
  select(-Date) %>%
  write_delim(paste0("ProcessedData/Dat_PRMS_Observed_EndDate_2024-06-06_Palemi.dat"),
              delim = "\t", col_names = FALSE)


# Read back in this file
# Then, append 'DAT_Metadata' to the beginning
DAT_Merged_Tab <- c(DAT_Metadata,
                    read_lines(paste0("ProcessedData/Dat_PRMS_Observed_EndDate_2024-06-06_Palemi.dat")))



# Double-check that the same number of tabs appears in every row of the vector
stopifnot(DAT_Merged_Tab %>% str_count("\t") %>% unique() %>% length() == 1)
stopifnot(DAT_Merged_Tab %>% str_count("\t") %>% unique() == 58)



# Write this vector to a file
write.table(DAT_Merged_Tab,
            paste0("ProcessedData/Dat_PRMS_Observed_EndDate_2024-06-06_PAlemi.dat"),
            sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)

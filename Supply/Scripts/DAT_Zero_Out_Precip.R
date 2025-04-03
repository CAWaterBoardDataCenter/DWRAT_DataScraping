# For a specified date range, set all precipitation values to zero



require(tidyverse)


source("../Demand/Scripts/Shared_Functions_Demand.R")



#### Inputs ####


# Location of the DAT file
filePath <- "ProcessedData/Dat_PRMS_Observed_EndDate_2024-12-31.dat"


# Date range for when precipitation data should be zeroed out
startDay <- "2008-03-01"
endDay <- "2008-05-31"


#### Procedure ####


print("Starting 'DAT_Zero_Out_Precip.R'...")



# Read the DAT file
datLines <- read_lines(filePath)



# Make a copy of it that is a tibble
# Get headers from a SharePoint file
# Filter the dates using 'startDay' and 'endDay'
datDF <- datLines %>%
  str_split("\t") %>% unlist() %>%
  matrix(nrow = length(datLines), byrow = TRUE) %>%
  data.frame() %>% tibble() %>%
  filter(!grepl("[A-Za-z#]", X1)) %>%
  set_names(read_lines(makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\DAT PRMS Blueprints\\Dat_Headers.txt"))) %>%
  mutate(DATE = paste(Year, month, day, sep = "-") %>% as.Date(format = "%Y-%m-%d")) %>%
  filter(DATE >= startDay & DATE <= endDay) %>%
  select(-DATE)



# Identify the precipitation columns in 'datDF'
precipCols <- which(grepl("PRECIP", names(datDF)))



# Zero them out
datDF[, precipCols] <- 0



# Update 'datLines' based on these changes to 'datDF'
for (i in 1:nrow(datDF)) {
  
  datLines[grep(paste0("^", datDF$Year[i], "\t", datDF$month[i], "\t", datDF$day[i], "\t"), datLines)] <- datDF[i, ] %>% unlist() %>% as.vector() %>% paste0(collapse = "\t")
  
}



# Save the updated DAT file
write_lines(datLines,
            filePath %>%
              str_replace("\\.dat", paste0("_Zeroed_", startDay, "_to_", endDay, ".dat")))



print("Done!")

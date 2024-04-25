require(tidyverse)
require(readxl)
require(writexl)


# "PRMS Dat Manual Template.xlsx" has two sheets
# One is the Dat table with Downsizer data
# The other is the same table with NOAA data instead
# Compare the two versions


# Get "Shared_Functions_Demand.R" functions from the Demand folder
source("../Demand/Scripts/Shared_Functions_Demand.R.R")


noaaDF <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\2023-10 to 2024-03 Manual Downloads\\PRMS Dat Manual Template.xlsx") %>%
  read_xlsx(sheet = "NOAA Version")
  # read_csv("C:/Users/aprashar/Desktop/Supply_Issues_Investigation/JAN-2021 to MAR-2024 (No PRISM Substitution)/[NO_PRISM]_NOAA_API_Processed_2024-03-31.csv") %>%
  # filter(Date >= "2023-10-01" & Date <= "2024-03-31") %>%
  # select(Date, NOAA_PRECIP1, NOAA_PRECIP2,
  #        NOAA_PRECIP3, NOAA_PRECIP5,
  #        NOAA_PRECIP8, NOAA_PRECIP10, 
  #        NOAA_PRECIP11, NOAA_PRECIP13,
  #        NOAA_PRECIP14, NOAA_PRECIP15,
  #        NOAA_TMAX1, NOAA_TMAX2,
  #        NOAA_TMAX6, NOAA_TMIN1,
  #        NOAA_TMIN2, NOAA_TMIN6)

downDF <- makeSharePointPath("DWRAT\\SDU_Runs\\Hydrology\\2023-10 to 2024-03 Manual Downloads\\PRMS Dat Manual Template.xlsx") %>%
  read_xlsx(sheet = "DOWNSIZER Version") #%>%
  # select(Date, DOWNSIZER_PRECIP1, DOWNSIZER_PRECIP2,
  #        DOWNSIZER_PRECIP3, DOWNSIZER_PRECIP5,
  #        DOWNSIZER_PRECIP8, DOWNSIZER_PRECIP10,
  #        DOWNSIZER_PRECIP11, DOWNSIZER_PRECIP13,
  #        DOWNSIZER_PRECIP14, DOWNSIZER_PRECIP15,
  #        `DOWNSIZER_TMAX1\r\n(USC00043875)`, DOWNSIZER_TMAX2,
  #        `DOWNSIZER_TMAX6\r\n(USC00047109)`, DOWNSIZER_TMIN1,
  #        DOWNSIZER_TMIN2, DOWNSIZER_TMIN6)


stopifnot(nrow(noaaDF) == nrow(downDF))
stopifnot(ncol(noaaDF) == ncol(downDF))



mismatchDF <- matrix(rep(integer(0), 7), ncol = 7) %>%
  data.frame() %>%
  set_names(c("ROW_INDEX", "COL_INDEX", "DATE", 
              "NOAA_COL_NAME", "DOWNSIZER_COL_NAME", 
              "NOAA_VALUE", "DOWNSIZER_VALUE")) %>%
  mutate(DATE = as.Date(DATE))
  


for (j in 1:ncol(noaaDF)) {
  
  for (i in 1:nrow(noaaDF)) {
    
    if (noaaDF[i, j] != downDF[i, j]) {
      
      print(paste0("Mismatch at Row ", i, " of ", names(noaaDF)[j], "/", names(downDF)[j], " ('", noaaDF[i, j], "' and '", downDF[i, j], "')"))
      
      mismatchDF[nrow(mismatchDF) + 1, ] <- list(i, j, noaaDF$Date[i], names(noaaDF)[j], names(downDF)[j], noaaDF[i, j], downDF[i, j])
      
      
    }
  }
  
}



# Differences between precipitation values for PRECIP2 columns
noaaDF$NOAA_PRECIP2 %>% sum() # 79.3
downDF$DOWNSIZER_PRECIP2 %>% sum() # 78.214



# Differences for all NOAA/DOWNSIZER precipitation columns
precipNOAA <- noaaDF %>% select(contains("NOAA_PRECIP")) %>% colSums()
precipDOWN <- downDF %>% select(contains("DOWNSIZER_PRECIP")) %>% colSums()


# Percent Differences
100 * abs(precipNOAA - precipDOWN) / precipDOWN

# 2024-04-09 Analysis
# PRECIP1       0.00%
# PRECIP2       1.39%
# PRECIP3       0.00%
# PRECIP5       0.00%
# PRECIP8   4,233.36%
# PRECIP10  1,710.53%
# PRECIP11    -15.51%
# PRECIP13  2,963.58%
# PRECIP14      0.00%
# PRECIP15      0.00%



# Output 'mismatchDF' to a spreadsheet
# Add a column for the percent difference between the two values as well
mismatchDF %>%
  mutate(NOAA_VALUE = unlist(NOAA_VALUE), DOWNSIZER_VALUE = unlist(DOWNSIZER_VALUE)) %>%
  mutate(PERCENT_DIFFERENCE = 100 * abs(NOAA_VALUE - DOWNSIZER_VALUE) / DOWNSIZER_VALUE) %>%
  write_xlsx("NOAA_vs_Downsizer_Comparison_Mismatches.xlsx")



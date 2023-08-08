# Load required libraries
library(dplyr)
library(lubridate)

# Sample dataset (replace this with your actual dataset)
data <- data.frame(
  APPLICATION_NUMBER = c(1, 2, 3, 4),
  PARTY_ID = c("A", "B", "C", "D"),
  EFFECTIVE_START_DATE = c("2018-01-01", "2019-01-01", "2020-01-01", "2018-01-01"),
  EFFECTIVE_END_DATE = c("2020-12-31", NA, NA, NA),
  START_YEAR = c(2018, 2019, 2020, 2018),
  END_YEAR = c(2020, NA, NA, NA)
)

# Convert date columns to Date type
data$EFFECTIVE_START_DATE <- as.Date(data$EFFECTIVE_START_DATE)
data$EFFECTIVE_END_DATE <- as.Date(data$EFFECTIVE_END_DATE)

# Define the reporting years
reporting_years <- seq(2018, 2022)

# Perform the analysis
result <- data %>%
  mutate(Reporting_Year = year(EFFECTIVE_START_DATE)) %>%
  filter(START_YEAR %in% reporting_years,
         is.na(END_YEAR) | END_YEAR >= (Reporting_Year + 1)) %>%
  group_by(APPLICATION_NUMBER, Reporting_Year) %>%
  summarize(Active_PARTY_ID = first(PARTY_ID))

# View the result
print(result)


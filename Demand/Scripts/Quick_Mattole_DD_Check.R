library(dplyr)

# Load data
df = read.csv("OutputData/MT_2017_2023_MDT_2025-04-23.csv")


# Load the Top 10 Highest Ranking Diversions
top_10 <- df %>%
  arrange(desc(TOTAL_EXPECTED_ANNUAL_DIVERSION)) %>% # sort by descending order 
  slice_head(n = 10) %>%
  select(APPLICATION_NUMBER, PRIMARY_OWNER_TYPE, APPLICATION_PRIMARY_OWNER, TOTAL_EXPECTED_ANNUAL_DIVERSION)
print(top_10)


# Define range of columns
target_columns <- which(names(df) == "JAN_MEAN_DIV"):
  which(names(df) == "TOTAL_EXPECTED_ANNUAL_DIVERSION")

#Subset df to just the rows where any of the target_columns have at least 1 NA value
subset <- df[ apply(is.na(df[, target_columns]), 1, any), ]

print(subset)

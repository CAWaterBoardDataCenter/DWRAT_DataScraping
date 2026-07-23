# Created By: Payman Alemi
# Last Updated: 4/25/2024

# Purpose:
  # This script stores functions that are used by multiple supply scripts

add_date_column <- function(dataframe, col_position) {
  # dataframe is your input dataframe and col_position refers to the desired position of the Date column
  # you are creating
  
  # Check that year, month, and day columns exist
  required_cols <- c("Year", "month", "day")
  
  if (!all(required_cols %in% names(dataframe))) {
    stop("One or more of the date columns (Year, month, day) is missing.")
  }
  # Check that year, month, and day are all numeric
  if (!all(sapply(dataframe[required_cols], is.numeric))) {
    stop("One or more of the date columns (Year, month, day) is not numeric.")
  }
  
  # Create the date_column
  date_column <- paste0(dataframe$Year, "/", dataframe$month, "/", dataframe$day) %>% as.Date(format = "%Y/%m/%d")
  
  # Bind the date_column to the original dataframe
  dataframe <- cbind(dataframe[, 1:col_position-1], Date = date_column, dataframe[, col_position:ncol(dataframe)])
  
  return(dataframe)
}
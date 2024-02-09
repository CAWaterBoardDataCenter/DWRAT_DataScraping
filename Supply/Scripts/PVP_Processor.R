#Download the USGS Discharge data [CFS] from station EF Russian 11461500
#Change on Time Span to select the observed data range
#Select Data to graph--pick Discharge, cubic feet per second

#Click on Download data
#Under Select data to retrieve, pick Primary Time Series
#Click on Retrieve
#Import the CSV
#Calculate daily, monthly, and weekly running flow averages 
#Plot the averages


#SCRIPT LAST UPDATED:
#BY: Payman Alemi
#ON: 12/7/2023

# Load packages
library(tidyverse)
library(here)
library(httr)
library(ggplot2)

#Define PVP columns
pvp_names = c("USGS", "Station ID", "Date", "Time_Zone", "Discharge", "Provisional_Type")

#Define Timeframe
start_date <- "2023-11-01"
end_date <- "2023-11-30"

#Scrape PVP URL----
base_url <- "https://waterservices.usgs.gov/nwis/iv/?sites=11461500&parameterCd=00060"
dynamic_url <- paste0(base_url, "&startDT=", start_date, "&endDT=", end_date, "&siteStatus=all&format=rdb")
print(dynamic_url)

response <- GET(dynamic_url)

# Check if the request was successful
if (http_status(response)$category == "Success") {
  
  #Extract content from response as plain text 
  content <- content(response, "text")
  
  #Unlist the content and split it by lines
  lines <- unlist(strsplit(content, "\n"))
  
  #Filter to lines containing the string "USGS"
  desired_lines <- lines[grepl("USGS", lines)]
} else {
  
  #print an error message if the HTTP retrieval function fails
  print("Failed to retrieve the content")
}

#Clean up PVP Data----
#Filter out comment lines starting with #
desired_lines <- desired_lines[!grepl("^#", desired_lines)]
print(desired_lines[1:5])

# Separate desired_lines by tab
PVP <- strsplit(desired_lines, "\t")

# Convert the list of vectors into a matrix
PVP_matrix <- do.call(rbind, PVP)

# Convert the matrix to a dataframe
PVP_df <- as.data.frame(PVP_matrix, stringsAsFactors = FALSE)

# Set the column names for the dataframe
colnames(PVP_df) <- pvp_names

# Now you can print the dataframe to see if it looks correct
print(head(PVP_df))

#Calculate Daily, Weekly, and Monthly Running Averages for Discharge----

#Convert Discharge column to numeric format
PVP_df$Discharge <- as.numeric(as.character(PVP_df$Discharge))

# Convert 'Date' from character to POSIXct
PVP_df$DateTime <- as.POSIXct(x = PVP_df$Date, 
                              format = "%Y-%m-%d %H:%M",
                              tz = "PDT")

# Extract day, week, and month from the date-time
PVP_df$Day <- as.Date(PVP_df$DateTime)
PVP_df$Week <- floor_date(x = PVP_df$DateTime, unit = "week") %>% as.Date
PVP_df$Month <- floor_date(x = PVP_df$DateTime, unit = "month") %>% as.Date

# Daily running average
PVP_df <- PVP_df %>%
  group_by(Day) %>%
  mutate(Daily_Running_Avg = cummean(Discharge))

# Weekly running average
PVP_df <- PVP_df %>%
  group_by(Week) %>%
  mutate(Weekly_Running_Avg = cummean(Discharge))

# Monthly running average
PVP_df <- PVP_df %>%
  group_by(Month) %>%
  mutate(Monthly_Running_Avg = cummean(Discharge))

#Graph Running Averages----
# Now, you use ggplot2 to plot the data, using DateTime on the x-axis
ggplot(PVP_df, aes(x = DateTime)) + 
  geom_line(aes(y = Discharge, color = "Actual Discharge"), size = 1) +
  geom_line(aes(y = Daily_Running_Avg, color = "Daily Running Avg"), size = 1) +
  geom_line(aes(y = Weekly_Running_Avg, color = "Weekly Running Avg"), size = 1) +
  geom_line(aes(y = Monthly_Running_Avg, color = "Monthly Running Avg"), size = 1) +
  
  labs(title = "Discharge Running Averages with DateTime",
       x = "DateTime",
       y = "Discharge (cfs)",
       color = "Legend") + 
  
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(
    "Actual Discharge" = "blue", 
    "Daily Running Avg" = "red", 
    "Weekly Running Avg" = "green", 
    "Monthly Running Avg" = "gray"
  )) +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "7 day")



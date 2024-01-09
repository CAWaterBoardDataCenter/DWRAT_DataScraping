# Run the scripts one chunk at a time to insure that everything is working correctly. When you become more familiar with the code you can run in larger sections. 
#Install if you do not have in your current packages or are not up to date.----
# install.packages("tidyverse")
#Load Packages- This step must be done each time the project is opened. ----
library(tidyverse)
library(readxl)
library(data.table) #for fread function


# Output a message to the console
cat("Starting 'Priority_Date_Preprocessing.R'...")


######################################################################## List of Application from GIS Step ####################################################################################

# Import GIS data reviewed by SDU on 7/17/2023 and Payman on 9/19/2023
Application_Number <- read_xlsx("InputData/RR_pod_points_Merge_filtered_PA_2023-09-19.xlsx") %>%
  group_by(APPLICATION_NUMBER, POD_ID) %>%
  summarize(FREQUENCY = n(), .groups = "drop") #Summarizes records by APPLICATION_NUMBER and POD_ID; 
  #adds a FREQUENCY column and drops the other columns


# Keep only the "APPLICATION_NUMBER" and "FREQUENCY" columns
Application_Number <- Application_Number %>%
  select(APPLICATION_NUMBER, FREQUENCY) %>%
  unique()


# Read in the eWRIMS Flat File
ewrims_flat_file <- read.csv("RawData/ewrims_flat_file.csv") %>%
  unique()

# Perform an inner join using "APPLICATION_NUMBER"; 
  # force a one_to_one relationship to avoid duplicate application_numbers

ewrims_flat_file_Combined <- inner_join(Application_Number, ewrims_flat_file, by = "APPLICATION_NUMBER",
                                        relationship = "one-to-one")

# Output 'ewrims_flat_file_Combined' to a folder
write_csv(ewrims_flat_file_Combined,"IntermediateData/ewrims_flat_file_WITH_FILTERS.csv")
################################################################### Priority Date ############################################################

# Produce the input file for the Priority Date module

# First, read in a flat file
Priority_Date <- read.csv("IntermediateData/ewrims_flat_file_WITH_FILTERS.csv")

# Extract a subset of the columns
Priority_Date_FINAL <- Priority_Date %>%
  select(APPLICATION_NUMBER, WATER_RIGHT_TYPE, PRIORITY_DATE, APPLICATION_RECD_DATE, APPLICATION_ACCEPTANCE_DATE, SUB_TYPE, YEAR_DIVERSION_COMMENCED)

# Output that variable to a CSV file
write_csv(Priority_Date_FINAL,"IntermediateData/Priority_Date_FINAL.csv")


# Remove variables that are no longer needed
remove(Priority_Date, Priority_Date_FINAL, ewrims_flat_file, ewrims_flat_file_Combined)


# Output a completion message to the console
cat("Done!\n")

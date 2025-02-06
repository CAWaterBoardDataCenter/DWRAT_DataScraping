



remove(list = ls())


require(crayon)
require(data.table)
require(tidyverse)
require(odbc)
require(DBI)



# Download the Snowflake flat files to SharePoint
source("Scripts/New_Snowflake_Scripts/[CA]_1_Snowflake_Data_Download.R")



# Extract a subset of the eWRIMS POD Flat File
source("Scripts/New_Snowflake_Scripts/[CA]_2_POD_Flat_File_Prep.R")



# Generate a flag table
source("Scripts/New_Snowflake_Scripts/[CA]_3_Flag_Table_Generation.R")



# Calculate priority dates for each water right
source("Scripts/New_Snowflake_Scripts/[CA]_4_Flag_Table_Priority_Date.R")



# Flag duplicated entries for different submissions in the same year with the same owner
source("Scripts/New_Snowflake_Scripts/[CA]_5_Flag_Table_Duplicate_Reporting.R")





# Add this check somewhere: 
# Check for different use total vs storage + direct (same right, same year)



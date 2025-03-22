



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



# Flag reports with annual totals that are significantly different from certain reference values
# (i.e., face value amount, initial diversion amount, average total, and median total)
source("Scripts/New_Snowflake_Scripts/[CA]_6_Flag_Table_Expected_Demand.R")



# Flag reports that seemingly exist but lack direct diversion and diversion to storage data
source("Scripts/New_Snowflake_Scripts/[CA]_7_Flag_Table_Empty_Reports.R")



# Flag reports that should be considered for a face value substitution procedure
source("Scripts/New_Snowflake_Scripts/[CA]_8_Flag_Table_Face_Value_Substitution.R")



# Primary Beneficial Use



# Add this check somewhere? 
# Check for different use total vs storage + direct (same right, same year)

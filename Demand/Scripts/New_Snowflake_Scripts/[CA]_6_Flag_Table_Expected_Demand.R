# Add columns to the flag table related to unusual demand volumes
# (Unit conversion errors are the most common reason for this)

# (1) MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV
# (1) EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT
# (2) EXPECTED_DEMAND_FLAG_AVG_OR_MED_VOLUMES


#### Setup ####


remove(list = ls())


require(crayon)
require(data.table)
require(tidyverse)



source("Scripts/Shared_Functions_Demand.R")


#### Procedure ####


print("Starting '[CA]_5_Flag_Table_Duplicate_Reporting.R'...")





useDF <- makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended") %>%
  list.files(full.names = TRUE) %>%
  sort() %>% tail(1) %>%
  fileRead("fread",
           select = c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", 
                      "DIVERSION_TYPE")) %>%
  unique()



# There should be no missing ("NA") entries in any of these columns
if (anyNA(useDF)) {
  
  cat("\n\n")
  stop(paste0("There is a problem with the extended CSV. None of the table's columns ",
              " ('APPLICATION_NUMBER', 'YEAR', 'MONTH', 'DIVERSION_TYPE',",
              "  and 'AMOUNT') should contain 'NA' values.") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("problem", red("problem")) %>%
         str_replace("None", bold("None")) %>%
         str_replace("NA", red("NA")))
  
}



# Convert 'useDF' into a data frame with totals summarized by year 
# Only consider "DIRECT" and "STORAGE" diversion types
annualDF <- useDF %>%
  filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE")) %>%
  group_by(APPLICATION_NUMBER, YEAR) %>%
  summarize(YEAR_TOTAL = sum(AMOUNT), .groups = "drop")



# Also read in the face value amounts and initial diversion volumes
maxDivDF <- makeSharePointPath("Program Watersheds/7. Snowflake Demand Data Downloads/Water Use Report Extended") %>%
  list.files(full.names = TRUE) %>%
  sort() %>% tail(1) %>%
  fileRead("fread",
           select = c("APPLICATION_NUMBER", "YEAR", "FACE_VALUE_AMOUNT",
                      "FACE_VALUE_UNITS", "INI_REPORTED_DIV_AMOUNT", 
                      "INI_REPORTED_DIV_UNIT")) %>%
  unique()



# Some of the face value or initial diversion volumes may use units of gallons instead of acre-feet
# Convert them if they exist
maxDivDF$FACE_VALUE_AMOUNT[!is.na(maxDivDF$FACE_VALUE_AMOUNT) & maxDivDF$FACE_VALUE_UNITS == "Gallons"] <- maxDivDF$FACE_VALUE_AMOUNT[!is.na(maxDivDF$FACE_VALUE_AMOUNT) & maxDivDF$FACE_VALUE_UNITS == "Gallons"] / 325851
maxDivDF$FACE_VALUE_UNITS[!is.na(maxDivDF$FACE_VALUE_AMOUNT) & maxDivDF$FACE_VALUE_UNITS == "Gallons"] <- "Acre-feet per Year"



maxDivDF$INI_REPORTED_DIV_AMOUNT[!is.na(maxDivDF$INI_REPORTED_DIV_AMOUNT) & maxDivDF$INI_REPORTED_DIV_UNIT == "Gallons"] <- maxDivDF$INI_REPORTED_DIV_AMOUNT[!is.na(maxDivDF$INI_REPORTED_DIV_AMOUNT) & maxDivDF$INI_REPORTED_DIV_UNIT == "Gallons"] / 325851
maxDivDF$INI_REPORTED_DIV_UNIT[!is.na(maxDivDF$INI_REPORTED_DIV_AMOUNT) & maxDivDF$INI_REPORTED_DIV_UNIT == "Gallons"] <- "Acre-feet"



# Verify that all face value amounts are in acre-feet

# First check that there's only one unit referenced in the column 
if (maxDivDF %>% filter(!is.na(FACE_VALUE_AMOUNT)) %>% 
    select(FACE_VALUE_UNITS) %>% unique() %>% nrow() != 1)  {
  
  if (maxDivDF %>% filter(!is.na(FACE_VALUE_AMOUNT)) %>% 
      select(FACE_VALUE_UNITS) %>% unique() %>% nrow() == 0) {
    
    cat("\n\n")
    stop(paste0("The 'FACE_VALUE_UNITS' column should contain only",
                " one non-NA type of units: acre-feet. However,",
                " there are zero non-NA entries.") %>%
           strwrap(width = getOption("width")) %>%
           paste0(collapse = "\n") %>%
           str_replace("one", blue("one")) %>%
           str_replace("acre.feet", green("acre-feet")) %>%
           str_replace("zero", red("zero")))
    
  } else {
    
    cat("\n\n")
    stop(paste0("The 'FACE_VALUE_UNITS' column should contain only",
                " one non-NA type of units (acre-feet). However,",
                " it contains ",
                maxDivDF %>% filter(!is.na(FACE_VALUE_AMOUNT)) %>% 
                  select(FACE_VALUE_UNITS) %>% unique() %>% nrow(),
                " unit strings: ",
                maxDivDF %>% filter(!is.na(FACE_VALUE_AMOUNT)) %>% 
                  select(FACE_VALUE_UNITS) %>% unique() %>% unlist() %>%
                  paste0(collapse = "; ")) %>%
           strwrap(width = getOption("width")) %>%
           paste0(collapse = "\n") %>%
           str_replace("one", blue("one")) %>%
           str_replace("acre.feet", green("acre-feet")) %>%
           str_replace(maxDivDF %>% filter(!is.na(FACE_VALUE_AMOUNT)) %>% 
                         select(FACE_VALUE_UNITS) %>% unique() %>% nrow() %>%
                         paste0(" ", ., " "), 
                       red(maxDivDF %>% filter(!is.na(FACE_VALUE_AMOUNT)) %>% 
                             select(FACE_VALUE_UNITS) %>% unique() %>% nrow() %>%
                             paste0(" ", ., " "))))
    
  }
  
}



# Then, confirm that the specified unit is acre-feet (per year)
if (unique(maxDivDF$FACE_VALUE_UNITS[!is.na(maxDivDF$FACE_VALUE_AMOUNT)])[1] != "Acre-feet per Year") {
  
  cat("\n\n")
  stop(paste0("The expected units for the face value is acre-feet. To identify this unit,",
              " the 'FACE_VALUE_UNITS' column should contain 'Acre-feet per Year'.",
              " However, it instead contains the string '",
              unique(maxDivDF$FACE_VALUE_UNITS[!is.na(maxDivDF$FACE_VALUE_AMOUNT)])[1], "'.") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("acre.feet", green("acre-feet")) %>%
         str_replace("Acre.feet per Year", green("Acre-feet per Year")) %>%
         str_replace("However", red("However")))
  
}



# Perform the same checks for the initial reported diversion amount

# First check that there's only one unit referenced in the column 
if (maxDivDF %>% filter(!is.na(INI_REPORTED_DIV_AMOUNT)) %>% 
    select(INI_REPORTED_DIV_UNIT) %>% unique() %>% nrow() != 1)  {
  
  if (maxDivDF %>% filter(!is.na(INI_REPORTED_DIV_AMOUNT)) %>% 
      select(INI_REPORTED_DIV_UNIT) %>% unique() %>% nrow() == 0) {
    
    cat("\n\n")
    stop(paste0("The 'INI_REPORTED_DIV_UNIT' column should contain only",
                " one non-NA type of units: acre-feet. However,",
                " there are zero non-NA entries.") %>%
           strwrap(width = getOption("width")) %>%
           paste0(collapse = "\n") %>%
           str_replace("one", blue("one")) %>%
           str_replace("acre.feet", green("acre-feet")) %>%
           str_replace("zero", red("zero")))
    
  } else {
    
    cat("\n\n")
    stop(paste0("The 'INI_REPORTED_DIV_UNIT' column should contain only",
                " one non-NA type of units (acre-feet). However,",
                " it contains ",
                maxDivDF %>% filter(!is.na(INI_REPORTED_DIV_AMOUNT)) %>% 
                  select(INI_REPORTED_DIV_UNIT) %>% unique() %>% nrow(),
                " unit strings: ",
                maxDivDF %>% filter(!is.na(INI_REPORTED_DIV_AMOUNT)) %>% 
                  select(INI_REPORTED_DIV_UNIT) %>% unique() %>% unlist() %>%
                  paste0(collapse = "; ")) %>%
           strwrap(width = getOption("width")) %>%
           paste0(collapse = "\n") %>%
           str_replace("one", blue("one")) %>%
           str_replace("acre.feet", green("acre-feet")) %>%
           str_replace(maxDivDF %>% filter(!is.na(INI_REPORTED_DIV_AMOUNT)) %>% 
                         select(INI_REPORTED_DIV_UNIT) %>% unique() %>% nrow() %>%
                         paste0(" ", ., " "), 
                       red(maxDivDF %>% filter(!is.na(INI_REPORTED_DIV_AMOUNT)) %>% 
                             select(INI_REPORTED_DIV_UNIT) %>% unique() %>% nrow() %>%
                             paste0(" ", ., " "))))
    
  }
  
}



# Then, confirm that the specified unit is acre-feet 
if (unique(maxDivDF$INI_REPORTED_DIV_UNIT[!is.na(maxDivDF$INI_REPORTED_DIV_AMOUNT)])[1] != "Acre-feet") {
  
  cat("\n\n")
  stop(paste0("The expected units for the face value is acre-feet. To identify this unit,",
              " the 'INI_REPORTED_DIV_UNIT' column should contain 'Acre-feet'.",
              " However, it instead contains the string '",
              unique(maxDivDF$INI_REPORTED_DIV_UNIT[!is.na(maxDivDF$INI_REPORTED_DIV_AMOUNT)])[1], "'.") %>%
         strwrap(width = getOption("width")) %>%
         paste0(collapse = "\n") %>%
         str_replace("acre.feet", green("acre-feet")) %>%
         str_replace("Acre.feet per Year", green("Acre-feet per Year")) %>%
         str_replace("However", red("However")))
  
}



# Make a note in 'maxDivDF' if a water right has "NA" for both 
# "FACE_VALUE_AMOUNT" and "INI_REPORTED_DIV_AMOUNT"
# (This column will be added to the flag table later)
maxDivDF <- maxDivDF %>%
  mutate(MISSING_BOTH_FACE_VALUE_AND_INI_REPORTED_DIV = is.na(FACE_VALUE_AMOUNT) & is.na(INI_REPORTED_DIV_AMOUNT))



# Join 'maxDivDF' to 'annualDF'
# This should be a "many-to-one" join operation
# (multiple rows in 'annualDF' for the same row in 'maxDivDF')
annualDF <- annualDF %>%
  left_join(maxDivDF, by = c("APPLICATION_NUMBER", "YEAR"), 
            relationship = "many-to-one")



# Create a flag for unusual "YEAR_TOTAL" values
# The variable should be "TRUE" if one of the following is TRUE:
#   (1) "YEAR_TOTAL" / "FACE_VALUE_AMOUNT" > 100
#   (2) "YEAR_TOTAL" / "FACE_VALUE_AMOUNT" < 1/100
#   (3) "YEAR_TOTAL" / "INI_REPORTED_DIV_AMOUNT" > 100
#   (4) "YEAR_TOTAL" / "INI_REPORTED_DIV_AMOUNT" < 1/100
annualDF <- annualDF %>%
  mutate(EXPECTED_DEMAND_FLAG_FV_OR_INI_DIV_AMOUNT = 
           (!is.na(FACE_VALUE_AMOUNT) & FACE_VALUE_AMOUNT > 0 & YEAR_TOTAL / FACE_VALUE_AMOUNT > 100) |
           (!is.na(FACE_VALUE_AMOUNT) & FACE_VALUE_AMOUNT > 0 & YEAR_TOTAL / FACE_VALUE_AMOUNT < 1/100) |
           (!is.na(INI_REPORTED_DIV_AMOUNT) & INI_REPORTED_DIV_AMOUNT > 0 & YEAR_TOTAL / INI_REPORTED_DIV_AMOUNT > 100) |
           (!is.na(INI_REPORTED_DIV_AMOUNT) & INI_REPORTED_DIV_AMOUNT > 0 & YEAR_TOTAL / INI_REPORTED_DIV_AMOUNT < 1/100))
  


# Create a variable that has averages and medians for each water right








source("Scripts/Watershed_Selection.R")
source("Scripts/Dataset_Year_Range.R")


# Load in the two required input files for this module
# (unique() is used because a duplicate row exists in 'fvDF')
statDF <- read.csv(paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Statistics_FINAL.csv"))
fvDF <- read.csv(paste0("IntermediateData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Statistics_FaceValue_IniDiv_Final.csv")) %>% unique()


# Create and append two new columns to 'statDF'
# "COMPOSITE_MONTHLY" and "COMPOSITE_ANNUAL"
# These columns are simply concatenations of columns in 'statDF'
# statDF <- statDF %>%
#   mutate(COMPOSITE_MONTHLY = paste0(APPLICATION_NUMBER, YEAR, MONTH, DIVERSION_TYPE),
#          COMPOSITE_ANNUAL = paste0(APPLICATION_NUMBER, MONTH, DIVERSION_TYPE))



# Some columns in the module are important but 
# not fitting as an addition to a DF (at this point)
# Save them as separate variables instead


# Define a vector of unique application numbers stored in 'statDF'
uniqAppNum <- unique(statDF$APPLICATION_NUMBER)


# Note the minimum and maximum years in 'statDF'
# (The minimum year should not be below 2014 for this module)
minYear <- max(2014, min(statDF$YEAR))
maxYear <- max(statDF$YEAR)


# Create variables for the following:
# Number of Water Rights
# Number of Reporting Years
# Total Number of Expected Reports
numRights <- length(uniqAppNum)
numYears <- maxYear - minYear + 1
expectedReports <- numRights * numYears



# Create a table of monthly "AMOUNT" values for each "DIVERSION_TYPE"
# "APPLICATION_NUMBER" and "YEAR" will be specified in each row
# There will be separate columns for each month and diversion type
# Use a separate function for this
monthlyDF <- monthlyUseValues(statDF)



# Create an alternative version of 'monthlyDF' that uses water years instead of calendar years
# (This is important for reports submitted for 2022 and later)
monthlyDF_WY <- monthlyDF %>% CY2WY()



# Filter 'monthlyDF' to before 2022 (when reports used calendar years)
monthlyDF <- monthlyDF %>%
  filter(YEAR < 2022)


# Filter 'monthlyDF_WY' to 2022 and later (where reports use water years)
monthlyDF_WY <- monthlyDF_WY %>%
  filter(YEAR >= 2022)



# The last three months of 2021 in 'monthlyDF' should be set to NA
# (They already appear in the water year dataset as part of WY2022)
if (2021 %in% monthlyDF$YEAR && 2022 %in% monthlyDF_WY$YEAR) {
  
  monthlyDF$OCT_DIRECT_DIVERSION[monthlyDF$YEAR == 2021] <- NA_real_
  monthlyDF$NOV_DIRECT_DIVERSION[monthlyDF$YEAR == 2021] <- NA_real_
  monthlyDF$DEC_DIRECT_DIVERSION[monthlyDF$YEAR == 2021] <- NA_real_
  
  monthlyDF$OCT_STORAGE_DIVERSION[monthlyDF$YEAR == 2021] <- NA_real_
  monthlyDF$NOV_STORAGE_DIVERSION[monthlyDF$YEAR == 2021] <- NA_real_
  monthlyDF$DEC_STORAGE_DIVERSION[monthlyDF$YEAR == 2021] <- NA_real_
  
  monthlyDF$OCT_REPORTED_USE[monthlyDF$YEAR == 2021] <- NA_real_
  monthlyDF$NOV_REPORTED_USE[monthlyDF$YEAR == 2021] <- NA_real_
  monthlyDF$DEC_REPORTED_USE[monthlyDF$YEAR == 2021] <- NA_real_
  
}


# Get the annual direct diversion for each application and year
# Add that column to 'monthlyDF' with the name "ANNUAL_DIRECT"
monthlyDF <- monthlyDF %>%
  rowwise() %>%
  mutate(ANNUAL_DIRECT = sum(JAN_DIRECT_DIVERSION, FEB_DIRECT_DIVERSION, 
                             MAR_DIRECT_DIVERSION, APR_DIRECT_DIVERSION, MAY_DIRECT_DIVERSION, 
                             JUN_DIRECT_DIVERSION, JUL_DIRECT_DIVERSION, AUG_DIRECT_DIVERSION, 
                             SEP_DIRECT_DIVERSION, OCT_DIRECT_DIVERSION, NOV_DIRECT_DIVERSION, 
                             DEC_DIRECT_DIVERSION, na.rm = TRUE)) %>%
  ungroup()



monthlyDF_WY <- monthlyDF_WY %>%
  rowwise() %>%
  mutate(ANNUAL_DIRECT = sum(JAN_DIRECT_DIVERSION, FEB_DIRECT_DIVERSION, 
                             MAR_DIRECT_DIVERSION, APR_DIRECT_DIVERSION, MAY_DIRECT_DIVERSION, 
                             JUN_DIRECT_DIVERSION, JUL_DIRECT_DIVERSION, AUG_DIRECT_DIVERSION, 
                             SEP_DIRECT_DIVERSION, OCT_DIRECT_DIVERSION, NOV_DIRECT_DIVERSION, 
                             DEC_DIRECT_DIVERSION, na.rm = TRUE)) %>%
  ungroup()



# Do the same for the storage values (call the column "ANNUAL_STORAGE")
monthlyDF <- monthlyDF %>%
  rowwise() %>%
  mutate(ANNUAL_STORAGE = sum(JAN_STORAGE_DIVERSION, FEB_STORAGE_DIVERSION, 
                              MAR_STORAGE_DIVERSION, APR_STORAGE_DIVERSION, MAY_STORAGE_DIVERSION, 
                              JUN_STORAGE_DIVERSION, JUL_STORAGE_DIVERSION, AUG_STORAGE_DIVERSION, 
                              SEP_STORAGE_DIVERSION, OCT_STORAGE_DIVERSION, NOV_STORAGE_DIVERSION, 
                              DEC_STORAGE_DIVERSION, na.rm = TRUE)) %>%
  ungroup()



monthlyDF_WY <- monthlyDF_WY %>%
  rowwise() %>%
  mutate(ANNUAL_STORAGE = sum(JAN_STORAGE_DIVERSION, FEB_STORAGE_DIVERSION, 
                              MAR_STORAGE_DIVERSION, APR_STORAGE_DIVERSION, MAY_STORAGE_DIVERSION, 
                              JUN_STORAGE_DIVERSION, JUL_STORAGE_DIVERSION, AUG_STORAGE_DIVERSION, 
                              SEP_STORAGE_DIVERSION, OCT_STORAGE_DIVERSION, NOV_STORAGE_DIVERSION, 
                              DEC_STORAGE_DIVERSION, na.rm = TRUE)) %>%
  ungroup()



# Although it appears later in the module, define "ANNUAL_USE" now
monthlyDF <- monthlyDF %>%
  rowwise() %>%
  mutate(ANNUAL_USE = sum(JAN_REPORTED_USE, FEB_REPORTED_USE, 
                          MAR_REPORTED_USE, APR_REPORTED_USE, MAY_REPORTED_USE, 
                          JUN_REPORTED_USE, JUL_REPORTED_USE, AUG_REPORTED_USE, 
                          SEP_REPORTED_USE, OCT_REPORTED_USE, NOV_REPORTED_USE, 
                          DEC_REPORTED_USE, na.rm = TRUE)) %>%
  ungroup()



monthlyDF_WY <- monthlyDF_WY %>%
  rowwise() %>%
  mutate(ANNUAL_USE = sum(JAN_REPORTED_USE, FEB_REPORTED_USE, 
                          MAR_REPORTED_USE, APR_REPORTED_USE, MAY_REPORTED_USE, 
                          JUN_REPORTED_USE, JUL_REPORTED_USE, AUG_REPORTED_USE, 
                          SEP_REPORTED_USE, OCT_REPORTED_USE, NOV_REPORTED_USE, 
                          DEC_REPORTED_USE, na.rm = TRUE)) %>%
  ungroup()



# Create two flag columns:
# "DUPLICATE_STORAGE_USE" and "DUPLICATE_DIRECT_STORAGE"
# If "ANNUAL_STORAGE" is greater than 0,
# Check if its values are equal to "ANNUAL_USE" and "ANNUAL_DIRECT"
# Make a note if that is the case
# Otherwise, set it to an empty string ("")
monthlyDF <- monthlyDF %>%
  mutate(DUPLICATE_STORAGE_USE = if_else(!is.na(ANNUAL_STORAGE) & ANNUAL_STORAGE > 0 & 
                                           ANNUAL_STORAGE == ANNUAL_USE,
                                         "DUPLICATE_STOR_USE", 
                                         ""),
         DUPLICATE_DIRECT_STORAGE = if_else(!is.na(ANNUAL_STORAGE) & ANNUAL_STORAGE > 0 &
                                              ANNUAL_STORAGE == ANNUAL_DIRECT,
                                            "DUPLICATE_DIV_STOR",
                                            ""))



# Next, add a calendar year sum of "ANNUAL_DIRECT" and "ANNUAL_STORAGE"
# Then, make a similar sum for the months between May and September (inclusive)
monthlyDF <- monthlyDF %>%
  mutate(CALENDAR_YEAR_TOTAL = ANNUAL_DIRECT + ANNUAL_STORAGE,
         MAY_TO_SEP_TOTAL_DIVERSION = MAY_DIRECT_DIVERSION + JUN_DIRECT_DIVERSION + 
           JUL_DIRECT_DIVERSION + AUG_DIRECT_DIVERSION + SEP_DIRECT_DIVERSION + 
           MAY_STORAGE_DIVERSION + JUN_STORAGE_DIVERSION + JUL_STORAGE_DIVERSION + 
           AUG_STORAGE_DIVERSION + SEP_STORAGE_DIVERSION)



# Perform similar steps for the water year dataset
monthlyDF_WY <- monthlyDF_WY %>%
  mutate(WATER_YEAR_TOTAL = ANNUAL_DIRECT + ANNUAL_STORAGE,
         MAY_TO_SEP_TOTAL_DIVERSION = MAY_DIRECT_DIVERSION + JUN_DIRECT_DIVERSION + 
           JUL_DIRECT_DIVERSION + AUG_DIRECT_DIVERSION + SEP_DIRECT_DIVERSION + 
           MAY_STORAGE_DIVERSION + JUN_STORAGE_DIVERSION + JUL_STORAGE_DIVERSION + 
           AUG_STORAGE_DIVERSION + SEP_STORAGE_DIVERSION)



# After that, link the data in 'fvDF' to 'monthlyDF'
# The join will be based on "APPLICATION_NUMBER" 
# Each application number should appear only once in 'fvDF' 
# (and 'monthlyDF' will have multiple rows per number)
monthlyDF <- monthlyDF %>%
  left_join(fvDF, by = "APPLICATION_NUMBER", relationship = "many-to-one")



monthlyDF_WY <- monthlyDF_WY %>%
  left_join(fvDF, by = "APPLICATION_NUMBER", relationship = "many-to-one")



# Create a column that converts "Initial_Reported_Diversion" to acre-feet per year
# if its units are reported as "Gallons"
# (There are 325,851 gallons in 1 AF)
monthlyDF <- monthlyDF %>%
  mutate(IniDiv_Converted_to_AF = if_else(INI_REPORTED_DIV_UNIT == "Gallons",
                                          INI_REPORTED_DIV_AMOUNT / 325851,
                                          INI_REPORTED_DIV_AMOUNT))



monthlyDF_WY <- monthlyDF_WY %>%
  mutate(IniDiv_Converted_to_AF = if_else(INI_REPORTED_DIV_UNIT == "Gallons",
                                          INI_REPORTED_DIV_AMOUNT / 325851,
                                          INI_REPORTED_DIV_AMOUNT))



# Next calculate "Diversion_as_Percent_of_FV"
# This is "CALENDAR_YEAR_TOTAL" divided by "FACE_VALUE_AMOUNT"
# (If "FACE_VALUE_AMOUNT" is NA, this calculation will produce NA too)
monthlyDF <- monthlyDF %>%
  mutate(Diversion_as_Percent_of_FV = CALENDAR_YEAR_TOTAL / FACE_VALUE_AMOUNT)



monthlyDF_WY <- monthlyDF_WY %>%
  mutate(Diversion_as_Percent_of_FV = WATER_YEAR_TOTAL / FACE_VALUE_AMOUNT)



# After that, add the column "Amount_over_FV"
# If "FACE_VALUE_AMOUNT" is not NA (and it's greater than 0)
# Check "Diversion_as_Percent_of_FV"
# If the ratio is greater than 1, this column is the difference between
# "CALENDAR_YEAR_TOTAL" and "FACE_VALUE_AMOUNT"
# Otherwise, it is 0
monthlyDF <- monthlyDF %>%
  mutate(Amount_over_FV = if_else(!is.na(FACE_VALUE_AMOUNT) & FACE_VALUE_AMOUNT > 0,
                                  if_else(Diversion_as_Percent_of_FV > 1,
                                          CALENDAR_YEAR_TOTAL - FACE_VALUE_AMOUNT,
                                          0),
                                  NA_real_))



monthlyDF_WY <- monthlyDF_WY %>%
  mutate(Amount_over_FV = if_else(!is.na(FACE_VALUE_AMOUNT) & FACE_VALUE_AMOUNT > 0,
                                  if_else(Diversion_as_Percent_of_FV > 1,
                                          WATER_YEAR_TOTAL - FACE_VALUE_AMOUNT,
                                          0),
                                  NA_real_))



# Repeat the above two steps with "IniDiv_Converted_to_AF" instead of "FACE_VALUE_AMOUNT"
monthlyDF <- monthlyDF %>%
  mutate(Diversion_as_Percent_of_IniDiv = CALENDAR_YEAR_TOTAL / IniDiv_Converted_to_AF,
         Amount_over_IniDiv = if_else(!is.na(IniDiv_Converted_to_AF) & IniDiv_Converted_to_AF > 0,
                                      if_else(Diversion_as_Percent_of_IniDiv > 1,
                                              CALENDAR_YEAR_TOTAL - IniDiv_Converted_to_AF,
                                              0),
                                      NA_real_))



monthlyDF_WY <- monthlyDF_WY %>%
  mutate(Diversion_as_Percent_of_IniDiv = WATER_YEAR_TOTAL / IniDiv_Converted_to_AF,
         Amount_over_IniDiv = if_else(!is.na(IniDiv_Converted_to_AF) & IniDiv_Converted_to_AF > 0,
                                      if_else(Diversion_as_Percent_of_IniDiv > 1,
                                              WATER_YEAR_TOTAL - IniDiv_Converted_to_AF,
                                              0),
                                      NA_real_))



# The next group of columns check "CALENDAR_YEAR_TOTAL" for different units
# That column's values are assumed to be different from AF/year
# The options are "gallons", "gallons per minute", "gallons per day", 
# and "cubic feet per second"
# Attempt to convert the values into "AF/year" assuming one of the previous unit options
# In 1 AF, there are 325,851 gal or 43,559.9 ft^3
# In 1 yr, there are 365 days, 525,600 min, or 31,536,000 s
monthlyDF <- monthlyDF %>%
  mutate(Annual_Diversion_if_reported_in_Gallons = CALENDAR_YEAR_TOTAL / 325851,
         Annual_Diversion_if_reported_in_GPM = CALENDAR_YEAR_TOTAL / 325851 * 525600,
         Annual_Diversion_if_reported_in_GPD = CALENDAR_YEAR_TOTAL / 325851 * 365,
         Annual_Diversion_if_reported_in_CFS = CALENDAR_YEAR_TOTAL / 43559.9 * 31536000)



monthlyDF_WY <- monthlyDF_WY %>%
  mutate(Annual_Diversion_if_reported_in_Gallons = WATER_YEAR_TOTAL / 325851,
         Annual_Diversion_if_reported_in_GPM = WATER_YEAR_TOTAL / 325851 * 525600,
         Annual_Diversion_if_reported_in_GPD = WATER_YEAR_TOTAL / 325851 * 365,
         Annual_Diversion_if_reported_in_CFS = WATER_YEAR_TOTAL / 43559.9 * 31536000)



# Add counterparts to the 'Diversion_as_Percent_of_FV' column
# These use the alternative unit columns in place of "CALENDAR_YEAR_TOTAL"
monthlyDF <- monthlyDF %>%
  mutate(Gallons_as_percent_of_FV = Annual_Diversion_if_reported_in_Gallons / FACE_VALUE_AMOUNT,
         GPM_as_percent_of_FV = Annual_Diversion_if_reported_in_GPM / FACE_VALUE_AMOUNT,
         GPD_as_percent_of_FV = Annual_Diversion_if_reported_in_GPD / FACE_VALUE_AMOUNT,
         CFS_as_percent_of_FV = Annual_Diversion_if_reported_in_CFS / FACE_VALUE_AMOUNT,
         Gallons_as_percent_of_IniDiv = Annual_Diversion_if_reported_in_Gallons / IniDiv_Converted_to_AF,
         GPM_as_percent_of_IniDiv = Annual_Diversion_if_reported_in_GPM / IniDiv_Converted_to_AF,
         GPD_as_percent_of_IniDiv = Annual_Diversion_if_reported_in_GPD / IniDiv_Converted_to_AF,
         CFS_as_percent_of_IniDiv = Annual_Diversion_if_reported_in_CFS / IniDiv_Converted_to_AF,
         QAQC_Action_Taken = NA_character_,
         QAQC_Reason = NA_character_,
         Staff = NA_character_)



monthlyDF_WY <- monthlyDF_WY %>%
  mutate(Gallons_as_percent_of_FV = Annual_Diversion_if_reported_in_Gallons / FACE_VALUE_AMOUNT,
         GPM_as_percent_of_FV = Annual_Diversion_if_reported_in_GPM / FACE_VALUE_AMOUNT,
         GPD_as_percent_of_FV = Annual_Diversion_if_reported_in_GPD / FACE_VALUE_AMOUNT,
         CFS_as_percent_of_FV = Annual_Diversion_if_reported_in_CFS / FACE_VALUE_AMOUNT,
         Gallons_as_percent_of_IniDiv = Annual_Diversion_if_reported_in_Gallons / IniDiv_Converted_to_AF,
         GPM_as_percent_of_IniDiv = Annual_Diversion_if_reported_in_GPM / IniDiv_Converted_to_AF,
         GPD_as_percent_of_IniDiv = Annual_Diversion_if_reported_in_GPD / IniDiv_Converted_to_AF,
         CFS_as_percent_of_IniDiv = Annual_Diversion_if_reported_in_CFS / IniDiv_Converted_to_AF,
         QAQC_Action_Taken = NA_character_,
         QAQC_Reason = NA_character_,
         Staff = NA_character_)



# A new data frame is needed for the next group of columns

# Each row will be for one unique application number
# There will be columns with average "AMOUNT" volumes for each month
# (For the use types "DIRECT" and "STORAGE")

# Standard deviations will be calculated as well (for "DIRECT" only)

# Use 'monthlyDF' to create this table
#avgDF <- monthlyAvg(monthlyDF)



#avgDF_WY <- monthlyAvg(monthlyDF_WY)



# Next, for each month, define a variable with the total expected diversion
# (The sum of "[MONTH]_AVERAGE_DIRECT_DIVERSION" and "[MONTH]_AVERAGE_STORAGE_DIVERSION")
# avgDF <- avgDF %>%
#   mutate(JAN_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(JAN_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(JAN_AVERAGE_STORAGE_DIVERSION, 0),
#          FEB_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(FEB_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(FEB_AVERAGE_STORAGE_DIVERSION, 0),
#          MAR_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(MAR_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(MAR_AVERAGE_STORAGE_DIVERSION, 0),
#          APR_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(APR_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(APR_AVERAGE_STORAGE_DIVERSION, 0),
#          MAY_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(MAY_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(MAY_AVERAGE_STORAGE_DIVERSION, 0),
#          JUN_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(JUN_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(JUN_AVERAGE_STORAGE_DIVERSION, 0),
#          JUL_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(JUL_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(JUL_AVERAGE_STORAGE_DIVERSION, 0),
#          AUG_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(AUG_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(AUG_AVERAGE_STORAGE_DIVERSION, 0),
#          SEP_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(SEP_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(SEP_AVERAGE_STORAGE_DIVERSION, 0),
#          OCT_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(OCT_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(OCT_AVERAGE_STORAGE_DIVERSION, 0),
#          NOV_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(NOV_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(NOV_AVERAGE_STORAGE_DIVERSION, 0),
#          DEC_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(DEC_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(DEC_AVERAGE_STORAGE_DIVERSION, 0))



# avgDF_WY <- avgDF_WY %>%
#   mutate(JAN_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(JAN_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(JAN_AVERAGE_STORAGE_DIVERSION, 0),
#          FEB_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(FEB_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(FEB_AVERAGE_STORAGE_DIVERSION, 0),
#          MAR_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(MAR_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(MAR_AVERAGE_STORAGE_DIVERSION, 0),
#          APR_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(APR_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(APR_AVERAGE_STORAGE_DIVERSION, 0),
#          MAY_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(MAY_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(MAY_AVERAGE_STORAGE_DIVERSION, 0),
#          JUN_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(JUN_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(JUN_AVERAGE_STORAGE_DIVERSION, 0),
#          JUL_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(JUL_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(JUL_AVERAGE_STORAGE_DIVERSION, 0),
#          AUG_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(AUG_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(AUG_AVERAGE_STORAGE_DIVERSION, 0),
#          SEP_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(SEP_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(SEP_AVERAGE_STORAGE_DIVERSION, 0),
#          OCT_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(OCT_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(OCT_AVERAGE_STORAGE_DIVERSION, 0),
#          NOV_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(NOV_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(NOV_AVERAGE_STORAGE_DIVERSION, 0),
#          DEC_EXPECTED_TOTAL_DIVERSION = 
#            replace_na(DEC_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(DEC_AVERAGE_STORAGE_DIVERSION, 0))



# After that, define a variable for the average annual 
# It will be the sum of the average monthly diversions for "DIRECT" and "STORAGE"
# avgDF <- avgDF %>%
#   mutate(ANNUAL_TOTAL_DIVERSION = 
#            JAN_EXPECTED_TOTAL_DIVERSION + FEB_EXPECTED_TOTAL_DIVERSION + 
#            MAR_EXPECTED_TOTAL_DIVERSION + APR_EXPECTED_TOTAL_DIVERSION + 
#            MAY_EXPECTED_TOTAL_DIVERSION + JUN_EXPECTED_TOTAL_DIVERSION + 
#            JUL_EXPECTED_TOTAL_DIVERSION + AUG_EXPECTED_TOTAL_DIVERSION + 
#            SEP_EXPECTED_TOTAL_DIVERSION + OCT_EXPECTED_TOTAL_DIVERSION + 
#            NOV_EXPECTED_TOTAL_DIVERSION + DEC_EXPECTED_TOTAL_DIVERSION)
# 
# 
# 
# avgDF_WY <- avgDF_WY %>%
#   mutate(ANNUAL_TOTAL_DIVERSION = 
#            JAN_EXPECTED_TOTAL_DIVERSION + FEB_EXPECTED_TOTAL_DIVERSION + 
#            MAR_EXPECTED_TOTAL_DIVERSION + APR_EXPECTED_TOTAL_DIVERSION + 
#            MAY_EXPECTED_TOTAL_DIVERSION + JUN_EXPECTED_TOTAL_DIVERSION + 
#            JUL_EXPECTED_TOTAL_DIVERSION + AUG_EXPECTED_TOTAL_DIVERSION + 
#            SEP_EXPECTED_TOTAL_DIVERSION + OCT_EXPECTED_TOTAL_DIVERSION + 
#            NOV_EXPECTED_TOTAL_DIVERSION + DEC_EXPECTED_TOTAL_DIVERSION)



# Get a similar column to the previous one, except for the dry period only
# (From May to September)
# avgDF <- avgDF %>%
#   mutate(MAY_TO_SEP_TOTAL_DIVERSION = 
#            MAY_EXPECTED_TOTAL_DIVERSION + JUN_EXPECTED_TOTAL_DIVERSION + 
#            JUL_EXPECTED_TOTAL_DIVERSION + AUG_EXPECTED_TOTAL_DIVERSION + 
#            SEP_EXPECTED_TOTAL_DIVERSION)



# avgDF_WY <- avgDF_WY %>%
#   mutate(MAY_TO_SEP_TOTAL_DIVERSION = 
#            MAY_EXPECTED_TOTAL_DIVERSION + JUN_EXPECTED_TOTAL_DIVERSION + 
#            JUL_EXPECTED_TOTAL_DIVERSION + AUG_EXPECTED_TOTAL_DIVERSION + 
#            SEP_EXPECTED_TOTAL_DIVERSION)



# Next, add to 'avgDF' the average of "ANNUAL_USE" for each application number
# ("ANNUAL_USE" is a column in 'monthlyDF')
# avgDF <- avgDF %>%
#   full_join(monthlyDF %>%
#               ungroup() %>%
#               select(APPLICATION_NUMBER, ANNUAL_USE) %>%
#               group_by(APPLICATION_NUMBER) %>%
#               summarize(TOTAL_ANNUAL_USE = mean(ANNUAL_USE)),
#             by = "APPLICATION_NUMBER", relationship = "one-to-one")



# avgDF_WY <- avgDF_WY %>%
#   full_join(monthlyDF_WY %>%
#               ungroup() %>%
#               select(APPLICATION_NUMBER, ANNUAL_USE) %>%
#               group_by(APPLICATION_NUMBER) %>%
#               summarize(TOTAL_ANNUAL_USE = mean(ANNUAL_USE)),
#             by = "APPLICATION_NUMBER", relationship = "one-to-one")



# Then, create a column that is the average of the standard deviations
# for the "DIRECT" use types
# (NA rows are ignored in these calculations)
# avgDF <- avgDF %>%
#   rowwise() %>%
#   mutate(AVERAGE_STDEV = mean(JAN_STDEV, FEB_STDEV, MAR_STDEV,
#                               APR_STDEV, MAY_STDEV, JUN_STDEV,
#                               JUL_STDEV, AUG_STDEV, SEP_STDEV,
#                               OCT_STDEV, NOV_STDEV, DEC_STDEV,
#                               na.rm = TRUE)) %>%
#   ungroup()



# if (nrow(avgDF_WY) > 0) {
#   
#   avgDF_WY <- avgDF_WY %>%
#     rowwise() %>%
#     mutate(AVERAGE_STDEV = mean(JAN_STDEV, FEB_STDEV, MAR_STDEV,
#                                 APR_STDEV, MAY_STDEV, JUN_STDEV,
#                                 JUL_STDEV, AUG_STDEV, SEP_STDEV,
#                                 OCT_STDEV, NOV_STDEV, DEC_STDEV,
#                                 na.rm = TRUE)) %>%
#     ungroup()
#   
# }



# After that, use 'statDF' to add new columns to 'avgDF'
# These columns ("Total_Cumulative_Diverted" and "Total_Cumulative_Use") will be
# sums of all diversion volumes ("DIRECT"/"STORAGE" and "USE")
# for each application number


# Add "Total_Cumulative_Diverted" first
# avgDF <- avgDF %>%
#   left_join(statDF %>%
#               filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE")) %>%
#               select(APPLICATION_NUMBER, AMOUNT) %>%
#               group_by(APPLICATION_NUMBER) %>%
#               summarize(Total_Cumulative_Diverted = sum(AMOUNT, na.rm = TRUE)),
#             by = "APPLICATION_NUMBER", relationship = "one-to-one")
# 
# 
# 
# avgDF_WY <- avgDF_WY %>%
#   left_join(statDF %>%
#               filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE")) %>%
#               select(APPLICATION_NUMBER, AMOUNT) %>%
#               group_by(APPLICATION_NUMBER) %>%
#               summarize(Total_Cumulative_Diverted = sum(AMOUNT, na.rm = TRUE)),
#             by = "APPLICATION_NUMBER", relationship = "one-to-one")


# Add "Total_Cumulative_Use" after that 
# avgDF <- avgDF %>%
#   left_join(statDF %>%
#               filter(DIVERSION_TYPE == "USE") %>%
#               select(APPLICATION_NUMBER, AMOUNT) %>%
#               group_by(APPLICATION_NUMBER) %>%
#               summarize(Total_Cumulative_Use = sum(AMOUNT, na.rm = TRUE)),
#             by = "APPLICATION_NUMBER", relationship = "one-to-one")
# 
# 
# avgDF_WY <- avgDF_WY %>%
#   left_join(statDF %>%
#               filter(DIVERSION_TYPE == "USE") %>%
#               select(APPLICATION_NUMBER, AMOUNT) %>%
#               group_by(APPLICATION_NUMBER) %>%
#               summarize(Total_Cumulative_Use = sum(AMOUNT, na.rm = TRUE)),
#             by = "APPLICATION_NUMBER", relationship = "one-to-one")



# The final column to add is "Total_Use_as_a_Percent_of_Total_Diverted"
# It will be a ratio of "Total_Cumulative_Use" to "Total_Cumulative_Diverted"
# Only perform that calculation if the latter is greater than 0 and not NA
# avgDF <- avgDF %>%
#   mutate(Total_Use_as_a_Percent_of_Total_Diverted = 
#            if_else(!is.na(Total_Cumulative_Diverted) & Total_Cumulative_Diverted > 0,
#                    Total_Cumulative_Use / Total_Cumulative_Diverted,
#                    NA_real_))



# avgDF_WY <- avgDF_WY %>%
#   mutate(Total_Use_as_a_Percent_of_Total_Diverted = 
#            if_else(!is.na(Total_Cumulative_Diverted) & Total_Cumulative_Diverted > 0,
#                    Total_Cumulative_Use / Total_Cumulative_Diverted,
#                    NA_real_))



# Merge the two 'monthlyDF' tibbles together
# NOTE: The data from October - December 2021 will be present twice in the dataset
# (both the CY2021 and the WY2022 datasets)
monthlyDF <- monthlyDF %>%
  bind_rows(monthlyDF_WY) %>%
  mutate(YEAR_TOTAL = if_else(is.na(WATER_YEAR_TOTAL), CALENDAR_YEAR_TOTAL, WATER_YEAR_TOTAL))


# avgDF <- avgDF %>%
#   bind_rows(avgDF_WY)


# The final step of this script is to output a spreadsheet 
# in a similar format as "ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics.xlsx"
# Use a separate function to create the workbook
# makeXLSX(avgDF, fvDF, monthlyDF, statDF, expectedReports, maxYear, minYear,
#          numRights, numYears, uniqAppNum)
monthlyDF %>%
  select(APPLICATION_NUMBER, YEAR, JAN_DIRECT_DIVERSION,
         FEB_DIRECT_DIVERSION, MAR_DIRECT_DIVERSION,
         APR_DIRECT_DIVERSION, MAY_DIRECT_DIVERSION,
         JUN_DIRECT_DIVERSION, JUL_DIRECT_DIVERSION,
         AUG_DIRECT_DIVERSION, SEP_DIRECT_DIVERSION,
         OCT_DIRECT_DIVERSION, NOV_DIRECT_DIVERSION,
         DEC_DIRECT_DIVERSION, JAN_STORAGE_DIVERSION,
         FEB_STORAGE_DIVERSION, MAR_STORAGE_DIVERSION,
         APR_STORAGE_DIVERSION, MAY_STORAGE_DIVERSION,
         JUN_STORAGE_DIVERSION, JUL_STORAGE_DIVERSION,
         AUG_STORAGE_DIVERSION, SEP_STORAGE_DIVERSION,
         OCT_STORAGE_DIVERSION, NOV_STORAGE_DIVERSION,
         DEC_STORAGE_DIVERSION) %>%
  write.xlsx(paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_Monthly_Diversions.xlsx"),
             overwrite = TRUE)
#write.xlsx(paste0("OutputData/", ws$ID, "_ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx"),
#           overwrite = TRUE)



monthlyDF %>%
  select(APPLICATION_NUMBER, INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT, 
         FACE_VALUE_AMOUNT, FACE_VALUE_UNITS, IniDiv_Converted_to_AF) %>%
  unique() %>%
  write.xlsx(paste0("OutputData/", ws$ID, "_", yearRange[1], "_", yearRange[2], "_ExpectedDemand_FV.xlsx"), overwrite = TRUE)



if (monthlyDF %>%
    select(APPLICATION_NUMBER, INI_REPORTED_DIV_AMOUNT, FACE_VALUE_AMOUNT) %>% 
    filter(is.na(INI_REPORTED_DIV_AMOUNT) & FACE_VALUE_AMOUNT == 0) %>% unique() %>%
    nrow() > 0) {
  
  cat(paste0("\n\nWarning: The following rights have a face value amount of 0 AF (with no initial reported diversion amount):\n\n",
             monthlyDF %>%
               select(APPLICATION_NUMBER, INI_REPORTED_DIV_AMOUNT, FACE_VALUE_AMOUNT) %>% 
               filter(is.na(INI_REPORTED_DIV_AMOUNT) & FACE_VALUE_AMOUNT == 0) %>% unique() %>%
               select(APPLICATION_NUMBER) %>% unlist() %>% sort() %>% paste0(collapse = "\n"),
             "\n\n"))
  
  cat(paste0("Note: This list can be extracted from the spreadsheet 'OutputData/", ws$ID, 
             "_", yearRange[1], "_", yearRange[2], 
             "_ExpectedDemand_FV.xlsx'\n\n"))
  
}



# A spreadsheet for QAQC review will be produced next
# Before that, read in data from a previous manual review (if it exists)
# Exclude entries that were already checked previously
if (!is.na(ws$QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH[1])) {
  
  reviewDF <- getXLSX(ws = ws, 
                      SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET",
                      FILEPATH = "QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH", 
                      WORKSHEET_NAME = "QAQC_UNIT_CONVERSION_ERRORS_WORKSHEET_NAME") %>%
    mutate(YEAR_TOTAL = as.numeric(YEAR_TOTAL))
  
  
  
  # Make a key for unique combinations of "APPLICATION_NUMBER", "YEAR", and "YEAR_TOTAL"
  reviewDF <- reviewDF %>%
    makeKey_APP_YEAR_AMOUNT()
  
  
  
  # If the second manual review was also performed, add that spreadsheet here too
  if (!is.na(ws$QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH[1])) {
    
    reviewDF2 <- getXLSX(ws = ws,
                         SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET",
                         FILEPATH = "QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH", 
                         WORKSHEET_NAME =  "QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_WORKSHEET_NAME")%>%
      mutate(YEAR_TOTAL = as.numeric(YEAR_TOTAL)) %>%
      makeKey_APP_YEAR_AMOUNT()
    
    
    
    # Combine the two review datasets
    reviewDF <- rbind(reviewDF, reviewDF2)
    
  }
  
  
  
  # Remove those already-reviewed rows from 'monthlyDF'
  monthlyDF <- compareKeys(monthlyDF, reviewDF)
  
}



# After that, save another spreadsheet with just columns related to assessing unit conversion errors
monthlyDF %>%
  select(APPLICATION_NUMBER, YEAR,
         YEAR_TOTAL, 
         FACE_VALUE_AMOUNT, IniDiv_Converted_to_AF,
         Diversion_as_Percent_of_FV, Diversion_as_Percent_of_IniDiv,
         Annual_Diversion_if_reported_in_Gallons, Gallons_as_percent_of_FV,
         Gallons_as_percent_of_IniDiv,
         Annual_Diversion_if_reported_in_GPM, GPM_as_percent_of_FV, GPM_as_percent_of_IniDiv,
         Annual_Diversion_if_reported_in_GPD, GPD_as_percent_of_FV, GPD_as_percent_of_IniDiv,
         Annual_Diversion_if_reported_in_CFS, CFS_as_percent_of_FV, CFS_as_percent_of_IniDiv,
         QAQC_Action_Taken, QAQC_Reason, Staff) %>%
  filter((Diversion_as_Percent_of_FV > 100 & FACE_VALUE_AMOUNT > 0) | 
           (Diversion_as_Percent_of_FV < 0.01 & Diversion_as_Percent_of_FV > 0 & FACE_VALUE_AMOUNT > 0) | 
           (Diversion_as_Percent_of_IniDiv > 100 & IniDiv_Converted_to_AF > 0) | 
           (Diversion_as_Percent_of_IniDiv < 0.01 & Diversion_as_Percent_of_IniDiv > 0 & IniDiv_Converted_to_AF > 0)) %>%
  arrange(APPLICATION_NUMBER, YEAR) %>% 
  write.xlsx(paste0("OutputData/", ws$ID, "_Expected_Demand_Units_QAQC.xlsx"), overwrite = TRUE)








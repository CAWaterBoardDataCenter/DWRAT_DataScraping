# Looking for annual diversion amounts that exceed previously report diversion amounts
# Also look for unit conversion errors



# This script is based on the Excel module called:
# "ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics.xlsx"


#### Dependencies ####


require(tidyverse)
require(openxlsx)
require(readxl)


#### Script Procedure ####


mainProcedure <- function (ws) {
  
  # The main body of the script
  
  
  
  # Load in the two required input files for this module
  # (unique() is used because a duplicate row exists in 'fvDF')
  statDF <- read.csv(paste0("IntermediateData/", ws$ID, "_Statistics_FINAL.csv"))
  fvDF <- read.csv(paste0("IntermediateData/", ws$ID, "_Statistics_FaceValue_IniDiv_Final.csv")) %>% unique()
  
  
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
           QAQC_Reason = NA_character_)
  
  
  
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
           QAQC_Reason = NA_character_)
  
  
  
  # A new data frame is needed for the next group of columns
  
  # Each row will be for one unique application number
  # There will be columns with average "AMOUNT" volumes for each month
  # (For the use types "DIRECT" and "STORAGE")
  
  # Standard deviations will be calculated as well (for "DIRECT" only)
  
  # Use 'monthlyDF' to create this table
  avgDF <- monthlyAvg(monthlyDF)
  
  
  
  avgDF_WY <- monthlyAvg(monthlyDF_WY)
  
  
  
  # Next, for each month, define a variable with the total expected diversion
  # (The sum of "[MONTH]_AVERAGE_DIRECT_DIVERSION" and "[MONTH]_AVERAGE_STORAGE_DIVERSION")
  avgDF <- avgDF %>%
    mutate(JAN_EXPECTED_TOTAL_DIVERSION = 
             replace_na(JAN_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(JAN_AVERAGE_STORAGE_DIVERSION, 0),
           FEB_EXPECTED_TOTAL_DIVERSION = 
             replace_na(FEB_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(FEB_AVERAGE_STORAGE_DIVERSION, 0),
           MAR_EXPECTED_TOTAL_DIVERSION = 
             replace_na(MAR_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(MAR_AVERAGE_STORAGE_DIVERSION, 0),
           APR_EXPECTED_TOTAL_DIVERSION = 
             replace_na(APR_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(APR_AVERAGE_STORAGE_DIVERSION, 0),
           MAY_EXPECTED_TOTAL_DIVERSION = 
             replace_na(MAY_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(MAY_AVERAGE_STORAGE_DIVERSION, 0),
           JUN_EXPECTED_TOTAL_DIVERSION = 
             replace_na(JUN_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(JUN_AVERAGE_STORAGE_DIVERSION, 0),
           JUL_EXPECTED_TOTAL_DIVERSION = 
             replace_na(JUL_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(JUL_AVERAGE_STORAGE_DIVERSION, 0),
           AUG_EXPECTED_TOTAL_DIVERSION = 
             replace_na(AUG_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(AUG_AVERAGE_STORAGE_DIVERSION, 0),
           SEP_EXPECTED_TOTAL_DIVERSION = 
             replace_na(SEP_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(SEP_AVERAGE_STORAGE_DIVERSION, 0),
           OCT_EXPECTED_TOTAL_DIVERSION = 
             replace_na(OCT_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(OCT_AVERAGE_STORAGE_DIVERSION, 0),
           NOV_EXPECTED_TOTAL_DIVERSION = 
             replace_na(NOV_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(NOV_AVERAGE_STORAGE_DIVERSION, 0),
           DEC_EXPECTED_TOTAL_DIVERSION = 
             replace_na(DEC_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(DEC_AVERAGE_STORAGE_DIVERSION, 0))
  
  
  
  avgDF_WY <- avgDF_WY %>%
    mutate(JAN_EXPECTED_TOTAL_DIVERSION = 
             replace_na(JAN_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(JAN_AVERAGE_STORAGE_DIVERSION, 0),
           FEB_EXPECTED_TOTAL_DIVERSION = 
             replace_na(FEB_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(FEB_AVERAGE_STORAGE_DIVERSION, 0),
           MAR_EXPECTED_TOTAL_DIVERSION = 
             replace_na(MAR_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(MAR_AVERAGE_STORAGE_DIVERSION, 0),
           APR_EXPECTED_TOTAL_DIVERSION = 
             replace_na(APR_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(APR_AVERAGE_STORAGE_DIVERSION, 0),
           MAY_EXPECTED_TOTAL_DIVERSION = 
             replace_na(MAY_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(MAY_AVERAGE_STORAGE_DIVERSION, 0),
           JUN_EXPECTED_TOTAL_DIVERSION = 
             replace_na(JUN_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(JUN_AVERAGE_STORAGE_DIVERSION, 0),
           JUL_EXPECTED_TOTAL_DIVERSION = 
             replace_na(JUL_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(JUL_AVERAGE_STORAGE_DIVERSION, 0),
           AUG_EXPECTED_TOTAL_DIVERSION = 
             replace_na(AUG_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(AUG_AVERAGE_STORAGE_DIVERSION, 0),
           SEP_EXPECTED_TOTAL_DIVERSION = 
             replace_na(SEP_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(SEP_AVERAGE_STORAGE_DIVERSION, 0),
           OCT_EXPECTED_TOTAL_DIVERSION = 
             replace_na(OCT_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(OCT_AVERAGE_STORAGE_DIVERSION, 0),
           NOV_EXPECTED_TOTAL_DIVERSION = 
             replace_na(NOV_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(NOV_AVERAGE_STORAGE_DIVERSION, 0),
           DEC_EXPECTED_TOTAL_DIVERSION = 
             replace_na(DEC_AVERAGE_DIRECT_DIVERSION, 0) + replace_na(DEC_AVERAGE_STORAGE_DIVERSION, 0))
  
  
  
  # After that, define a variable for the average annual 
  # It will be the sum of the average monthly diversions for "DIRECT" and "STORAGE"
  avgDF <- avgDF %>%
    mutate(ANNUAL_TOTAL_DIVERSION = 
             JAN_EXPECTED_TOTAL_DIVERSION + FEB_EXPECTED_TOTAL_DIVERSION + 
             MAR_EXPECTED_TOTAL_DIVERSION + APR_EXPECTED_TOTAL_DIVERSION + 
             MAY_EXPECTED_TOTAL_DIVERSION + JUN_EXPECTED_TOTAL_DIVERSION + 
             JUL_EXPECTED_TOTAL_DIVERSION + AUG_EXPECTED_TOTAL_DIVERSION + 
             SEP_EXPECTED_TOTAL_DIVERSION + OCT_EXPECTED_TOTAL_DIVERSION + 
             NOV_EXPECTED_TOTAL_DIVERSION + DEC_EXPECTED_TOTAL_DIVERSION)
  
  
  
  avgDF_WY <- avgDF_WY %>%
    mutate(ANNUAL_TOTAL_DIVERSION = 
             JAN_EXPECTED_TOTAL_DIVERSION + FEB_EXPECTED_TOTAL_DIVERSION + 
             MAR_EXPECTED_TOTAL_DIVERSION + APR_EXPECTED_TOTAL_DIVERSION + 
             MAY_EXPECTED_TOTAL_DIVERSION + JUN_EXPECTED_TOTAL_DIVERSION + 
             JUL_EXPECTED_TOTAL_DIVERSION + AUG_EXPECTED_TOTAL_DIVERSION + 
             SEP_EXPECTED_TOTAL_DIVERSION + OCT_EXPECTED_TOTAL_DIVERSION + 
             NOV_EXPECTED_TOTAL_DIVERSION + DEC_EXPECTED_TOTAL_DIVERSION)
  
  
  
  # Get a similar column to the previous one, except for the dry period only
  # (From May to September)
  avgDF <- avgDF %>%
    mutate(MAY_TO_SEP_TOTAL_DIVERSION = 
             MAY_EXPECTED_TOTAL_DIVERSION + JUN_EXPECTED_TOTAL_DIVERSION + 
             JUL_EXPECTED_TOTAL_DIVERSION + AUG_EXPECTED_TOTAL_DIVERSION + 
             SEP_EXPECTED_TOTAL_DIVERSION)
  
  
  
  avgDF_WY <- avgDF_WY %>%
    mutate(MAY_TO_SEP_TOTAL_DIVERSION = 
             MAY_EXPECTED_TOTAL_DIVERSION + JUN_EXPECTED_TOTAL_DIVERSION + 
             JUL_EXPECTED_TOTAL_DIVERSION + AUG_EXPECTED_TOTAL_DIVERSION + 
             SEP_EXPECTED_TOTAL_DIVERSION)
  
  
  
  # Next, add to 'avgDF' the average of "ANNUAL_USE" for each application number
  # ("ANNUAL_USE" is a column in 'monthlyDF')
  avgDF <- avgDF %>%
    full_join(monthlyDF %>%
                ungroup() %>%
                select(APPLICATION_NUMBER, ANNUAL_USE) %>%
                group_by(APPLICATION_NUMBER) %>%
                summarize(TOTAL_ANNUAL_USE = mean(ANNUAL_USE)),
              by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  
  avgDF_WY <- avgDF_WY %>%
    full_join(monthlyDF_WY %>%
                ungroup() %>%
                select(APPLICATION_NUMBER, ANNUAL_USE) %>%
                group_by(APPLICATION_NUMBER) %>%
                summarize(TOTAL_ANNUAL_USE = mean(ANNUAL_USE)),
              by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  
  # Then, create a column that is the average of the standard deviations
  # for the "DIRECT" use types
  # (NA rows are ignored in these calculations)
  avgDF <- avgDF %>%
    rowwise() %>%
    mutate(AVERAGE_STDEV = mean(JAN_STDEV, FEB_STDEV, MAR_STDEV,
                                APR_STDEV, MAY_STDEV, JUN_STDEV,
                                JUL_STDEV, AUG_STDEV, SEP_STDEV,
                                OCT_STDEV, NOV_STDEV, DEC_STDEV,
                                na.rm = TRUE)) %>%
    ungroup()
  
  
  
  avgDF_WY <- avgDF_WY %>%
    rowwise() %>%
    mutate(AVERAGE_STDEV = mean(JAN_STDEV, FEB_STDEV, MAR_STDEV,
                                APR_STDEV, MAY_STDEV, JUN_STDEV,
                                JUL_STDEV, AUG_STDEV, SEP_STDEV,
                                OCT_STDEV, NOV_STDEV, DEC_STDEV,
                                na.rm = TRUE)) %>%
    ungroup()
  
  
  
  # After that, use 'statDF' to add new columns to 'avgDF'
  # These columns ("Total_Cumulative_Diverted" and "Total_Cumulative_Use") will be
  # sums of all diversion volumes ("DIRECT"/"STORAGE" and "USE")
  # for each application number
  
  
  # Add "Total_Cumulative_Diverted" first
  avgDF <- avgDF %>%
    left_join(statDF %>%
                filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE")) %>%
                select(APPLICATION_NUMBER, AMOUNT) %>%
                group_by(APPLICATION_NUMBER) %>%
                summarize(Total_Cumulative_Diverted = sum(AMOUNT, na.rm = TRUE)),
              by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  
  avgDF_WY <- avgDF_WY %>%
    left_join(statDF %>%
                filter(DIVERSION_TYPE %in% c("DIRECT", "STORAGE")) %>%
                select(APPLICATION_NUMBER, AMOUNT) %>%
                group_by(APPLICATION_NUMBER) %>%
                summarize(Total_Cumulative_Diverted = sum(AMOUNT, na.rm = TRUE)),
              by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  # Add "Total_Cumulative_Use" after that 
  avgDF <- avgDF %>%
    left_join(statDF %>%
                filter(DIVERSION_TYPE == "USE") %>%
                select(APPLICATION_NUMBER, AMOUNT) %>%
                group_by(APPLICATION_NUMBER) %>%
                summarize(Total_Cumulative_Use = sum(AMOUNT, na.rm = TRUE)),
              by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  avgDF_WY <- avgDF_WY %>%
    left_join(statDF %>%
                filter(DIVERSION_TYPE == "USE") %>%
                select(APPLICATION_NUMBER, AMOUNT) %>%
                group_by(APPLICATION_NUMBER) %>%
                summarize(Total_Cumulative_Use = sum(AMOUNT, na.rm = TRUE)),
              by = "APPLICATION_NUMBER", relationship = "one-to-one")
  
  
  
  # The final column to add is "Total_Use_as_a_Percent_of_Total_Diverted"
  # It will be a ratio of "Total_Cumulative_Use" to "Total_Cumulative_Diverted"
  # Only perform that calculation if the latter is greater than 0 and not NA
  avgDF <- avgDF %>%
    mutate(Total_Use_as_a_Percent_of_Total_Diverted = 
             if_else(!is.na(Total_Cumulative_Diverted) & Total_Cumulative_Diverted > 0,
                     Total_Cumulative_Use / Total_Cumulative_Diverted,
                     NA_real_))
  
  
  
  avgDF_WY <- avgDF_WY %>%
    mutate(Total_Use_as_a_Percent_of_Total_Diverted = 
             if_else(!is.na(Total_Cumulative_Diverted) & Total_Cumulative_Diverted > 0,
                     Total_Cumulative_Use / Total_Cumulative_Diverted,
                     NA_real_))
  
  
  
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
    write.xlsx(paste0("OutputData/", ws$ID, "_ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx"),
               overwrite = TRUE)
  
  
  
  monthlyDF %>%
    select(APPLICATION_NUMBER, INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT, 
           FACE_VALUE_AMOUNT, FACE_VALUE_UNITS, IniDiv_Converted_to_AF) %>%
    write.xlsx(paste0("OutputData/", ws$ID, "_ExpectedDemand_FV.xlsx"), overwrite = TRUE)
  
  
  
  # A spreadsheet for QAQC review will be produced next
  # Before that, read in data from a previous manual review (if it exists)
  # Exclude entries that were already checked previously
  if (!is.na(ws$QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH[1])) {
    
    reviewDF <- getXLSX(ws, "IS_SHAREPOINT_PATH_QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET",
                        "QAQC_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH", 
                        "QAQC_UNIT_CONVERSION_ERRORS_WORKSHEET_NAME")
    
    
    
    # Make a key for unique combinations of "APPLICATION_NUMBER", "YEAR", and "YEAR_TOTAL"
    reviewDF <- reviewDF %>%
      makeKey_APP_YEAR_AMOUNT()
    
    
    
    # If the second manual review was also performed, add that spreadsheet here too
    if (!is.na(ws$QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH[1])) {
      
      reviewDF2 <- getXLSX(ws, "IS_SHAREPOINT_PATH_QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET",
                           "QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_SPREADSHEET_PATH", 
                           "QAQC_MEDIAN_BASED_UNIT_CONVERSION_ERRORS_WORKSHEET_NAME") %>%
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
           QAQC_Action_Taken, QAQC_Reason) %>%
    filter((Diversion_as_Percent_of_FV > 100 & FACE_VALUE_AMOUNT > 0) | 
             (Diversion_as_Percent_of_FV < 0.01 & Diversion_as_Percent_of_FV > 0 & FACE_VALUE_AMOUNT > 0) | 
             (Diversion_as_Percent_of_IniDiv > 100 & IniDiv_Converted_to_AF > 0) | 
             (Diversion_as_Percent_of_IniDiv < 0.01 & Diversion_as_Percent_of_IniDiv > 0 & IniDiv_Converted_to_AF > 0)) %>%
    write.xlsx(paste0("OutputData/", ws$ID, "_Expected_Demand_Units_QAQC.xlsx"), overwrite = TRUE)
  
  
  
  # Then include a spreadsheet focused on "CALENDAR_YEAR_TOTAL"/"WATER_YEAR_TOTAL" for all rights in 'monthlyDF'
  monthlyDF %>%
    select(APPLICATION_NUMBER, YEAR, CALENDAR_YEAR_TOTAL, WATER_YEAR_TOTAL) %>%
    write.xlsx(paste0("OutputData/", ws$ID, "_Calendar_Year_Totals_AF.xlsx"), overwrite = TRUE)
  
  
  
  # Output a message to the console
  cat("Done!\n")
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}


monthlyUseValues <- function (statDF) {
  
  # Create a data frame with a water volume specified for each use type and month
  
  
  # Define variables that contain the different months and use types
  # (Note: The use type "Combined (Direct + Storage)" is ignored in this module)
  monthNames <- month.abb %>% toupper()
  
  useTypes <- unique(statDF$DIVERSION_TYPE) %>%
    sort() %>%
    str_subset("Combined ", negate = TRUE)
  
  
  
  # Iterate through the use types and months
  # Create a new data frame with this volume information
  for (i in 1:length(useTypes)) {
    
    for (j in 1:length(monthNames)) {
      
      # Filter 'statDF' to this iteration's month and use type
      # Then, sum the values in the "AMOUNT" column
      # Save that result to a temporary data frame
      tempDF <- statDF %>%
        filter(DIVERSION_TYPE == useTypes[i] & MONTH == j) %>%
        select(APPLICATION_NUMBER, YEAR, AMOUNT) %>%
        group_by(APPLICATION_NUMBER, YEAR) %>%
        summarize(!! paste0(monthNames[j], "_",
                         c("DIRECT_DIVERSION", "STORAGE_DIVERSION", "REPORTED_USE")[i]) :=
                    sum(AMOUNT, na.rm = TRUE), .groups = "keep")
      
      
      # NOTE
      # The use of "!!" and ":=" inside summarize() allows a string to be used
      # as a column name
      
      
      # If this is the first iteration of the loops
      if (i == 1 && j == 1) {
        
        # Define the main table DF with 'tempDF'
        useDF <- tempDF
        
        
      # Otherwise, join 'tempDF' to 'useDF'
      } else {
        
        useDF <- useDF %>%
          full_join(tempDF, by = c("APPLICATION_NUMBER", "YEAR"),
                    relationship = "one-to-one")
        
      }
      
    } # End of loop j
    
  } # End of loop i
  
  
  
  # Return 'useDF'
  return(useDF)
  
}


CY2WY <- function (monthlyDF) {
  
  # Given a data frame of monthly diversion volumes for each calendar year ('monthlyDF'),
  # create an alternative data frame that displays data by water year instead
  
  
  
  # Create two subsets of 'monthlyDF'
  # One data frame will have the data for the JAN - SEP months in that year
  # The other data frame will have OCT - DEC data
  mainDF <- monthlyDF %>%
    ungroup() %>%
    select(APPLICATION_NUMBER, YEAR,
           matches("^(JAN)|(FEB)|(MAR)|(APR)|(MAY)|(JUN)|(JUL)|(AUG)|(SEP)"))
  
  
  
  secondDF <- monthlyDF %>%
    ungroup() %>%
    select(APPLICATION_NUMBER, YEAR,
           matches("^(OCT)|(NOV)|(DEC)"))
  
  
  
  # In each row of 'secondDF', increase the value of "YEAR" by 1
  # In a water year, the last three months of the previous year are part of the current water year
  secondDF <- secondDF %>%
    mutate(YEAR = YEAR + 1)
  
  
  
  # Join 'secondDF' to 'mainDF'
  wyDF <- mainDF %>%
    left_join(secondDF, by = c("APPLICATION_NUMBER", "YEAR"))
  
  
  
  # Return 'wyDF'
  return(wyDF)
  
}


monthlyAvg <- function (monthlyDF) {
  
  # Create a data frame with an average water volume specified for each month
  # (With a different average for use types of "DIRECT" and "STORAGE")
  
  # Include the standard deviations as well (but for "DIRECT" only)
  
  
  # Define variables that contain the different months and use types
  # (Note: Only the "DIRECT" and "STORAGE" use types are considered in this step)
  monthNames <- month.abb %>% toupper()
  useTypes <- c("DIRECT", "STORAGE")
  
  
  
  # Iterate through the use types and months
  # Create a new data frame with these averages
  for (i in 1:length(useTypes)) {
    
    for (j in 1:length(monthNames)) {
      
      
      # Create a temporary string that contains the column name relevant to this iteration
      # (It is based on the month and use type)
      colStr <- paste0(monthNames[j], "_", useTypes[i], "_DIVERSION")
      
      # NOTE
      # Using eval() with this variable will be the equivalent of inputting 
      # the column name as a string in the other functions
      
      
      
      # For the next step, if the use type is "DIRECT", 
      # a standard deviation will also be calculated
      
      
      # In both cases, get this iteration's corresponding column
      # (based on the month and use type)
      # Group the data by application number and get the mean
      # Save that result to a temporary data frame
      
      
      if (useTypes[i] == "DIRECT") {
        
        tempDF <- monthlyDF %>%
          ungroup() %>%
          select(APPLICATION_NUMBER, eval(colStr)) %>%
          group_by(APPLICATION_NUMBER) %>%
          summarize(!! paste0(monthNames[j], "_AVERAGE_", useTypes[i], "_DIVERSION") := 
                      mean(.data[[colStr]]),
                    !! paste0(monthNames[j], "_STDEV") := 
                      sd(.data[[colStr]]))
        
      } else {
        
        tempDF <- monthlyDF %>%
          ungroup() %>%
          select(APPLICATION_NUMBER, eval(colStr)) %>%
          group_by(APPLICATION_NUMBER) %>%
          summarize(!! paste0(monthNames[j], "_AVERAGE_", useTypes[i], "_DIVERSION") := 
                      mean(.data[[colStr]]))
        
      }
      
      
      # NOTES
      # ungroup() is needed in case "YEAR" is currently set as one of the grouping variables
      # "!!" and ":=" are used to input a string and use it as a new column name
      # '.data' is wrapped around 'colStr' to allow use of the column referenced by 'colStr'
      
      
      
      # If this is the first iteration of the loops
      if (i == 1 && j == 1) {
        
        # Define the main table DF with 'tempDF'
        useDF <- tempDF
        
        
        # Otherwise, join 'tempDF' to 'useDF'
      } else {
        
        useDF <- useDF %>%
          full_join(tempDF, by = "APPLICATION_NUMBER", relationship = "one-to-one")
        
      }
      
    } # End of loop j
    
  } # End of loop i
  
  
  
  # Return 'useDF'
  return(useDF)
  
}


makeXLSX <- function (avgDF, fvDF, monthlyDF, statDF, expectedReports, maxYear, 
                      minYear, numRights, numYears, uniqAppNum) {
  
  # Make an XLSX file whose format is similar to the module XLSX file
  
  
  
  # Create an Excel workbook object and add a worksheet to it
  wb <- createWorkbook()
  
  addWorksheet(wb, "ReportedDiversionAnalysis")
  
  
  
  # Add title information to the first few cells
  writeData(wb, "ReportedDiversionAnalysis", "INFO:", startCol = 1, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis", 
            "INPUT DIVERSION AND USE DATA  - FROM STATISTICS_FINAL.csv", 
            startCol = 2, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis", "ACTION:", startCol = 1, startRow = 2)
  
  writeData(wb, "ReportedDiversionAnalysis", 
            "PASTE INPUT DATA FROM PRE-PROCESSING SCRIPT HERE - DELETE SAMPLE DATA BELOW", 
            startCol = 2, startRow = 2)
  
  writeData(wb, "ReportedDiversionAnalysis", "CALCULATE COMPOSITE KEY INDICES", 
            startCol = 7, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis", 
            "FILL DOWN FORMULAS TO BOTTOM OF NEW DATA RANGE, THEN CALCULATE", 
            startCol = 7, startRow = 2)
  
  
  
  # Write 'statDF' to the first part of the spreadsheet
  writeData(wb, "ReportedDiversionAnalysis",
            statDF,
            startCol = 1, startRow = 3)
  
  
  # Then write 'fvDF' next
  # ('statDF' should be 8 columns long, and there should be a single-column gap)
  # So 'fvDF' would start on Column 10/J
  writeData(wb, "ReportedDiversionAnalysis",
            fvDF,
            startCol = 10, startRow = 3)
  
  
  
  # Also add the title information associated with 'fvDF'
  writeData(wb, "ReportedDiversionAnalysis",
            "INPUT FACE VALUE AND INITIAL REPORTED DIVERSION DATA FOR SPREADSHEET - FROM EWRIMS InFLAT FILE",
            startCol = 10, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "Paste data from Statistics_FaceValue.csv",
            startCol = 10, startRow = 2)
  
  
  
  # Column 15/O should be 'uniqAppNum' (and some title information)
  writeData(wb, "ReportedDiversionAnalysis",
            "LIST OF WATER RIGHT APPLICATIONS",
            startCol = 15, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "ARRAY, DON'T FILL DOWN",
            startCol = 15, startRow = 2)
  
  writeData(wb, "ReportedDiversionAnalysis",
            uniqAppNum %>% matrix(ncol = 1) %>% data.frame() %>% set_names("UniqueApplID"),
            startCol = 15, startRow = 3)
  
  
  # Column 16/P contains a title, 'minYear', 'maxYear', and the labels 
  # "No of Water Rights", "No of Reporting Years", and "Total No of Expected Reports"
  writeData(wb, "ReportedDiversionAnalysis",
            "REPORTING YEARS IN DATASET (2014+)",
            startCol = 16, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "MIN_YEAR",
            startCol = 16, startRow = 3)
  
  writeData(wb, "ReportedDiversionAnalysis",
            minYear,
            startCol = 16, startRow = 4)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "MAX_YEAR",
            startCol = 16, startRow = 5)
  
  writeData(wb, "ReportedDiversionAnalysis",
            maxYear,
            startCol = 16, startRow = 6)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "No of Water Rights",
            startCol = 16, startRow = 8)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "No of Reporting Years",
            startCol = 16, startRow = 9)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "Total No of Expected Reports",
            startCol = 16, startRow = 10)
  
  
  
  # In Column 17/Q, add the boxes related to manual override as well as
  # the values for the variables names listed in Column 16
  writeData(wb, "ReportedDiversionAnalysis",
            "MANUAL OVERRIDE",
            startCol = 17, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "SELECT CUSTOM RANGE (A NULL VALUE USES ALL AVAILABLE REPORTS)",
            startCol = 17, startRow = 2)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "MANUAL_MIN_YEAR",
            startCol = 17, startRow = 3)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "MANUAL_MAX_YEAR",
            startCol = 17, startRow = 5)
  
  writeData(wb, "ReportedDiversionAnalysis",
            numRights,
            startCol = 17, startRow = 8)
  
  writeData(wb, "ReportedDiversionAnalysis",
            numYears,
            startCol = 17, startRow = 9)
  
  writeData(wb, "ReportedDiversionAnalysis",
            expectedReports,
            startCol = 17, startRow = 10)
  
  
  
  # In Column 19/S, there is a column of counts
  # The number of 0s in this column is equal to the number of rows in 'monthlyDF'
  writeData(wb, "ReportedDiversionAnalysis",
            "ARRAY, DON'T FILL",
            startCol = 19, startRow = 2)
  
  writeData(wb, "ReportedDiversionAnalysis",
            rep(0, nrow(monthlyDF)) %>%
              matrix(ncol = 1) %>% data.frame() %>% set_names("COUNT"),
            startCol = 19, startRow = 3)
  
  
  # Column 20/T onwards contains variables from 'monthlyDF' with the exception
  # of Column 21/U, which must be prepared right now
  # (It is simply a counter for the application ID)
  
  # This line will fill in Columns 20/T to 68/BP
  writeData(wb, "ReportedDiversionAnalysis",
            monthlyDF %>%
              rowwise() %>%
              mutate(INDEX = which(uniqAppNum == APPLICATION_NUMBER)) %>%
              select(YEAR, INDEX, APPLICATION_NUMBER, 
                     JAN_DIRECT_DIVERSION, FEB_DIRECT_DIVERSION, MAR_DIRECT_DIVERSION,
                     APR_DIRECT_DIVERSION, MAY_DIRECT_DIVERSION, JUN_DIRECT_DIVERSION,
                     JUL_DIRECT_DIVERSION, AUG_DIRECT_DIVERSION, SEP_DIRECT_DIVERSION,
                     OCT_DIRECT_DIVERSION, NOV_DIRECT_DIVERSION, DEC_DIRECT_DIVERSION,
                     JAN_STORAGE_DIVERSION, FEB_STORAGE_DIVERSION, MAR_STORAGE_DIVERSION,
                     APR_STORAGE_DIVERSION, MAY_STORAGE_DIVERSION, JUN_STORAGE_DIVERSION,
                     JUL_STORAGE_DIVERSION, AUG_STORAGE_DIVERSION, SEP_STORAGE_DIVERSION,
                     OCT_STORAGE_DIVERSION, NOV_STORAGE_DIVERSION, DEC_STORAGE_DIVERSION,
                     JAN_REPORTED_USE, FEB_REPORTED_USE, MAR_REPORTED_USE,
                     APR_REPORTED_USE, MAY_REPORTED_USE, JUN_REPORTED_USE,
                     JUL_REPORTED_USE, AUG_REPORTED_USE, SEP_REPORTED_USE,
                     OCT_REPORTED_USE, NOV_REPORTED_USE, DEC_REPORTED_USE,
                     ANNUAL_DIRECT, ANNUAL_STORAGE, DUPLICATE_STORAGE_USE,
                     DUPLICATE_DIRECT_STORAGE, CALENDAR_YEAR_TOTAL, MAY_TO_SEP_TOTAL_DIVERSION,
                     FACE_VALUE_AMOUNT, INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT,
                     IniDiv_Converted_to_AF),
            startCol = 20, startRow = 3)
  
  
  # The title cells from Columns T to BL will need to be added as well
  writeData(wb, "ReportedDiversionAnalysis",
            "RESULTS TABLE - MONTHLY REPORTED DIRECT DIVERSION BY CALENDAR YEAR",
            startCol = 20, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "FILL DOWN FORMULAS - DO NOT MODIFY FORMULAS",
            startCol = 20, startRow = 2)
  
  # Column 35/AI
  writeData(wb, "ReportedDiversionAnalysis",
            "RESULTS TABLE - MONTHLY REPORTED STORAGE DIVERSION BY CALENDAR YEAR",
            startCol = 35, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "FILL DOWN FORMULAS - DO NOT MODIFY FORMULAS",
            startCol = 35, startRow = 2)
  
  # Column 47/AU
  writeData(wb, "ReportedDiversionAnalysis",
            "RESULTS TABLE - MONTHLY REPORTED USE BY CALENDAR YEAR",
            startCol = 47, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "FILL DOWN FORMULAS - DO NOT MODIFY FORMULAS",
            startCol = 47, startRow = 2)
  
  # Column 59/BG
  writeData(wb, "ReportedDiversionAnalysis",
            "TOTAL ANNUAL DIRECT DIVERSION",
            startCol = 59, startRow = 1)
  
  # Column 60/BH
  writeData(wb, "ReportedDiversionAnalysis",
            "TOTAL ANNUAL STORAGE DIVERSION",
            startCol = 60, startRow = 1)
  
  # Column 61/BI
  writeData(wb, "ReportedDiversionAnalysis",
            "DUPLICATE DIVERSION, STORAGE, AND USE",
            startCol = 61, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            '"DUPLICATE_STOR_USE"  INDICATES POTENTIAL REPORTING ERROR - INACCURATE REPORTING OF STORAGE AND/OR USE',
            startCol = 61, startRow = 2)
  
  # Column 62/BJ
  writeData(wb, "ReportedDiversionAnalysis",
            "DUPLICATE DIVERSION, STORAGE, AND USE",
            startCol = 62, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            '"DUPLICATE_DIV_STOR"  INDICATES POTENTIAL REPORTING ERROR - DOUBLE COUNTING',
            startCol = 62, startRow = 2)
  
  # Column 63/BK
  writeData(wb, "ReportedDiversionAnalysis",
            "TOTAL ANNUAL REPORTED DIVERSION BY CALENDAR YEAR",
            startCol = 63, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "HISTORIC ANNUAL TOTAL (DIRECT + STOR) DEMAND",
            startCol = 63, startRow = 2)
  
  # Column 64/BL
  writeData(wb, "ReportedDiversionAnalysis",
            "TOTAL DRY-SEASON REPORTED DIVERSION",
            startCol = 64, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "HISTORIC DRY YEAR DEMAND",
            startCol = 64, startRow = 2)
  
  # Column 65/BM
  writeData(wb, "ReportedDiversionAnalysis",
            "EXPECTED MAXIMUM ANNUAL TOTAL DIVERSIONS",
            startCol = 65, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "BASED ON FACE VALUE OR INITIAL REPORTED DIVERSION AMOUNT",
            startCol = 65, startRow = 2)
  
  
  # For Columns 69/BQ to 78/BZ, use 'monthlyDF' again
  # Column 69/BQ will be a renamed version of "CALENDAR_YEAR_TOTAL"
  writeData(wb, "ReportedDiversionAnalysis",
            monthlyDF %>%
              ungroup() %>%
              rename(AnnualTotalDiversion = CALENDAR_YEAR_TOTAL) %>%
              select(AnnualTotalDiversion, ANNUAL_USE, Diversion_as_Percent_of_FV,
                     Amount_over_FV, Diversion_as_Percent_of_IniDiv,
                     Amount_over_IniDiv, Annual_Diversion_if_reported_in_Gallons,
                     Annual_Diversion_if_reported_in_GPM, Annual_Diversion_if_reported_in_GPD,
                     Annual_Diversion_if_reported_in_CFS),
            startCol = 69, startRow = 3)
  
  
  # Fill in the title cells over these columns next
  
  # Column 69/BQ
  writeData(wb, "ReportedDiversionAnalysis",
            "TOTAL ANNUAL DIVERSION",
            startCol = 69, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "FOR REFERENCE/ INTERMEDIATE CALC",
            startCol = 69, startRow = 2)
  
  # Column 70/BR
  writeData(wb, "ReportedDiversionAnalysis",
            "TOTAL ANNUAL USE",
            startCol = 70, startRow = 1)
  
  # Column 71/BS
  writeData(wb, "ReportedDiversionAnalysis",
            "CHECK IF DIVERSION EXCEEDS FACE VALUE",
            startCol = 71, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "RATIO SHOULD NOT EXCEED 1",
            startCol = 71, startRow = 2) 
  
  # Column 72/BT
  writeData(wb, "ReportedDiversionAnalysis",
            "PRIORITIZE REVIEW BASED ON VOLUME",
            startCol = 72, startRow = 2) 
  
  # Column 73/BU
  writeData(wb, "ReportedDiversionAnalysis",
            "CHECK IF DIVERSION EXCEEDS INITIAL REPORTED DIVERSION",
            startCol = 73, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "RATIO SHOULD NOT EXCEED 1",
            startCol = 73, startRow = 2) 
  
  # Column 74/BV
  writeData(wb, "ReportedDiversionAnalysis",
            "PRIORITIZE REVIEW BASED ON VOLUME",
            startCol = 74, startRow = 2) 
  
  # Column 75/BW
  writeData(wb, "ReportedDiversionAnalysis",
            "GALLONS UNIT CONVERSION ERROR",
            startCol = 75, startRow = 1) 
  
  writeData(wb, "ReportedDiversionAnalysis",
            "CHECK IF UNITS OF GALLONS MAKE MORE SENSE THAN ACRE-FEET",
            startCol = 75, startRow = 2) 
  
  # Column 76/BX
  writeData(wb, "ReportedDiversionAnalysis",
            "GALLONS PER MINUTE UNIT CONVERSION ERROR",
            startCol = 76, startRow = 1) 
  
  writeData(wb, "ReportedDiversionAnalysis",
            "CHECK IF UNITS OF GPM MAKE MORE SENSE THAN ACRE-FEET",
            startCol = 76, startRow = 2) 
  
  # Column 77/BY
  writeData(wb, "ReportedDiversionAnalysis",
            "GALLONS PER DAY UNIT CONVERSION ERROR",
            startCol = 77, startRow = 1) 
  
  writeData(wb, "ReportedDiversionAnalysis",
            "CHECK IF UNITS OF GPD MAKE MORE SINCE THAN ACRE-FEET",
            startCol = 77, startRow = 2) 
  
  # Column 78/BZ
  writeData(wb, "ReportedDiversionAnalysis",
            "CUBIC FEET PER SECOND UNIT CONVERSION ERROR",
            startCol = 78, startRow = 1) 
  
  writeData(wb, "ReportedDiversionAnalysis",
            "CHECK IF UNITS OF CFS MAKE MORE SINCE THAN ACRE-FEET",
            startCol = 78, startRow = 2) 
  
  
  
  # After that, data from 'avgDF' will be input
  # It will fill in columns from Column 80/CB to Column 135/EE
  writeData(wb, "ReportedDiversionAnalysis",
            avgDF %>%
              select(APPLICATION_NUMBER, 
                     JAN_AVERAGE_DIRECT_DIVERSION, FEB_AVERAGE_DIRECT_DIVERSION,
                     MAR_AVERAGE_DIRECT_DIVERSION, APR_AVERAGE_DIRECT_DIVERSION,
                     MAY_AVERAGE_DIRECT_DIVERSION, JUN_AVERAGE_DIRECT_DIVERSION,
                     JUL_AVERAGE_DIRECT_DIVERSION, AUG_AVERAGE_DIRECT_DIVERSION,
                     SEP_AVERAGE_DIRECT_DIVERSION, OCT_AVERAGE_DIRECT_DIVERSION,
                     NOV_AVERAGE_DIRECT_DIVERSION, DEC_AVERAGE_DIRECT_DIVERSION,
                     JAN_AVERAGE_STORAGE_DIVERSION, FEB_AVERAGE_STORAGE_DIVERSION,
                     MAR_AVERAGE_STORAGE_DIVERSION, APR_AVERAGE_STORAGE_DIVERSION,
                     MAY_AVERAGE_STORAGE_DIVERSION, JUN_AVERAGE_STORAGE_DIVERSION,
                     JUL_AVERAGE_STORAGE_DIVERSION, AUG_AVERAGE_STORAGE_DIVERSION,
                     SEP_AVERAGE_STORAGE_DIVERSION, OCT_AVERAGE_STORAGE_DIVERSION,
                     NOV_AVERAGE_STORAGE_DIVERSION, DEC_AVERAGE_STORAGE_DIVERSION,
                     JAN_EXPECTED_TOTAL_DIVERSION, FEB_EXPECTED_TOTAL_DIVERSION,
                     MAR_EXPECTED_TOTAL_DIVERSION, APR_EXPECTED_TOTAL_DIVERSION,
                     MAY_EXPECTED_TOTAL_DIVERSION, JUN_EXPECTED_TOTAL_DIVERSION,
                     JUL_EXPECTED_TOTAL_DIVERSION, AUG_EXPECTED_TOTAL_DIVERSION,
                     SEP_EXPECTED_TOTAL_DIVERSION, OCT_EXPECTED_TOTAL_DIVERSION,
                     NOV_EXPECTED_TOTAL_DIVERSION, DEC_EXPECTED_TOTAL_DIVERSION,
                     ANNUAL_TOTAL_DIVERSION, MAY_TO_SEP_TOTAL_DIVERSION, TOTAL_ANNUAL_USE,
                     JAN_STDEV, FEB_STDEV, MAR_STDEV, APR_STDEV, MAY_STDEV, JUN_STDEV,
                     JUL_STDEV, AUG_STDEV, SEP_STDEV, OCT_STDEV, NOV_STDEV, DEC_STDEV,
                     AVERAGE_STDEV, Total_Cumulative_Diverted, Total_Cumulative_Use,
                     Total_Use_as_a_Percent_of_Total_Diverted),
            startCol = 80, startRow = 3) 
  
  
  # Add title cells for these columns
  
  # Column 80/CB
  writeData(wb, "ReportedDiversionAnalysis",
            "RESULTS TABLE - AVERAGE MONTHLY REPORTED DIRECT DIVERSION BY WATER RIGHT",
            startCol = 80, startRow = 1) 
  
  writeData(wb, "ReportedDiversionAnalysis",
            "FOR USE IN ESTIMATING EXPECTED DEMAND",
            startCol = 80, startRow = 2) 
  
  # Column 93/CO
  writeData(wb, "ReportedDiversionAnalysis",
            "RESULTS TABLE - AVERAGE MONTHLY REPORTED STORAGE DIVERSION BY WATER RIGHT",
            startCol = 93, startRow = 1) 
  
  writeData(wb, "ReportedDiversionAnalysis",
            "FOR USE IN ESTIMATING EXPECTED DEMAND",
            startCol = 93, startRow = 2) 
  
  # Column 105/DA
  writeData(wb, "ReportedDiversionAnalysis",
            "RESULTS TABLE - AVERAGE MONTHLY REPORTED TOTAL DIVERSION (DIRECT+STORAGE) BY WATER RIGHT",
            startCol = 105, startRow = 1) 
  
  writeData(wb, "ReportedDiversionAnalysis",
            "FOR USE IN ESTIMATING EXPECTED DEMAND",
            startCol = 105, startRow = 2) 
  
  # Column 117/DM, 118/DN, and 119/DO
  writeData(wb, "ReportedDiversionAnalysis",
            "AVERAGE ANNUAL REPORTED DIVERSION (DIRECT + STORAGE)",
            startCol = 117, startRow = 1) 
  
  writeData(wb, "ReportedDiversionAnalysis",
            "AVERAGE DRY-SEASON REPORTED DIVERSION (DIRECT + STORAGE)",
            startCol = 118, startRow = 1) 
  
  writeData(wb, "ReportedDiversionAnalysis",
            "AVERAGE ANNUAL REPORTED USE",
            startCol = 119, startRow = 1)
  
  # Column 120/DP
  writeData(wb, "ReportedDiversionAnalysis",
            "STANDARD DEVIATION CALCULATIONS",
            startCol = 120, startRow = 1) 
  
  writeData(wb, "ReportedDiversionAnalysis",
            "FILL DOWN FORMULA - DO NOT MODIFY FORMULA",
            startCol = 120, startRow = 2) 
  
  # Column 122/DR
  writeData(wb, "ReportedDiversionAnalysis",
            "STANDARD DEVIATION PER MONTH ACROSS REPORTS (YEARS)",
            startCol = 122, startRow = 1) 
  
  # Column 132/EB
  writeData(wb, "ReportedDiversionAnalysis",
            "AVERAGE STANDARD DEVIATION PER MONTH ACROSS REPORTS (YEARS)",
            startCol = 132, startRow = 1)
  
  writeData(wb, "ReportedDiversionAnalysis",
            "Sort by this field - The larger this value, the more variation in the reporting from year to year (potential error)",
            startCol = 132, startRow = 2)
  
  # Column 133/EC
  writeData(wb, "ReportedDiversionAnalysis",
            "COMPARING TOTAL USE TO TOTAL DIVERSION",
            startCol = 133, startRow = 1)
  
  # Column 135/EE
  writeData(wb, "ReportedDiversionAnalysis",
            "RATIO OF TOTAL USE TO TOTAL DIVERSION SHOULD APPROACH 1 OVER TIME. CANNOT USE MORE WATER THAN DIVERTED",
            startCol = 135, startRow = 2)
  
  
  
  # Finally, save the workbook as a file
  saveWorkbook(wb, 
               "OutputData/ExpectedDemand_ExceedsFV_UnitConversion_StorVsUseVsDiv_Statistics_Scripted.xlsx", 
               overwrite = TRUE)
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}



makeKey_APP_YEAR_AMOUNT <- function (dataDF) {
  
  # Make a column that identifies unique combinations of:
  #   (1) APPLICATION_NUMBER
  #   (2) YEAR
  #   (3) YEAR_TOTAL
  
  
  # Older spreadsheets either had "CALENDAR_YEAR_TOTAL" only
  # or both "CALENDAR_YEAR_TOTAL" and "WATER_YEAR_TOTAL" as separate columns
  # Newer versions of this script produce a single column "YEAR_TOTAL" instead
  # If this spreadsheet is an older version, make it like newer review sheets (with "YEAR_TOTAL")
  if ("YEAR_TOTAL" %in% names(dataDF)) {
    
    dataDF <- dataDF %>%
      select(APPLICATION_NUMBER, YEAR, YEAR_TOTAL)
    
    
    # The spreadsheet has both columns
  } else if ("WATER_YEAR_TOTAL" %in% names(dataDF) && "CALENDAR_YEAR_TOTAL" %in% names(dataDF)) {
    
    dataDF <- dataDF %>%
      mutate(YEAR_TOTAL = if_else(is.na(WATER_YEAR_TOTAL), CALENDAR_YEAR_TOTAL, WATER_YEAR_TOTAL)) %>%
      select(APPLICATION_NUMBER, YEAR, YEAR_TOTAL)
    
    
    # Only "CALENDAR_YEAR_TOTAL"
  } else if ("CALENDAR_YEAR_TOTAL" %in% names(dataDF)) {
    
    dataDF <- dataDF %>%
      rename(YEAR_TOTAL = CALENDAR_YEAR_TOTAL) %>%
      select(APPLICATION_NUMBER, YEAR, YEAR_TOTAL)
    
    # Only "WATER_YEAR_TOTAL"
  } else {
    
    dataDF <- dataDF %>%
      rename(YEAR_TOTAL = WATER_YEAR_TOTAL) %>%
      select(APPLICATION_NUMBER, YEAR, YEAR_TOTAL)
    
  }
  
  
  
  # Create a column titled "KEY" with these three variables
  # Then return 'dataDF'
  return(dataDF %>%
           mutate(KEY = paste0(APPLICATION_NUMBER, "_", YEAR, "_", YEAR_TOTAL)))
  
  
}



compareKeys <- function (mainDF, compareDF) {
  
  # Compare "KEY" columns containing an "APPLICATION_NUMBER", "YEAR", and "YEAR_TOTAL" value
  # Because of potential rounding differences between CSV and XLSX files, this comparison
  # will be repeated with different levels of rounding
  
  
  
  # Iterate with different rounding digits used
  for (i in 0:16) {
    
    mainDF <- mainDF %>%
      mutate(KEY = paste0(APPLICATION_NUMBER, "_", YEAR, "_", round(YEAR_TOTAL, digits = i))) %>%
      filter(!(KEY %in% compareDF$KEY)) %>%
      select(-KEY)
    
  }
  
  
  
  # Return 'mainDF' afterwards
  return(mainDF)
  
}



#### Script Execution ####

cat("Starting 'Expected_Demand.R'...")

mainProcedure(ws)


print("The Expected_Demand.R script is done running!")


remove(mainProcedure, makeXLSX, monthlyAvg, monthlyUseValues, CY2WY)



# Code to look at diversion percentage variables
# (Use this code after the completion of 'monthlyDF' around Line 191)


# Considering applications with a Face Value amount specified
# (Some statement application numbers have no Initial Diversion amount specified, and they have a Face Value of 0 in the database; 
# they are removed from this count)

# # The first count is of rows with a ratio exceeding 1 (possible sign of unit conversion errors)
# monthlyDF %>% filter(!is.na(Diversion_as_Percent_of_FV) & !grepl("^S", APPLICATION_NUMBER)) %>% 
#   filter(Diversion_as_Percent_of_FV > 1) %>%
#   nrow()
# 
# # This count is of rows with a ratio less than or equal to 1 (unlikely to have unit conversion errors)
# monthlyDF %>% filter(!is.na(Diversion_as_Percent_of_FV) & !grepl("^S", APPLICATION_NUMBER)) %>% 
#   filter(Diversion_as_Percent_of_FV <= 1) %>%
#   nrow()
# 
# 
# # These next lines focus on statement application numbers and their Initial Diversion amount ratios
# 
# # This count is for rows with a ratio above 1 (flag for unit conversion errors)
# monthlyDF %>% filter(!is.na(Diversion_as_Percent_of_IniDiv)) %>% 
#   filter(Diversion_as_Percent_of_IniDiv > 1) %>%
#   nrow()
# 
# 
# # This count is for rows with a ratio less than or equal to 1
# monthlyDF %>% filter(!is.na(Diversion_as_Percent_of_IniDiv)) %>% 
#   filter(Diversion_as_Percent_of_IniDiv <= 1) %>%
#   nrow()
# 
# 
# # This is a count of records where the right is a statement, but it has no Initial Diversion amount specified
# monthlyDF %>% filter(is.na(Diversion_as_Percent_of_IniDiv) & grepl("^S", APPLICATION_NUMBER)) %>%
#   nrow()
# 
# 
# # This is a subset of the above count; it gives the number of records with a specified Face Value amount (of 0 AF)
# monthlyDF %>% filter(is.na(Diversion_as_Percent_of_IniDiv) & grepl("^S", APPLICATION_NUMBER)) %>%
#   filter(!is.na(Diversion_as_Percent_of_FV)) %>%
#   nrow()
# 
# 
# # This is a different subset, noting the number of records with a sizable DD, Storage, or Use value
# monthlyDF %>% filter(is.na(Diversion_as_Percent_of_IniDiv) & grepl("^S", APPLICATION_NUMBER)) %>%
#   filter(ANNUAL_DIRECT > 5 | ANNUAL_STORAGE > 5 | ANNUAL_USE > 5) %>%
#   nrow()


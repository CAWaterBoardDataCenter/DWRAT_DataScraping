# Identify water rights with duplicate values


# This script is a recreation of the "DuplicateReport_SameOwner" XLSX file

# It will produce a spreadsheet called "DuplicateReport_SameOwner_Scripted.xlsx" that 
# mirrors the format of the original "DuplicateReport_SameOwner.xlsx" file

# However, there is a key difference
# This script will attempt to download updated owner information each time it is run


# NOTE
# This script requires you to be connected to the VPN
# (This is necessary to collect the eWRIMS Flat File for the latest owner information)


# (It is also recommended that this script is run on the same day that its input file 
#  "Statistics_FINAL.csv" is downloaded to ensure consistency)


#### Dependencies ####


require(tidyverse)
require(openxlsx)
require(readxl)


#### Script Procedure ####


mainProcedure <- function () {
  
  # The main body of the script
  
  
  # Begin the procedure by collecting updated application and owner information
  # This data will be downloaded from an eWRIMS Flat File
  ownerDF <- getEWRIMS()
  
  
  # Then, read in the input file for this module ("Statistics_FINAL.csv")
  statDF <- read.csv("IntermediateData/Statistics_FINAL.csv")
  
  
  # Also, use the output of "DuplicateMonths_Years.R" ("DuplicateMonths_Years_Scripted.xlsx")
  # Some of the columns are shared between these two sheets
  dupMonths <- read_xlsx("OutputData/DuplicateMonths_Years_Scripted.xlsx")
  
  
  # The following columns will be added to 'statDF':
  # "Owner"
  # "AnnualReportedTotalDirect"
  # "AnnualTotalStorage"
  # "AnnualTotalDiversion"
  # "Duplicate_Reports_Same_Owner"
  
  
  # First, create "Owner"
  # Joining 'ownerDF' to 'statDF' will create this column
  # (Then "APPLICATION_PRIMARY_OWNER" can be renamed to "Owner")
  statDF <- statDF %>% left_join(ownerDF, by = "APPLICATION_NUMBER", relationship = "many-to-one") %>%
    rename(Owner = APPLICATION_PRIMARY_OWNER)
  
  
  # NOTE
  # It should be a "many-to-one" relationship, 
  # meaning that multiple rows of 'statDF' will match with one row of 'ownerDF'
  
  
  
  # Verify that every column of 'statDF' has an owner specified (so no NA values)
  stopifnot(!anyNA(statDF$Owner))
  
  
  
  # The next three columns ("AnnualReportedTotalDirect", "AnnualTotalStorage",
  # and "AnnualTotalDiversion") were calculated in 'dupMonths'
  
  # Join these columns to 'statDF' using their shared columns
  
  # In 'dupMonths', these column values should be the same for rows with the 
  # same "APPLICATION_NUMBER" and "YEAR" values
  
  # But to simplify the process, join by more shared columns
  # That way, each row of 'statDF' will have exactly one match in 'dupMonths'
  # (By doing so, "one-to-one" can be specified in left_join() for an extra error check)
  statDF <- left_join(statDF,
                      dupMonths %>%
                        select(APPLICATION_NUMBER, YEAR, MONTH, DIVERSION_TYPE, 
                               AnnualReportedTotalDirect, AnnualTotalStorage, AnnualTotalDiversion) %>%
                        unique(), 
                      by = c("APPLICATION_NUMBER", "YEAR", "MONTH", "DIVERSION_TYPE"), relationship = "one-to-one")
  
  
  
  # Ensure that the new columns have been completely filled
  stopifnot(!anyNA(statDF$AnnualReportedTotalDirect))
  stopifnot(!anyNA(statDF$AnnualTotalStorage))
  stopifnot(!anyNA(statDF$AnnualTotalDiversion))
  
  
  
  # Calculate the final column next ("Duplicate_Reports_Same_Owner")
  # This column involves checking "AnnualTotalDiversion"
  # If that column has a value of 0, this column is also 0
  # Otherwise, this column is one-twelfth of the counts of rows with this row's
  # values for "AnnualTotalDiversion", "APPLICATION_NUMBER", "Owner", and "YEAR"
  
  
  # Because multiple rows will have the same calculation and result,
  # create a separate variable to perform this counting quickly
  
  # Select the four considered columns and use summarize() to get a count
  # of the frequencies of combinations of these four columns' values
  # (Note that the "1/12" has already been applied in the formula for "Duplicate_Reports_Same_Owner")
  dupOwnerCounts <- statDF %>% 
    select(AnnualTotalDiversion, APPLICATION_NUMBER, Owner, YEAR) %>% 
    group_by(YEAR, AnnualTotalDiversion, APPLICATION_NUMBER, Owner) %>% 
    summarize(Duplicate_Reports_Same_Owner = n() / 12, .groups = "keep")
  
  
  # However, "Duplicate_Reports_Same_Owner" should have a value of 0
  # if "AnnualTotalDiversion" has a value of 0
  # Replace values in "Duplicate_Reports_Same_Owner" accordingly to reflect that
  dupOwnerCounts <- dupOwnerCounts %>% 
    mutate(Duplicate_Reports_Same_Owner = if_else(AnnualTotalDiversion > 0,
                                                  Duplicate_Reports_Same_Owner,
                                                  0))
  
  
  # As a final step, join 'dupOwnerCounts' to 'statDF' so that the table
  # gains the new column "Duplicate_Reports_Same_Owner"
  # This relationship should be "many-to-one" (multiple rows in 'statDF' will
  # match with the same row in 'dupOwnerCounts')
  statDF <- left_join(statDF, dupOwnerCounts,
                      by = c("APPLICATION_NUMBER", "YEAR", "Owner", "AnnualTotalDiversion"),
                      relationship = "many-to-one")
  
  
  
  # Perform one final error check
  # ("Duplicate_Reports_Same_Owner" should have no missing values)
  stopifnot(!anyNA(statDF$Duplicate_Reports_Same_Owner))
  
  
  
  # Finally, write 'statDF' to an XLSX file
  write.xlsx(statDF, 
             "OutputData/DuplicateReport_SameOwner_Scripted.xlsx", 
             overwrite = TRUE)
  
  
  
  cat("Done!\n")
  
  
  
  # Return nothing
  return(invisible(NULL))
}


getEWRIMS <- function () {
  
  # Create a data frame containing two columns:
  # "APPLICATION_NUMBER"
  # "APPLICATION_PRIMARY_OWNER"
  
  
  # Download an eWRIMS flat file (VPN required)
  ewrimsDF <- read_csv("RawData/ewrims_flat_file.csv", col_types = cols(.default = col_character()))
  
  
  # The two required columns are already present in this variable
  # However, "APPLICATION_PRIMARY_OWNER" may have empty values in some rows
  
  
  # First try to use the "PRIMARY_OWNER_NAME" column to fill in missing values
  ewrimsDF <- ewrimsDF %>%
    mutate(APPLICATION_PRIMARY_OWNER = if_else(is.na(APPLICATION_PRIMARY_OWNER), 
                                               PRIMARY_OWNER_NAME, 
                                               APPLICATION_PRIMARY_OWNER))
  
  
  
  # Before proceeding to the next steps,
  # Restrict 'ewrimsDF' to unique combinations of "APPLICATION_NUMBER" and "APPLICATION_PRIMARY_OWNER"
  # And remove rows that are NA for both columns
  ewrimsDF <- ewrimsDF %>%
    select(APPLICATION_NUMBER, APPLICATION_PRIMARY_OWNER) %>%
    unique() %>%
    filter(!(is.na(APPLICATION_NUMBER) & is.na(APPLICATION_PRIMARY_OWNER)))
  
  
  # Also, confirm that each "APPLICATION_NUMBER" only appears once in 'ewrimsDF'
  stopifnot(length(unique(ewrimsDF$APPLICATION_NUMBER)) == nrow(ewrimsDF))
  
  
  
  # After that, return 'ewrimsDF'
  return(ewrimsDF)
  
  
  
  # NOTE
  # There are application numbers with NA owner names
  # I originally tried to use the eWRIMS parties flat file to get more names,
  # but they didn't have anything new for primary owner names
  
  
  # I didn't delete that procedure; it is commented out below
  # I'm leaving it here for posterity and as a reminder:
  
  
  # # Next, if there are no more NA values present in the "APPLICATION_PRIMARY_OWNER" column,
  # if (!anyNA(ewrimsDF$APPLICATION_PRIMARY_OWNER)) {
  #   
  #   # Simply return 'ewrimsDF'
  #   return(ewrimsDF)
  #   
  # }
  # 
  # 
  # 
  # # Otherwise, extra work is necessary
  # 
  # 
  # 
  # # Get the eWRIMS Parties flat file
  # # (This is an extra big file, so it may take a few minutes to load into R)
  # partiesDF <- "https://intapps.waterboards.ca.gov/downloadFile/faces/flatFilesEwrims.xhtml?fileName=ewrims_flat_file_party.csv" %>%
  #   read_csv(col_types = cols(.default = col_character()))
  # 
  # 
  # # In addition to "APPLICATION_PRIMARY_OWNER" and "PRIMARY_OWNER_NAME",
  # # this table has "FIRST_NAME", "MIDDLE_NAME", and "LAST_NAME_OR_COMPANY_NAME"
  # # Those columns will be merged into one and input into 'ewrimsDF'
  # 
  # 
  # # However, 'partiesDF' contains more than primary owners
  # # Filter down 'partiesDF' to include only "Primary Owner" (or NA) for "RELATIONSHIP_TYPE"
  # partiesDF <- partiesDF %>%
  #   filter(is.na(RELATIONSHIP_TYPE) | RELATIONSHIP_TYPE == "Primary Owner")
  # 
  # 
  # # First, replace the NA values in those three columns with empty strings ("")
  # # (This will be useful when merging the name columns together)
  # partiesDF <- partiesDF %>%
  #   mutate(FIRST_NAME = replace_na(FIRST_NAME, ""),
  #          MIDDLE_NAME = replace_na(MIDDLE_NAME, ""),
  #          LAST_NAME_OR_COMPANY_NAME = replace_na(LAST_NAME_OR_COMPANY_NAME, ""))
  # 
  # 
  # # Then, create a new column "MERGED_NAME"
  # # It will be a combination of these three columns (using paste(), with sep = " ")
  # # Also, remove excess spaces that may appear in the name (due to the empty string values)
  # partiesDF <- partiesDF %>%
  #   mutate(MERGED_NAME = paste(FIRST_NAME, MIDDLE_NAME, LAST_NAME_OR_COMPANY_NAME) %>%
  #            str_remove_all("^\\s+") %>% str_remove_all("\\s{2,}"))
  # 
  # 
  # # Extract the "APPLICATION_NUMBER" and "MERGED_NAME" columns from 'partiesDF'
  # # Filter its values to only applications with NA owner names in 'ewrimsDF'
  # partiesDF <- partiesDF %>%
  #   filter(APPLICATION_NUMBER %in% ewrimsDF$APPLICATION_NUMBER[is.na(ewrimsDF$APPLICATION_PRIMARY_OWNER)]) %>%
  #   select(APPLICATION_NUMBER, MERGED_NAME) %>%
  #   unique()
  # 
  # 
  # stopifnot(length(unique(partiesDF$APPLICATION_NUMBER)) == nrow(partiesDF))
  # 
  # 
  # # Then, join 'ewrimsDF' and 'partiesDF' together
  # ewrimsDF %>%
  #   left_join(partiesDF, by = "APPLICATION_NUMBER")
  # 
  # 
  # 
  # 
  # # Select the two desired columns from the resultant data frame
  # # Also, remove repeats from the data frame
  # 
  # 
  # 
  # 
  # # Verify that each application number appears only once in 'ewrimsDF'
  # stopifnot(length(unique(ewrimsDF$APPLICATION_NUMBER)) == nrow(ewrimsDF))
  # 
  # 
  # # Also, remove a row if its application number and owner name are both NA
  # # After that, return 'ewrimsDF'
  # return(ewrimsDF %>%
  #          filter(!(is.na(APPLICATION_NUMBER) & is.na(APPLICATION_PRIMARY_OWNER))))
  
}



#### Script Execution ####



cat("Starting 'DuplicateReport_SameOwner.R'...")


mainProcedure()



remove(mainProcedure, getEWRIMS)
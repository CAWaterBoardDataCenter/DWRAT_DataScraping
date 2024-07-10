# Last Updated On: 7/10/2024
# Last Update By: Payman Alemi

# The purpose of this script is to check the 2 flat files that we are obtaining from Snowflake
# for accuracy; we had assumed that the ReportManager and Snowflake versions
# of the flat files were equivalent but we have seen numerous instances where they're not.
# Lack of identical data from the ingestion point has led to issues with many of the flagging 
# and acquisition scripts. We have to identify differences between ReportManager and Snowflake
# and notify Jake Pasner so those differences can be resolved. ewrims data is ewrims data--
# the storage location should NOT alter the data structure or values!!!!

# 1st Flat File = water_use_report_extended_csv
    # ReportManager name:
    # Snowflake creation process:
        #1) Create a stored procedure that calls EWRIMS_FLAT_FILE_WATER_USE_REPORT_EXTENDED
        #2) Execute the stored procedure with a call (aka Task)

# for accuracy, which is supplied by the water_use_report_extended flat file
# on Sn
party_info = res %>% select(APPLICATION_PRIMARY_OWNER, PARTY_ID) %>%
                     unique() %>%
                     group_by(APPLICATION_PRIMARY_OWNER, PARTY_ID) %>%
                     summarise(OwnerCount = n()) %>% arrange(desc(OwnerCount))
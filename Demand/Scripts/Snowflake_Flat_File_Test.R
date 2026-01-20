# Last Updated By: Payman Alemi on 12/22/2025

# PURPOSE: ---------------------------------------------------------------------
# This script tests the CalWATRS flat files that the Data Governance Team (DGT)
# has loaded into Snowflake


# Load Libraries----
require(odbc)
require(DBI)
require(tidyverse)
require(sf)
require(openxlsx)
require(mapview)
require(lwgeom)
require(httr)
require(data.table)
require(odbc)
require(DBI)
require(readxl)
require(janitor)
require(writexl)

# Connect to Snowflake----
sf_con <- DBI::dbConnect(drv = odbc::odbc(), 
               dsn = "snowflake", 
               server = "gb51005.west-us-2.azure.snowflakecomputing.com", 
               Trusted_Connection = "True", 
               authenticator = "externalbrowser",
               database = "SWRCB_INTERNAL_TEST",
               schema = "WR_CALWATRS_FLATFILES",
               role = "SWRCB_INTERNAL_TEST_WR_CALWATRS_FLATFILES_R_ACROLE",
               warehouse = "WR_WH")


# Send a SQL Query to Snowflake
query <- dbSendQuery(sf_con,  
                    "SELECT DISTINCT * FROM SWRCB_INTERNAL_TEST.
                    WR_CALWATRS_FLATFILES.ANNUAL_REPORTS")

# Load the result
AR_flat_file <- dbFetch(query)

# Export a subset of AR_flat_file as a CSV for inspection
#ar_subset <- AR_flat_file %>% filter(ANNUALREPORT_WR_WATERRIGHTID__C %in% c("A001029", "S023515"))

ar_subset <- AR_flat_file %>% relocate(ANNUALREPORT_WR_WATERRIGHTID__C,
                                    .before = everything())

ar_subset <- head(ar_subset,100)

write.xlsx( x = ar_subset,
            file = "OutputData/ar_subset.xlsx",
            sheetName = "ar_subset",
            overWrite = TRUE)

# Remove excessive columns
# Define the list of columns to keep
cols_to_keep <- c(
  "ANNUALREPORT_WR_WATERRIGHTID__C",
  "ANNUALREPORT_WR_APPLICATION_STATUS__C",
  "ANNUALREPORT_WR_APRDIVERSIONVOLUME__C",
  "ANNUALREPORT_WR_APRSTORAGEVOLUME__C",
  "ANNUALREPORT_WR_APRUSEVOLUME__C",
  "ANNUALREPORT_WR_AUGDIVERSIONVOLUME__C",
  "ANNUALREPORT_WR_AUGSTORAGEVOLUME__C",
  "ANNUALREPORT_WR_AUGUSEVOLUME__C",
  "ANNUALREPORT_WR_DECDIVERSIONVOLUME__C",
  "ANNUALREPORT_WR_DECSTORAGEVOLUME__C",
  "ANNUALREPORT_WR_FEBDIVERSIONVOLUME__C",
  "ANNUALREPORT_WR_FEBSTORAGEVOLUME__C",
  "ANNUALREPORT_WR_JANDIVERSIONVOLUME__C",
  "ANNUALREPORT_WR_JANSTORAGEVOLUME__C",
  "ANNUALREPORT_WR_JULDIVERSIONVOLUME__C",
  "ANNUALREPORT_WR_JULSTORAGEVOLUME__C",
  "ANNUALREPORT_WR_JUNDIVERSIONVOLUME__C",
  "ANNUALREPORT_WR_JUNSTORAGEVOLUME__C",
  "ANNUALREPORT_WR_MARDIVERSIONVOLUME__C",
  "ANNUALREPORT_WR_MARSTORAGEVOLUME__C",
  "ANNUALREPORT_WR_MAYDIVERSIONVOLUME__C",
  "ANNUALREPORT_WR_MAYSTORAGEVOLUME__C",
  "ANNUALREPORT_WR_MONTHLYDATAUNIT__C",
  "ANNUALREPORT_WR_MONTHLYSTORAGEUNIT__C",
  "ANNUALREPORT_WR_MONTHLY_DATA__C",
  "ANNUALREPORT_WR_NOVDIVERSIONVOLUME__C",
  "ANNUALREPORT_WR_NOVSTORAGEVOLUME__C",
  "ANNUALREPORT_WR_OCTDIVERSIONVOLUME__C",
  "ANNUALREPORT_WR_OCTSTORAGEVOLUME__C",
  "ANNUALREPORT_WR_SEPDIVERSIONVOLUME__C",
  "ANNUALREPORT_WR_SEPSTORAGEVOLUME__C",
  "ANNUALREPORT_WR_SEPUSEVOLUME__C",
  "ANNUALREPORT_WR_YEAR__C",
  "ANNUALREPORT_WR_ANNUALDIVERSIONVOLUMETOTAL__C",
  "ANNUALREPORT_WR_ANNUALREPORTYEAR__C",
  "ANNUALREPORT_WR_ANNUALSTORAGEVOLUMETOTAL__C",
  "ANNUALREPORT_WR_ANNUALUSEVOLUMETOTAL__C",
  "ANNUALREPORT_WR_PRIMARYOWNER__C",
  "ANNUALREPORT_WR_ANNUALDIVERSIONVOLUMEUNIT__C",
  "ANNUALREPORT_WR_ANNUALSTORAGEVOLUMEUNIT__C",
  "ANNUALREPORT_WR_ANNUALUSEVOLUMETOTALUNIT__C",
  "ANNUALREPORT_LAST_SUCCESSFUL_SYNC",
  "WR_APP_CLAIM_NAME",
  "WR_APP_CLAIM_STATUS",
  "APP_APPLIEDDATE",
  "APP_APPROVEDDATE",
  "APP_PAYMENTDATE",
  "APP_180DAYPERMITTYPE__C",
  "APP_ACCEPTANCEDATE__C"
)

ar_subset <- AR_flat_file %>%
  select(all_of(cols_to_keep))

# Remove "ANNUAL_REPORT_WR_" prefix from all the column names
ar_subset <- ar_subset %>%
  rename_with(~ str_remove(.x, "^ANNUALREPORT_WR_"))

#Export ar_subset columns to CSV
#write.csv(x = names(ar_subset), file = "IntermediateData/ar_subset_cols.csv", row.names = FALSE)


# Import ewrims_water_use_report_extended flat file ----
ewrims_water_use_report_extended <- read.csv("RawData/water_use_report_extended.csv")

# Unique water right IDs in ewrims_water_use_report_extended
ewure_right_count <- ewrims_water_use_report_extended %>% 
  distinct(APPLICATION_NUMBER) %>%
  nrow()

print(ewure_right_count)
#38,462 unique water rights in ewrims water use_report_extended flat file

# Unique water rights in AR_flat_file
ar_ff_right_count <- ar_subset %>%
  distinct(WATERRIGHTID__C) %>%
  nrow()

print(ar_ff_right_count) #44,519 unique water rights in CalWATRS Annual Report Flat File

# Are there any rights in eWRIMS that don't exist in CalWATRS? 
ewrims_only <- anti_join(x = ewrims_water_use_report_extended, 
                         y = ar_subset,
                        by = c("APPLICATION_NUMBER" = "WATERRIGHTID__C")) %>% 
                select("APPLICATION_NUMBER") %>%
                distinct()

print(ewrims_only) # Reveals 0 rights 

# Are there any rights in CalWATRS that don't exist in eWRIMS?
calwatrs_only <- anti_join(x = ar_subset,
                           y = ewrims_water_use_report_extended, 
                          by = c("WATERRIGHTID__C" = "APPLICATION_NUMBER")) %>% 
                select("WATERRIGHTID__C") %>%
                distinct()

print(calwatrs_only) #6,093 rights

# Whittle to Russian River Rights-----

# Load the RR MDT_2017_2024
rr_mdt = read.csv(file ="C:/Users/palemi/Water Boards/Supply and Demand Assessment - Documents/DWRAT/SDU_Runs/Demand_Datasets/RR_2017_2024_MDT_2025-04-04.csv")

# Inner Join RR Water Rights to AR_flat_file
rr_ar_subset <- rr_mdt %>%
  select(APPLICATION_NUMBER) %>% 
  inner_join(AR_flat_file,
             by = c("APPLICATION_NUMBER" = "ANNUALREPORT_WR_WATERRIGHTID__C"))    


# Are there any RR rights missing from Ar

# Export RR Annual Report Subset to Excel for inspection----


head(rr_ar_subset_final,10)

# Set the file path
excel_path = "OutputData/rr_ar_wb_inspection.xlsx"

# Createthe existing rr_ar_subset workbook object
rr_ar_wb <-createWorkbook(file = excel_path)

new_sheet <- "rr_ar_subset"
new_table <- "rr_ar_subset_table"

## Check if the sheet and table already exist and if so, remove them
if (new_sheet %in% names(rr_ar_wb)) {
  removeWorksheet(rr_ar_wb, sheet = new_sheet)
  cat(paste(" Existing sheet '", new_sheet, " ' found and removed.\n"))
}

# Add the 1st sheet
addWorksheet(wb = rr_ar_wb,
             sheetName = "rr_ar_subset")

# Write the data to the 1st sheet as a table
writeDataTable(wb = rr_ar_wb,
               sheet = new_sheet,
               x = rr_ar_subset_final,
               tableName = new_table,
               tableStyle = "TableStyleMedium9",
               startCol = 1,
               startRow = 1)
               

# Save the workbook
saveWorkbook(wb = rr_ar_wb,
             file = excel_path,
             overwrite = TRUE)

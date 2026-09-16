# PURPOSE: ---------------------------------------------------------------------

# Last Updated By: Payman Alemi on 9/3/2026

# "The purpose is to download the Calwatrs Flat Files Locally


# LOAD LIBRARIES----------------------------------------------------------------
library(DBI)
library(dbplyr)
library(odbc)

# CONNECT TO SNOWFLAKE----------------------------------------------------------

sf_con <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "snowflake",
  server = "gb51005.west-us-2.azure.snowflakecomputing.com",
  Trusted_Connection = "True",
  authenticator = "externalbrowser",
  warehouse = "DWR_WH"
  
)



# To disconnect, use:

#

#  dbDisconnect(sf_con)
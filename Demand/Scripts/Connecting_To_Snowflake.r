library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)
install packages(“odbc”)

# Establish connection.
sf_con <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "snowflake",
  server = "gb51005.west-us-2.azure.snowflakecomputing.com",
  Trusted_Connection = "True",
  authenticator = "externalbrowser"
)
 
# To disconnect, use:
#
#  dbDisconnect(sf_con)

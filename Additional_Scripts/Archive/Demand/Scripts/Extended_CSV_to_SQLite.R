# Convert a subset of "RawData/water_use_report_extended.csv" into a SQLite database
# This will make future queries to the dataset faster


#### Dependencies ####

require(data.table)
require(RSQLite)

#### Function ####

mainProcedure <- function () {
  
  
  # Establish a connection to a new 'sqlite' file in the "RawData" folder
  # (If the file does not exist, it will be created)
  conn <- dbConnect(dbDriver("SQLite"), "RawData/water_use_report_extended_subset.sqlite")
  
  
  
  # Specify the columns that will be imported from "water_use_report_extended.csv"
  columnNames <- c("APPLICATION_NUMBER", "YEAR", "MONTH", "AMOUNT", "DIVERSION_TYPE",
                   "FACE_VALUE_AMOUNT", "FACE_VALUE_UNITS", "EFFECTIVE_DATE", "EFFECTIVE_FROM_DATE",
                   "WATER_RIGHT_TYPE", "DIRECT_DIV_SEASON_START",
                   "STORAGE_SEASON_START", "DIRECT_DIV_SEASON_END", "STORAGE_SEASON_END",
                   "PARTY_ID", "APPLICATION_PRIMARY_OWNER", "MAX_STORAGE")
  
  
  
  # Read in those columns
  tableDF <- fread("RawData/water_use_report_extended.csv", select = columnNames, fill = TRUE)
  
  
  
  # Write 'tableDF' to the sqlite file
  dbWriteTable(conn, "Table", tableDF, overwrite = TRUE)
  
  
  
  # End the connection to the database afterwards
  dbDisconnect(conn)
  
  
  
  # Output a completion message
  cat("Done!\n")
  
  
  
  # Return nothing
  return(invisible(NULL))
  
}

#### Script Execution ####

print("Starting 'Extended_CSV_to_SQLite.R'...")


mainProcedure()


remove(mainProcedure)
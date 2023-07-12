library(tidyverse)
library(here)

# Import CSVs
eWRIMS_List <- read.csv(here("InputData/RR_POD_Review_List.csv"))
Master_List <- read.csv(here("OutputData/Flat_File_eWRIMS_2023-06-16.csv"))

# Add Water_Right_Type column to eWRIMS_List
eWRIMS_List <- left_join(x = eWRIMS_List, y = Master_List, by = c("APPL_ID", "POD_ID"))
eWRIMS_List <- select(eWRIMS_List, APPL_ID, WATER_RIGHT_TYPE)

# Grab Documents from eWRIMS in bulk
AppNum <- eWRIMS_List$APPL_ID
WR_Type <- eWRIMS_List$WATER_RIGHT_TYPE

for (i in 1:nrow(eWRIMS_List)) {
#for (i in 1:10) {
  tryCatch({
    # Download the document (permit, license, statement, registration, etc.)
    download.file(url = paste0("https://ciwqs.waterboards.ca.gov/ciwqs/ewrims/DocumentRetriever.jsp?appNum=",
                               AppNum[i], "&wrType=", WR_Type[i], "&docType=DOCS"),
                  destfile = paste0("OutputData/", AppNum[i], ".pdf"),
                  mode = "wb") # Resolves download issues on Windows
  }, error = function(e) {
    # Handle the error or simply ignore it
    cat("Error occurred for AppNum:", AppNum[i], "\n")
  })
}







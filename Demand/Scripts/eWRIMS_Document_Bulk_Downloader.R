library(tidyverse)
library(here)
library(data.table)

# Import eWRIMS PODs for your watershed----
Application_Number <- getXLSX(
  ws = ws,
  SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_GIS_PREPROCESSING_SPREADSHEET",
  FILEPATH = "GIS_PREPROCESSING_SPREADSHEET_PATH",
  WORKSHEET_NAME = "GIS_PREPROCESSING_WORKSHEET_NAME")

# Filter to unique Application_Numbers in your watershed
Application_Number <- Application_Number %>% select(APPLICATION_NUMBER) %>% unique()

# Import water_use_report_extended_CSV
# Just import the APPLICATION_NUMBER and WATER_RIGHT_TYPE
ewrims_data <- fread(input = "RawData/water_use_report_extended.csv", 
                     select = c("APPLICATION_NUMBER", "WATER_RIGHT_TYPE") # columns to load
)


#Generate an eWRIMS List based on your watershed
# Inner Join Application_Number to ewrims_data based on APPLICATION_NUMBER field  and remove duplicates
eWRIMS_List <- inner_join(x = Application_Number,
                          y = ewrims_data,
                          by = "APPLICATION_NUMBER") %>% unique()
# Produces 2271 unique records for the Russian River watershed on 9/20/2024

#Change the input CSV as needed depending on the watershed whose eWRIMS docs you need
eWRIMS_Names = colnames(eWRIMS_List) %>% sort()
eWRIMS_Names
eWRIMS_List = eWRIMS_List %>% select(APPLICATION_NUMBER, WATER_RIGHT_TYPE) %>% unique()

#Update WR_Types to Match URL Requirements
  #Replace spaces
eWRIMS_List$WATER_RIGHT_TYPE = gsub(x = eWRIMS_List$WATER_RIGHT_TYPE, pattern = " ", replacement = "%20")

#Set Download Timeout to 600 seconds
options(timeout = 600)

#Prevent the re-downloading of PDFs that have already been downloaded----

#Generate list of files, remove .pdf extension, of already downloaded PDFs
File_List = list.files(here("OutputData/ewrims_docs")) %>%str_remove(pattern = "\\.pdf$")
eWRIMS_List = eWRIMS_List%>%filter(!(APPLICATION_NUMBER %in% File_List)) #Remove Application_Numbers of already downloaded PDFs

for (i in 1:nrow(eWRIMS_List)) {
  tryCatch({
    # Download the document (permit, license, statement, registration, etc.)
    download.file(url = paste0("https://ciwqs.waterboards.ca.gov/ciwqs/ewrims/DocumentRetriever.jsp?appNum=",
                               eWRIMS_List$APPLICATION_NUMBER[i], "&wrType=",eWRIMS_List$WATER_RIGHT_TYPE[i], "&docType=DOCS"),
                  destfile = paste0(makeSharePointPath(ws$EWRIMS_REPORTS_FOLDER_PATH), eWRIMS_List$APPLICATION_NUMBER[i], ".pdf"),
                  mode = "wb") # Resolves download issues on Windows
  }, error = function(e) {
    # Handle the error or simply ignore it
    cat("Error occurred for AppNum:", AppNum[i], "\n")
  })
}







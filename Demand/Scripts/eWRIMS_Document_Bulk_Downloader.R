library(tidyverse)
library(here)

# Import eWRIMS PODs as of 12/18/2023----
#Change the input CSV as needed depending on the watershed whose eWRIMS docs you need
eWRIMS_List <- read.csv(here("InputData/Butte_List.csv"))
eWRIMS_Names = colnames(eWRIMS_List) %>% sort()
eWRIMS_Names
eWRIMS_List = eWRIMS_List %>% select(APPLICATION_NUMBER, WATER_RIGHT_TYPE) %>% unique()

#Update WR_Types to Match URL Requirements
  #Replace spaces
eWRIMS_List$WATER_RIGHT_TYPE = gsub(x = eWRIMS_List$WATER_RIGHT_TYPE, pattern = " ", replacement = "%20")

#Set Download Timeout to 300 seconds
options(timeout = 300)

#Prevent the re-downloading of PDFs that have already been downloaded----

#Generate list of files, remove .pdf extension, of already downloaded PDFs
File_List = list.files(here("OutputData/eWRIMS_Docs")) %>%str_remove(pattern = "\\.pdf$")
eWRIMS_List = eWRIMS_List%>%filter(!(APPLICATION_NUMBER %in% File_List)) #Remove Application_Numbers of already downloaded PDFs

#for (i in 1:nrow(eWRIMS_List)) {
  for (i in 1:nrow(eWRIMS_List)) {
  tryCatch({
    # Download the document (permit, license, statement, registration, etc.)
    download.file(url = paste0("https://ciwqs.waterboards.ca.gov/ciwqs/ewrims/DocumentRetriever.jsp?appNum=",
                               eWRIMS_List$APPLICATION_NUMBER[i], "&wrType=",eWRIMS_List$WATER_RIGHT_TYPE[i], "&docType=DOCS"),
                  destfile = paste0("OutputData/eWRIMS_Docs/", eWRIMS_List$APPLICATION_NUMBER[i], ".pdf"),
                  mode = "wb") # Resolves download issues on Windows
  }, error = function(e) {
    # Handle the error or simply ignore it
    cat("Error occurred for AppNum:", AppNum[i], "\n")
  })
}







#Load Packages----
library(readxl) #For Reading Excel packages
library(tidyverse)

#Import GIS Outputs----
  #I am comparing the attribute tables of Francisco's GIS pre-processing output to mine:
  #Francisco: RR_pod_points_Merge_filtered.xlsx
  #Payman:RR_pod_points_Merge_filtered_PA_2023-09-19.xlsx

Francisco_RR_POD = read_xlsx(path = "InputData/RR_pod_points_Merge_filtered.xlsx", col_names = TRUE)

#Rename APPL_ID to APPLICATION_NUMBER
Francisco_RR_POD = rename(Francisco_RR_POD, APPLICATION_NUMBER = APPL_ID)

Payman_RR_POD = read_xlsx(path = "InputData/RR_pod_points_Merge_filtered_PA_2023-09-19.xlsx", col_names = TRUE)


#Compare Francisco and Payman datasets----

  #Anti_Join Payman_RR_POD to Francisco_RR_POD by APPLICATION_NUMBER and vice versa
  Payman_Anti = anti_join(x = Payman_RR_POD, y = Francisco_RR_POD, by =  "APPLICATION_NUMBER")
    #1 record exists in Payman_RR_POD that doesn't exist in Francisco_RR_POD
        #S028872 is within the RR watershed and should be included in the dataset; 
                  #mistakenly excluded from Francisco_RR_POD dataset
  
  Francisco_Anti= anti_join(x = Francisco_RR_POD, y = Payman_RR_POD, by = "APPLICATION_NUMBER")
    #Francisco_Anti cannot be generated because all records in Francisco_RR_POD also exist in
    #Payman_RR_POD
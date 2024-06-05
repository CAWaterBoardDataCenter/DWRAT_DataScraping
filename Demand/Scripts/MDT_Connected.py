import pandas as pd
import os

Demand_Path = "C:/Users/palemi/Documents/GitHub/DWRAT_DataScraping/Demand"
os.chdir(Demand_Path)  # Change directory to the folder containing the CSV

#Import the demand CSVs
MDT_2017_2020 = pd.read_csv("OutputData/RR_2017-2020_MasterDemandTable_2024-04-24.csv")
#MDT_2017_2022 = pd.read_csv("OutputData/RR_2017-2022_MasterDemandTable.csv")  # Replace with the appropriate encoding if needed

#Set the Diversion Year
Diversion_Year = "2024"

#List comprehension to create column names
diversion_months = [f"{Diversion_Year}-{i:02d}" for i in range (1,13)]

#Rename JAN_MEAN_DIV, FEB_MEAN_DIV to 2024-01, 2024-02, etc.
df = MDT_2017_2020.iloc[:,1:13]
print(df)
df.columns = diversion_months
#Rename Application_Number to USER



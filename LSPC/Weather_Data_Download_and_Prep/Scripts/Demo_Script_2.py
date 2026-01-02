# DEMO #2

# These scripts demonstrate how to use the functions that interface with "Watershed_LSPC_Paths.xlsx"



#### SETUP ####

import HL01_Shared_Functions as hlp


from WS02_Watershed_Selection import ws


import pandas as pd



#### PROCEDURE ####

print("\n\n\nStarting 'Demo_Script_2.py'...")


# Read in the watershed boundary GIS file
tempDF = hlp.wsReadGIS(ws, ["WATERSHED_BOUNDARY_DATABASE_PATH", "WATERSHED_BOUNDARY_LAYER_NAME"])


# Output information to the console
print("\n\nWatershed boundary file's attribute table:")

print(tempDF)


# Write 'tempDF' to a CSV file
# (Use "ID" in the filename)
tempDF.to_csv("Processed_Data/" + hlp.wsExtract(ws, ['ID'])[0] + "_Watershed_Boundaries_Attribute_Table.csv")


# Finally, output a message based on whether the final directory folder path is specified
if pd.isna(hlp.wsExtract(ws, ["WEATHER_FILE_OUTPUT_DIRECTORY"])[0]):
    print("\n\nSide Note: Missing a final output directory! Later scripts will fail!")

else:
    print("\n\nThe final weather files will be stored in this path:")
    
    print(hlp.wsExtract(ws, ["WEATHER_FILE_OUTPUT_DIRECTORY"])[0])

    print("\n\n(Relative paths reference the 'Weather_Data_Download_and_Prep' folder!)")


# Output a completion message
print("\n\n\nScript complete!")

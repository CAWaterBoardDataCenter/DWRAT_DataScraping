# DEMO #1

# These scripts demonstrate how to use the functions that interface with "Watershed_LSPC_Paths.xlsx"



#### SETUP ####

import HL01_Shared_Functions as hlp


from WS02_Watershed_Selection import ws



#### PROCEDURE ####

print("\n\n\nStarting 'Demo_Script_1.py'...")


# Read in a watershed's station spreadsheet
tempDF = hlp.wsReadFile(ws, ["WEATHER_STATIONS_SPREADSHEET_PATH", "WEATHER_STATIONS_WORKSHEET_NAME"])


# Filter to "RAWS" stations
tempDF = tempDF[tempDF["DATA_SOURCE"] == "RAWS"]


# Output a message detailing the station information
if tempDF.shape[0] > 1:
    print("\n\nThe 'RAWS' stations for " + hlp.wsExtract(ws, ["NAME"])[0] + " are: ")

else:
    print("\n\nThe 'RAWS' station for " + hlp.wsExtract(ws, ["NAME"])[0] + " is: ")


print(tempDF.to_string())


# End of script
print("\n\n\nScript complete!")

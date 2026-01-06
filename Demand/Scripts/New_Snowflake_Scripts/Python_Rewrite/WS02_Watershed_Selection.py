# Given the index specified in "WS01_Set_Parameters.py", 
# extract the filepaths for a particular watershed 

# Store this information in a data frame variable called 'ws'



#### SETUP ####

import os


import HL01_Shared_Functions as hlp



#### IMPORTANT VARIABLES ####

# Import the chosen index from the user input file
from WS01_Set_Parameters import wsIndex


# The next variable defines the filepaths to the general paths spreadsheet
# There are two versions of this file (depending on whether the user is a part of SDA or not)
sheetPaths = ["Program Watersheds/4. Demand Data Tracking/Watershed_Demand_Dataset_Paths.xlsx",
              "../../../InputData/Watershed_Demand_Dataset_Paths.xlsx"]


# The final important variable defines the style of the paths table
# It should be one of two values ("COLUMN" or "ROW")
spreadsheetStyle = "ROW"

# The legacy filepath spreadsheet uses a "ROW" styling
# Each watershed has its own row in the spreadsheet
# (And each column corresponds to a different filepath option)

# The new version of the paths spreadsheet uses a "COLUMN" styling
# Each column (starting from the third one onwards) corresponds to a different watershed



#### PROCEDURE ####

# Start by reading in the paths spreadsheet
# (If the SharePoint spreadsheet can be found, use that)
# (Otherwise, use the one in the "InputData" folder)
if os.path.exists(hlp.makeSharePointPath(sheetPaths[0])):
    print("\nUsing SharePoint version of the watershed paths spreadsheet!\n")

    ws = hlp.readXLSX([sheetPaths[0]], True)

else:
    print("\nUsing local repository version of the watershed paths spreadsheet!\n")

    ws = hlp.readXLSX([sheetPaths[1]], False)


# Next, based on 'spreadsheetStyle', apply a different approach to narrow 'ws' to only the selected watershed
if spreadsheetStyle == "COLUMN":

    # Raise an exception if 'wsIndex' is greater than the maximum number of watershed columns in 'ws'
    # (The first two columns in 'ws' do not contain watershed information)
    if wsIndex > ws.shape[1] - 2:
        raise ValueError("In \"WS01_Set_Parameters.py\", 'wsIndex' is set to '" + str(wsIndex) + "', but the maximum acceptable value for this spreadsheet is '" + 
                         str(ws.shape[1] - 2) + "'")
    
    
    # If there's no issue, extract two columns from 'ws':
    #   (1) The "COLUMN" column (the second column of 'ws')
    #   (2) The column whose index equals 'wsIndex' + 1 (accounting for the two informational columns at the beginning)
    ws = ws.iloc[:, [1, wsIndex + 1]]


    # Finally, output a message about the watershed
    print("Running scripts for " + ws.columns[1] + "!")

elif spreadsheetStyle == "ROW":
    
    # Raise an exception if 'wsIndex' is greater than the maximum number of rows in 'ws' (minus 1 because of the category row at the very top)
    if wsIndex > ws.shape[0] - 1:
        raise ValueError("In \"WS01_Set_Parameters.py\", 'wsIndex' is set to '" + str(wsIndex) + "', but the maximum acceptable value for this spreadsheet is '" + 
                         str(ws.shape[0] - 1) + "'")
    

    # If there are no issues, proceed to the next step


    # Because the first row of the table contains only general category labels, start by updating the column headers to match row 0 of 'ws'
    # (This row contains the actual headers)
    ws.columns = ws.iloc[0]


    # After that, extract the row from 'ws' that corresponds to 'wsIndex' - 1 (accounting for zero-indexing):
    ws = ws.iloc[wsIndex]


    # Finally, output a message about the watershed
    print("Running scripts for " + ws.NAME + "!")

else:
    raise ValueError("'spreadsheetStyle' should be either 'ROW' or 'COLUMN', not '" + spreadsheetStyle + "'!")

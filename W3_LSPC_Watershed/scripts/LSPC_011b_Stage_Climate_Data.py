# Continuing from the R script 'LSPC_011a_Stage_Climate_Data.R', 
# this script processes the downloaded climate data into preliminary LSPC weather files

# Manual review spreadsheets are generated as well


# Use the helper script to prepare the climate-related objects 'app' and 'projects'
from HLP_001_Setup_Weather_Object import app, projects


# Process the downloaded data next (i.e., Staging)


# NLDAS (must be first)
for project in projects:
    app.run.stage.nldas(project)


# PRISM
for project in projects:
    app.run.stage.prism(project)


# CIMIS
for project in projects:
    app.run.stage.cimis(project)


# Gage data (NOAA, CDEC, LCD, and/or RAWS)
for project in projects:
    app.run.qc_gage_data(project)


# Manual review spreadsheets will now be generated 

# After some adjustments to these spreadsheets, 
# the next step will be for users to review flagged data for errors

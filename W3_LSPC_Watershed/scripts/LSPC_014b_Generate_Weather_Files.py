# Continuing from the R script 'LSPC_014a_Generate_Weather_Files.R', 
# this script processes the downloaded climate data into preliminary LSPC weather files

# Manual review spreadsheets are generated as well


# Use the helper script to prepare the climate-related objects 'app' and 'projects'
from HLP_001_Setup_Weather_Object import app, projects


# Adjust the gage data based on the manual review spreadsheets
for project in projects:
    app.run.qc_remake_timeseries_automation(project)


# Stage the gage data
for project in projects:
    app.run.stage.gage(project)


# Conclude the procedure by preparing the final "pre" and "air" files for each watershed
for project in projects:
    app.run.write.pre(project)


for project in projects:
    app.run.write.air(project)

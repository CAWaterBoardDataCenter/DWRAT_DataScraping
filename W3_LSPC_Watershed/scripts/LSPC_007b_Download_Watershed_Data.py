# Continuing from the R script 'LSPC_007a_Download_Gage_Data.R', this script downloads data specific to each watershed
# The sources include NOAA, CDEC, LCD, and RAWS


# Use the helper script to prepare the climate-related objects 'app' and 'projects'
from HLP_001_Setup_Weather_Object import app, projects


# Begin fetching project-specific data


# NOAA
for project in projects:
    app.run.fetch.gage.noaa(project)


# CDEC
for project in projects:
    app.run.fetch.gage.cdec(project)


# LCD
for project in projects:
    app.run.fetch.gage.lcd(project)


# RAWS
for project in projects:
    app.run.fetch.gage.raws(project)

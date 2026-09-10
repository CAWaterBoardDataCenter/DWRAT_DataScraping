# Continuing from the R script 'LSPC_004a_Download_Shared_Climate_Data.R', this script downloads data shared by multiple watersheds


# Use the helper script to prepare the climate-related objects 'app' and 'projects'
from HLP_001_Setup_Weather_Object import app, projects


# Begin fetching shared climate data


# PRISM
for project in projects:
    app.run.fetch.prism(project)


# CIMIS
for project in projects:
    app.run.fetch.cimis(project)


# NLDAS
for project in projects:
    app.run.fetch.nldas(project)

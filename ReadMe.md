# DWRAT DataScraping Repository Overview  
  
**Authors:** Payman Alemi and Aakash Prashar  
  
**Last Updated On:** 2026-06-25  
  
This repository contains files related to several ongoing development projects managed by the Waterboards' [Supply and Demand Assessment](https://www.waterboards.ca.gov/waterrights/water_issues/programs/supply-and-demand/) (SDA) section. The three main folders are "W1_Watershed_Demand", "W2_Russian_River", and "W3_LSPC_Watershed".  
  
## W1_Watershed_Demand (Demand Workflow)  
For every watershed in California, water rightholders self-report monthly diversion data on an annual basis. These scripts can help users gather relevant water rights for a watershed and flag potential issues in their reporting data. In combination with several manual QA/QC processes, these scripts ultimately generate a *demand dataset* for the watershed that can be used to simulate withdrawals in various sub-basins of a watershed on a monthly timescale. This dataset has applications in both SDA's model development and water availability analysis procedures.  
  
## W2_Russian_River (Russian River Workflow)  
Originally a process inherited from the Waterboards' Cannabis Instream Flows unit, this workflow integrates hydrologic modeling and water availability analysis—with exclusive focus on the Russian River watershed. It involves gathering weather data, applying QA/QC procedures to the gauge data, forecasting conditions for the remainder of the water year, and modeling flows via [PRMS](https://www.usgs.gov/software/precipitation-runoff-modeling-system-prms) and [SRP](https://github.com/andyrich/SRPHM_update_ag). This produces a *supply dataset* that the scripts use in conjunction with the *demand dataset* to perform a water availability analysis via **Paradigm DWRAT** (a modified version of the [Drought Water Rights Allocation Tool](https://github.com/CAWaterBoardDataCenter/DWRAT)).  
  
## W3_LSPC_Watershed (LSPC Watershed Workflow)  
For watersheds where SDA and its contractors have developed a [Loading Simulation Program in C++](https://cfpub.epa.gov/si/si_public_record_Report.cfm?Lab=NERL&dirEntryId=75860) (LSPC) model, this is the counterpart to the **Russian River Workflow**. The scripts in this folder perform similar processes, obtaining weather data, performing QA/QC analyses, forecasting conditions in the current water year, and modeling hydrologic flows (via LSPC in this case). Furthermore, later scripts in the workflow integrate the resultant *supply dataset* with the watershed's *demand dataset* to run **Paradigm DWRAT**.  
  
## Supporting Folders  
This repository contains several additional folders that support the three main workflows.  
  
### Shared_Scripts  
Several processes are shared between different workflows. This folder contains scripts whose functions can be imported by any procedure to perform these overlapping actions.  
  
### Models  
Tools such as **LSPC** and **Paradigm DWRAT** are stored here.  
  
### Additional_Scripts (Miscellaneous Scripts)  
The scripts in this folder are not integrated into a formal workflow, but they may contain useful supplemental or one-off procedures. The **Archive** sub-folder also contains files from obsolete workflows and scripts. Another particularly important directory is the **XLSX_Tracking** folder (discussed in the next section).  
  
### Additional_Scripts/XLSX_Tracking  
To improve the structure and usability of the workflows' control files, they are primarily shared as spreadsheets. However, the downside is that they are binary files, which are not tracked well by git. The imperfect workaround to this is an R script that extracts the underlying XML of these spreadsheets and stores them in this folder. These XML files are text-based and can be tracked by git. In addition, by compressing these files together and changing the "zip" extension to "xlsx", the spreadsheet can be recovered.  
  
# System Requirements  
The scripts and procedures are primarily developed for use on computers with **Windows** operating systems. In addition, the following programs are required:  
  
* [R](https://www.r-project.org/)  
  
* [RStudio](https://posit.co/products/open-source/rstudio)  
  
* [Anaconda Distribution](https://www.anaconda.com/download)  
  
* A program that can read and edit XLSX files (such as **Microsoft Excel**, **Google Sheets**, or **LibreOffice Calc**)  
  
* [Git](https://git-scm.com/) (some scripts submit git commands using Command Prompt)  
  
* [Google Chrome](https://www.google.com/chrome/) (only used in backup procedures that rely on RSelenium)  
  
In addition, GIS software such as **ArcGIS Pro** or **QGIS** is not mandatory, but it may assist users with the workflows.  
  
# Initial Setup  
The scripts in this repository are mainly coded in R and Python. However, the primary orchestrators are generally R scripts. All of the repository's R scripts rely on the "DWRAT_DataScraping" [RStudio Project](https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects), which can be activated using the file "DWRAT_DataScraping.Rproj". **Please ensure that this project is active when running any of the repository's R scripts.**  
  
## Dependencies  
The required packages for this repository's R and Python scripts are tracked via [renv](https://rstudio.github.io/renv/index.html) and [Anaconda YAML files](https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html#creating-an-environment-from-an-environment-yml-file), respectively.  
  
Once `renv` has been installed, its `restore` function can help download all required R packages (assuming the "DWRAT_DataScraping" project is active). For Python, the two required Anaconda environments are detailed in [Paradigm_DWRAT_Environment.yml](https://github.com/CAWaterBoardDataCenter/DWRAT_DataScraping/blob/main/Models/Paradigm_DWRAT/Paradigm_DWRAT_Environment.yml) and [LSPC_Climate_Environment.yml](https://github.com/CAWaterBoardDataCenter/DWRAT_DataScraping/blob/main/W3_LSPC_Watershed/LSPC_Climate_Environment.yml). [Anaconda Prompt](https://www.anaconda.com/docs/reference/glossary#anaconda-prompt) can help setup these environments with the command `conda create --file [PATH_TO_YML]`.[^1]  
  
**NOTE**: For the "W2_Russian_River" workflow, dependency setup is performed **automatically** by the scripts. Eventually, the other workflows will be updated to do the same as well.  
  
[^1]: Please remember to encase the path name in quotation marks if it contains spaces.
  
## Getting Started  
The remaining setup steps depend on the chosen workflow. For more information, please consult the workflows' primary scripts and associated documentation.  
  
### W1_Watershed_Demand  
Please review the [Documentation](https://github.com/CAWaterBoardDataCenter/DWRAT_DataScraping/tree/main/W1_Watershed_Demand/Documentation) folder and [Demand_Master_Script.R](https://github.com/CAWaterBoardDataCenter/DWRAT_DataScraping/blob/main/W1_Watershed_Demand/Scripts/Demand_Master_Script.R).  
  
### W2_Russian_River  
Formal documentation is still pending. Please review [ReadMe.txt](https://github.com/CAWaterBoardDataCenter/DWRAT_DataScraping/blob/main/W2_Russian_River) and [RRW_000_Master_Script.R](https://github.com/CAWaterBoardDataCenter/DWRAT_DataScraping/blob/main/W2_Russian_River/Scripts/RRW_000_Master_Script.R).  
  
### W3_LSPC_Watershed  
Formal documentation is still pending.  

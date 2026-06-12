# DWRAT DataScraping GitHub Repository README  
  
**Author:** Payman Alemi and Aakash Prashar  
  
**Last Updated On:** 2026-06-12  
  
This repository contains files related to several ongoing development projects managed by the Waterboards' [Supply and Demand Assessment](https://www.waterboards.ca.gov/waterrights/water_issues/programs/supply-and-demand/) (SDA) section. The three main folders are "W1_Watershed_Demand", "W2_Russian_River", and "W3_LSPC_Watershed".  
  
## W1_Watershed_Demand (Demand Workflow)  
For every watershed in California, water rightholders self-report monthly diversion data on an annual basis. These scripts can help users gather relevant water rights for a watershed and flag potential issues in their reporting data. In combination with several manual QA/QC processes, these scripts ultimately generate a *demand dataset* for the watershed that can be used to simulate withdrawals in various sub-basins of a watershed on a monthly timescale.  
  
## W2_Russian_River (Russian River Workflow)  
Originally a process inherited from the Waterboards' Cannabis Instream Flows unit, this workflow integrates hydrologic modeling and water availability analysis—with exclusive focus on the Russian River watershed. It involves gathering weather data, applying QA/QC procedures to the gauge data, forecasting conditions for the remainder of the water year, and modeling flows via [PRMS](https://www.usgs.gov/software/precipitation-runoff-modeling-system-prms) and [SRP](https://github.com/andyrich/SRPHM_update_ag). This produces a *supply dataset* that the scripts use in conjunction with the *demand dataset* to perform a water availability analysis via **Paradigm DWRAT** (a modified version of the [Drought Water Rights Allocation Tool](https://github.com/CAWaterBoardDataCenter/DWRAT)).  
  
## W3_LSPC_Watershed (LSPC Watershed Workflow)  
For watersheds where SDA and its contractors have developed a [Loading Simulation Program in C++](https://cfpub.epa.gov/si/si_public_record_Report.cfm?Lab=NERL&dirEntryId=75860) (LSPC) model, this is the counterpart to the **Russian River Workflow**. The scripts in this folder perform similar processes, obtaining weather data, performing QA/QC analyses, forecasting conditions in the current water year, and modeling hydrologic flows (this time using LSPC). Furthermore, later scripts in the workflow integrate the resultant *supply dataset* with the watershed's *demand dataset* to run **Paradigm DWRAT**.  
  
## Supporting Folders    
This repository contains several additional folders that support the three main workflows.  
  
### Shared_Scripts  
Several processes are shared between different workflows. This folder contains scripts that can be imported by any procedure to perform these overlapping steps.  
  
### Models  
Tools such as LSPC and Paradigm DWRAT are stored here.  
  
### Additional_Scripts (Miscellaneous Scripts)  
The scripts in this folder are not integrated into a formal workflow, but they may contain useful supplemental or one-off procedures. The **Archive** sub-folder also contains files from obsolete workflows and scripts. This folder contains an important directory as well:  
  
### Additional_Scripts/XLSX_Tracking  
To improve the structure and usability of the workflows' control files, they are primarily shared as spreadsheets. However, the downside is that they are binary files, which are not tracked well by git. The imperfect workaround to this is an R script that extracts the underlying XML of these spreadsheets and stores them in this folder. These XML files are text-based and can be tracked by git.  

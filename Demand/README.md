The **Demand** folder contains the files necessary for converting the raw diverter
demand data from eWRIMS into a processed, QAQC’d demand dataset ready
for importation into DWRAT. As of 2023-08-01, this folder has 4
subfolders, which just contain placeholder scripts–the actual scripts
have yet to be written.

- **Documentation**: This folder will contain relevant emails, PDFs, Word
  Documents and other files containing instructions or information about
  the project.

- **InputData:** This folder just contains the output CSV from the GIS
  pre-processing steps, which serves as the input file for most of the R
  pre-processing steps and ultimately for the demand module scripts.

- **Intermediate Data:** This folder contains datasets that have been
  modified from the RawData folder. These datasets are considered
  intermediate because they will be used as inputs for other R Scripts.
  *Files in this folder are ignored by the .gitignore file.*

- **Module and Script Comparison:** This folder compares the outputs of
  the Excel Demand Data Modules and the outputs of the corresponding Demand Data R
  scripts. Each Excel module has a corresponding R Script.

- **OutputData:** This folder will contain the final demand dataset CSVs
  that are ready for importation into the *Upper Russian River (URR)* and
  *Lower Russian River (LRR)* Drought Water Rights Allocation (DWRAT)
  models.

- **RawData:** This folder contains the eWRIMS flat files downloaded from https://intapps.waterboards.ca.gov/downloadFile/faces/flatFilesEwrims.xhtml.
*Files in this folder are ignored by the .gitignore file.*

- **Scripts:** This folder contain all the scripts of the Demand sub-repository. The Demand_Master.R script is a parent script that lists the individual child scripts in the correct run sequence; there are a few other standalone scripts that the Demand_Master.R script that does not reference. Each script has detailed comments describing its purpose. 

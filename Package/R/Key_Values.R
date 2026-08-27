
# Assumptions used by one or more workflows are stored as functions in this script

# That way, if a value must be updated, that can be done in one location 
# instead of in several


#### Control File Paths ####

#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' # ...
master_control_path <- function () {
  
  # This is the repository path to the Master Control File
  return("Master_Control_File.xlsx")
  
}



#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' # ...
master_control_worksheet <- function () {
  
  # This is the worksheet name for the Master Control File
  return("Main")
  
}


#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' # ...
demand_control_local_path <- function () {
  
  # This is the repository path to the demand workflow's control file
  return("W1_Watershed_Demand/Input/Watershed_Demand_Dataset_Paths.xlsx")
  
}



#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' # ...
demand_control_worksheet <- function () {
  
  # This is the worksheet name for the demand workflow's control file
  return("Main_Sheet")
  
}



#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' # ...
rr_control_local_path <- function () {
  
  # This is the repository path to the Russian River workflow's control spreadsheet
  return("W2_Russian_River/Input/RR_Workflow_Control_File.xlsx")
  
}



#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' # ...
rr_control_worksheet <- function () {
  
  # This is the worksheet name for the Russian River workflow control file
  return("Sheet1")
  
}



#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' # ...
lspc_weather_control_path <- function () {
  
  # This is the repository path to the LSPC workflow's weather control spreadsheet
  # It contains information related to preparing the weather input files
  return("W3_LSPC_Watershed/inputs/LSPC_Weather_Control.xlsx")
  
}



#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' # ...
lspc_weather_control_worksheet <- function () {
  
  # This is the worksheet name for the LSPC workflow's weather control spreadsheet
  return("Control")
  
}



#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' # ...
lspc_master_control_local_path <- function () {
  
  # This is the repository path to the LSPC workflow's primary control spreadsheet
  # It contains information related to managing the entire workflow
  return("W3_LSPC_Watershed/inputs/LSPC_Workflow_Control_File.xlsx")
  
}



#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' # ...
lspc_master_control_worksheet <- function () {
  
  # This is the worksheet name for the LSPC workflow's master control spreadsheet
  return("Sheet1")
  
}



#### Weather Data Start Dates ####

#' @title Get the Start Date for PRISM
#' 
#' @description
#' This function returns a [Date] object containing the earliest date for which  
#' daily data from [PRISM](https://prism.oregonstate.edu/) is available. 
#' 
#' @details
#' The `Parameter-elevation Regressions on Independent Slopes Model` (PRISM) 
#' dataset is a core component of SDA's hydrologic modeling work. Frequently in the
#' Russian River workflow, data before and after the start of PRISM is used. 
#' However, different conditions may apply based on the availability of PRISM data. 
#' Rather than type out the start date in every location, this function provides 
#' easy access to that information. 
#' 
#' In this package, the start date for PRISM could have been stored as a variable 
#' instead of a function, but because this package is stored within a sub-folder 
#' of the larger repository, it is easier to track and update this value if it 
#' is a function. 
#' 
#' @usage prism_start()
#' 
#' @returns A single [Date] value
#' 
#' @export
#' 
#' @examples
#' # Define an example start date for a file
#' dataStart <- as.Date("2023-05-15", format = "%Y-%m-%d") 
#'
#' dataStart > prism_start()
#' # Will be FALSE
#' 
#' 
#' # This function helps avoid typing out PRISM's start date repeatedly
#' # across different functions and scripts
#' if (as.Date("1947-10-01", format = "%Y-%m-%d") < prism_start()) {
#'   #  This inner code would run
#' }
prism_start <- function () {
  
  # Return the start date for daily PRISM data as a Date variable
  
  return(as.Date("1981-01-01", format = "%Y-%m-%d"))
  
}



#' @title Get the Start Date for CIMIS
#' 
#' @description
#' This function returns a [Date] object containing the earliest date for which  
#' daily data from [CIMIS](https://cimis.water.ca.gov/) is available. 
#' 
#' @details
#' The `California Irrigation Management Information System` (CIMIS) dataset 
#' is an important component of SDA's hydrologic modeling work. Frequently in the
#' Russian River workflow, data before and after the start of CIMIS is used. 
#' However, different conditions may apply based on the availability of CIMIS data. 
#' Rather than type out the start date in every location, this function provides 
#' easy access to that information. 
#' 
#' In this package, the start date for CIMIS could have been stored as a variable 
#' instead of a function, but because this package is stored within a sub-folder 
#' of the larger repository, it is easier to track and update this value if it 
#' is a function. 
#' 
#' @usage cimis_start()
#' 
#' @returns A single [Date] value
#' 
#' @export
#' 
#' @examples
#' # Define an example start date for a file
#' dataStart <- as.Date("2022-12-01", format = "%Y-%m-%d") 
#'
#' dataStart > cimis_start()
#' # Will be FALSE
#' 
#' 
#' # This function helps avoid typing out CIMIS's start date repeatedly
#' # across different functions and scripts
#' if (as.Date("1947-10-01", format = "%Y-%m-%d") < cimis_start()) {
#'   #  This inner code would run
#' }
cimis_start <- function () {
  
  # Return the start date for CIMIS as a Date variable
  
  return(as.Date("1982-06-07", format = "%Y-%m-%d"))
  
}

# This script performs two or three major checks:
#
#   (1) Make sure the connectivity matrix is a DOWNSTREAM matrix
#
#       A "1" in a row means that the row sub-basin eventually flows 
#       into the column sub-basin
#
#       With our model formulation, every sub-basin has AT MOST 
#       one downstream catchment
#
#   (2) Make sure the catchment IDs match the IDs in the connectivity matrix
#       (and vice versa)
#
#   (3) If a sub-basin assignment sheet exists, make sure those catchments 
#       match the connectivity matrix sub-basins too


base::remove(list = ls())


require(dplyr)
require(readxl)
require(sf)
require(cli)


cat("\n\nStarting \"Conn_Mat_QAQC.R\"...\n\n")


source("W1_Watershed_Demand/Scripts/Watershed_Selection.R")


cat("\n\n")


# Read in the watershed catchments layer and the connectivity matrix
subWS <- getGIS(ws = ws, 
                GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                GIS_FILE_PATH = "SUBBASIN_POLYGONS_DATABASE_PATH",
                GIS_FILE_LAYER_NAME ="SUBBASIN_POLYGONS_LAYER_NAME")


# This identifies the field that contains unique catchment IDs
fieldName <- ws$SUBBASIN_FIELD_ID_NAMES %>%
  str_split(";") %>% unlist() %>% trimws() |>
  pluck(1)


connMat <- getXLSX(ws,
                   "IS_SHAREPOINT_PATH_CONNECTIVITY_MATRIX_SPREADSHEET",
                   "CONNECTIVITY_MATRIX_SPREADSHEET_PATH",
                   "CONNECTIVITY_MATRIX_WORKSHEET_NAME")


#### General Checks ####

if (nrow(connMat) != (ncol(connMat) - 1)) {
  stop("Connectivity matrix not in a matrix format!")
  
  # 'connMat' has one extra column that lists all catchment IDs
  
}


# Every catchment in the first row should appear as a column name
if (!all(connMat[[1]] %in% names(connMat))) {
  stop("Every catchment ID in the first row should appear as a column header too")
}


#### Check #1 Downstream Connectivity Matrix ####

# Make sure the connectivity matrix is a DOWNSTREAM matrix

# Every catchment should zero or one immediately downstream catchments


for (i in 1:nrow(connMat)) {
  
  # Get a list of all catchments that are downstream of this catchment's flowpath
  flowIndices <- which(connMat[i, -1] == 1)
  
  
  if (length(flowIndices) == 0) {
    next
  } else {
    flowIndices <- flowIndices + 1  # Add one because 'flowIndices' ignored the first column
  }
  
  
  # Get all downstream catchments (ignore the current iteration's catchment in this list)
  flowPath <- names(connMat)[base::setdiff(flowIndices, i)]
  
  
  # Get the number of upstream connections for each downstream catchment
  # Further downstream catchments have larger numbers
  # (since more catchments eventually drain into that catchment)
  if (length(flowPath) > 1) {
    downstreamSums <- colSums(connMat[, names(connMat) %in% flowPath])
  } else {
    downstreamSums <- connMat[, names(connMat) %in% flowPath] |> sum() |> set_names(flowPath)
  }
  
  
  # The most immediate downstream sub-basin would have the minimum column sum
  immediateDownstream <- names(downstreamSums)[which(downstreamSums == min(downstreamSums))]
  
  
  # If 'immediateDownstream' has more than one value (or it is empty), this is an error
  if (length(immediateDownstream) != 1) {
    stop(paste0("Catchment ", connMat[[1]][i], " does not have exactly one immediately ",
                "downstream catchment. Is this matrix transposed?"))
  }
  
}


#### CHECK #2 Catchments and Conn Mat ####

# Confirm that the catchment layer and connectivity matrix have the same IDs
if (!all(subWS[[fieldName]] %in% connMat[[1]])) {
  
  print(subWS[[fieldName]][which(!(subWS[[fieldName]] %in% connMat[[1]]))])
  
  stop("Not all catchment IDs appear in the connectivity matrix!")
  
}


# Check the reverse scenario too
if (!all(connMat[[1]] %in% subWS[[fieldName]])) {
  
  print(connMat[[1]][which(!(connMat[[1]] %in% subWS[[fieldName]]))])
  
  stop("Not all connectivity matrix IDs appear in the catchment layer!")
  
}


# Make sure 'subWS' and 'connMat' have the same number of rows
if (nrow(subWS) != nrow(connMat)) {
  
  stop("Catchment layer and connectivity matrix have different number of rows. Duplicate IDs?")
  
}


#### CHECK #3 New Catchments, Old Sub-Basin Sheet ####

# If the sub-basin spreadsheet exists, read it in
# Confirm that the catchments in 'basinDF' match 'connMat'
if (!is.na(ws$SUBBASIN_ASSIGNMENT_SPREADSHEET_PATH)) {
  
  basinDF <- getXLSX(ws, 
                     "IS_SHAREPOINT_PATH_SUBBASIN_ASSIGNMENT_SPREADSHEET",
                     "SUBBASIN_ASSIGNMENT_SPREADSHEET_PATH",
                     "SUBBASIN_ASSIGNMENT_WORKSHEET_NAME")
  
  
  if (!all(basinDF[[fieldName]] %in% connMat[[1]])) {
    stop(paste0("The connectivity matrix does not match the existing sub-basin assignment spreadsheet!\n",
                "Please ensure both files are up-to-date before generating the Master Demand Table.\n",
                "Delete the sub-basin spreadsheet information from \"Watershed_Demand_Dataset_Paths.xlsx\" ",
                "if it requires updates."))
  }
  
}


# If no errors occur, notify the user that the script is complete
cat("\n\n")
print("No issues detected!")

"\n\"Conn_Mat_QAQC.R\" is complete!\n\n" |>
  col_green() |>
  cat()


# Clear the environment
base::remove(list = ls())

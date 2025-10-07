# After the LSPC model has been run successfully, 
# this script can prepare that data for the Paradigm DWRAT



remove(list = ls())



require(tidyverse)
require(sf)
require(readxl)



cat("Starting 'LSPC_Postprocessing.R'...\n\n")



source("Scripts/Watershed_Selection.R")



# Read in the watershed boundaries
wsBound <- getGIS(ws = ws, 
                  GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY",
                  GIS_FILE_PATH = "WATERSHED_BOUNDARY_DATABASE_PATH",
                  GIS_FILE_LAYER_NAME = "WATERSHED_BOUNDARY_LAYER_NAME") %>%
  st_transform("epsg:3488")



# Also get the watershed's subbasins
subWS <- getGIS(ws = ws, 
                GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_SUBBASIN_POLYGONS",
                GIS_FILE_PATH = "SUBBASIN_POLYGONS_DATABASE_PATH",
                GIS_FILE_LAYER_NAME ="SUBBASIN_POLYGONS_LAYER_NAME") %>%
  st_transform(st_crs(wsBound)) %>%
  rename(BASIN = ws$SUBBASIN_FIELD_ID_NAMES[1] %>% str_split(";") %>% unlist() %>% trimws() %>% head(1))



# Get the Master Demand Table
mdtDF <- makeSharePointPath(ws$MASTER_DEMAND_TABLE_CSV_PATH) %>%
  read_csv()



# Use the HUC-12 boundaries for modeling in DWRAT
huc12 <- makeSharePointPath("Program Watersheds/1. Watershed Folders/Navarro River/Data/GIS Datasets/WBD_18_HU2_Shape/Shape/") %>%
  st_read(layer = "WBDHU12") %>%
  st_transform(st_crs(wsBound)) %>%
  select(huc12, name)



# Shrink 'wsBound' by 1 meter and find relevant boundaries from 'huc12'
huc12 <- huc12[lengths(st_intersects(huc12, st_buffer(wsBound, dist = -1))) > 0, ]



stopifnot(nrow(huc12) == length(unique(huc12$huc12)))



# Set up a connectivity matrix for the DWRAT run



# Read in the full catchment connectivity matrix
connMat <- getXLSX(ws,
                   "IS_SHAREPOINT_PATH_CONNECTIVITY_MATRIX_SPREADSHEET",
                   "CONNECTIVITY_MATRIX_SPREADSHEET_PATH",
                   "CONNECTIVITY_MATRIX_WORKSHEET_NAME")



# Assign HUC-12 to catchments
intersections <- st_intersects(st_centroid(subWS), huc12)



stopifnot(max(lengths(intersections)) == 1)
stopifnot(min(lengths(intersections)) == 1)



subWS <- subWS %>%
  mutate(huc12 = huc12$huc12[unlist(intersections)])



# Use the catchment assignments to determine the outlet for each HUC-12 subbasin
names(connMat)[1] <- "BASIN"



connMat <- connMat %>%
  rename() %>%
  left_join(subWS %>%
              st_drop_geometry() %>%
              select(BASIN, huc12) %>%
              unique(),
            by = "BASIN",
            relationship = "one-to-one")



lastCatch <- huc12 %>%
  st_drop_geometry() %>%
  select(huc12, name) %>%
  mutate(BASIN = NA_real_)



# Get the outlet catchment for each HUC-12 subbasin
# (It should be the catchment that all other catchments in the subbasin drain to)
for (i in 1:nrow(lastCatch)) {
  
  subConn <- connMat %>%
    filter(huc12 == lastCatch$huc12[i]) %>%
    select(-huc12)
  
  
  
  subConn <- subConn %>%
    select(BASIN, all_of(as.character(subConn$BASIN)))
  
  
  
  maxIndex <- which(colSums(subConn[, -1]) == nrow(subConn))
  
  
  
  stopifnot(length(maxIndex) == 1)
  
  
  
  lastCatch$BASIN[i] <- as.numeric(names(subConn)[maxIndex + 1])
  
}



# Create a connectivity matrix with just the catchments in 'lastCatch'
dwratConn <- connMat %>%
  filter(BASIN %in% lastCatch$BASIN) %>%
  select(BASIN, all_of(as.character(lastCatch$BASIN)))



# Replace the catchment IDs with HUC-12 numbers
dwratConn$BASIN <- dwratConn$BASIN %>%
  map_chr(~ lastCatch$huc12[which(lastCatch$BASIN == .)])


colnames(dwratConn)[-1] <- colnames(dwratConn)[-1] %>%
  map_chr(~ lastCatch$huc12[which(lastCatch$BASIN == .)])



# Create a "FLOWS_TO" variable that indicates the immediate downstream HUC-12 of each basin
flowsTo <- huc12 %>%
  st_drop_geometry() %>%
  select(huc12) %>%
  mutate(FLOWS_TO = NA_character_)



# Use 'dwratConn' to make this determination
for (i in 1:nrow(flowsTo)) {
  
  iterRow <- dwratConn %>%
    filter(BASIN == flowsTo$huc12[i]) %>%
    select(-as.character(flowsTo$huc12[i]))
  
  
  nonZeroBasins <- names(which(colSums(iterRow[, -1]) == 1))
  
  
  
  # The most downstream basin will not connect to anything
  if (length(nonZeroBasins) == 0) {
    next
  }
  
  
  
  downstreamSums <- colSums(dwratConn[, names(dwratConn) %in% nonZeroBasins])
  
  
  immediateDownstream <- names(downstreamSums)[which(downstreamSums == min(downstreamSums))]
  
  
  stopifnot(length(immediateDownstream) == 1)
  
  
  flowsTo$FLOWS_TO[i] <- immediateDownstream
  
}



# Only the most downstream basin should have "NA" in "FLOWS_TO"
stopifnot(sum(is.na(flowsTo$FLOWS_TO)) == 1)



# Replace that entry with 000
flowsTo$FLOWS_TO[is.na(flowsTo$FLOWS_TO)] <- "000"



# Get the supply data for each catchment in 'lastCatch'
RO <- makeSharePointPath(ws$LSPC_STREAM_OUTPUT_CSV_PATH) %>%
  read_csv() %>%
  filter(parmname == "RO") %>%
  filter(rchid %in% lastCatch$BASIN) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"),
         value1_AF = value1 / 43559.9) # The CFS values are already using a monthly timestep
                                       # The only necessary conversion is AF to ft^3



# Append "HUC-12" labels to 'RO'
RO <- RO %>%
  rename(BASIN = rchid) %>%
  left_join(lastCatch %>% select(BASIN, huc12),
            by = "BASIN", relationship = "many-to-one")



#### PARADIGM DWRAT ####

# Create outputs that mimic the required files for Paradigm DWRAT:

# "formatted_demand.csv"
# "formatted_supply.csv"
# "generated_basins.csv"



# Join HUC-12 and "FLOWS_TO" information to 'mdtDF'
# Then rename "BASIN" to "CATCHMENT" (and "huc12" to "BASIN")
mdtDF <- mdtDF %>%
  left_join(subWS %>% st_drop_geometry() %>% select(BASIN, huc12),
            by = "BASIN", relationship = "many-to-one") %>%
  left_join(flowsTo, by = "huc12", relationship = "many-to-one") %>%
  rename(CATCHMENT = BASIN) %>%
  rename(BASIN = huc12) %>%
  mutate(BASIN = paste0("HUC_", BASIN)) %>%
  mutate(FLOWS_TO = paste0("HUC_", FLOWS_TO))



# Finally, write the variable to "formatted_demand.csv"
mdtDF %>%
  write_csv("OutputData/formatted_demand.csv")




RO <- RO %>%
  select(date, value1_AF, huc12) %>%
  rename(BASIN = huc12, Date = date) %>%
  arrange(Date, BASIN) #%>%
# mutate(BASIN = paste0("HUC_", BASIN))



# Transform 'RO' so that each "date" value marks a unique row and
# each HUC-12 is its own column 
# ("value1_AF" provides the values in this new table)
pivot_wider(RO, id_cols = Date, names_from = BASIN, values_from = value1_AF) %>%
  mutate(Date = format(Date, "%m/%d/%Y")) %>%
  mutate(Date = str_replace(Date, "/[0-9]{2}/", "/01/")) %>%
  write_csv("OutputData/formatted_supply.csv")



# The final input file lists the various sub-basins and their connectivity
# (In a simplified one-to-one format)
flowsTo <- flowsTo %>%
  rename(BASIN = huc12) %>%
  arrange(BASIN) %>%
  mutate(BASIN = paste0("HUC_", BASIN)) %>%
  mutate(FLOWS_TO = paste0("HUC_", FLOWS_TO))



flowsTo %>%
  write_csv("OutputData/generated_basins.csv")



cat("\n\nThe script has finished running!\n\n")

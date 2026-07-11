# Download elevation data from USGS for a watershed

# Get the bounding box of a watershed boundary and download TIFF files for this region

# Then combine them into a single raster file


## IMPORTANT ##

# This data comes from the USGS National Elevation Dataset (NED)
# It is the latest 1/3 arc-second elevation rasters


## WARNING ##

# This script may require a lot of RAM!
# (Especially if the watershed is as big as Trinity River)


#### Setup ####

# Clear the environment
base::remove(list = ls())


# Import packages
require(tidyverse)
require(sf)
require(stars)


# Change the default download timeout value to 1,000,000 seconds
options(timeout = 10^6)


#### Procedure ####

cat("\n\n")
cat("Starting 'USGS_Elevation_Raster_Downloader.R'!")
cat("\n\n")


# Import a watershed selection
source("W1_Watershed_Demand/Scripts/Watershed_Selection.R")


cat("\n\n")
cat("[1/3]\tDownloading raster files...")
cat("\n\n")


# Read in the watershed boundary layer
wsBound <- getGIS(ws = ws, 
                  GIS_SHAREPOINT_BOOL = "IS_SHAREPOINT_PATH_WATERSHED_BOUNDARY",
                  GIS_FILE_PATH = "WATERSHED_BOUNDARY_DATABASE_PATH",
                  GIS_FILE_LAYER_NAME = "WATERSHED_BOUNDARY_LAYER_NAME") |>
  st_transform("NAD83")


# Get the range of latitude and longitude values for this boundary 
# Use the bounding box for that
bboxCoords <- wsBound |> st_bbox()


# Rasters are available by latitude and longitude coordinate (with no decimal places)
# Get the range of rasters to download by obtaining the floor and ceiling 
# of the minimum and maximum values
minLat <- bboxCoords |> pluck("ymin") |> floor()
maxLat <- bboxCoords |> pluck("ymax") |> ceiling()
minLon <- bboxCoords |> pluck("xmin") |> floor()
maxLon <- bboxCoords |> pluck("xmax") |> ceiling()


# Get the range of latitude values between 'minLat' and 'maxLat'
# Do the same for the longitude coordinates
latRange <- minLat:maxLat
lonRange <- minLon:maxLon


# Initialize a vector to hold the filenames for each of the downloaded rasters
dlNames <- rep("", length = length(latRange) * length(lonRange))


# Iterate through the latitude and longitude values
# Download rasters to the "Output" folder
for (i in 1:length(latRange)) {
  
  for (j in 1:length(lonRange)) {
    
    # Prepare a latitude-longitude string
    # (This appears in the URL path and filename)
    latLonStr <- paste0(if_else(latRange[i] < 0, "s", "n"),
                        abs(latRange[i]),
                        if_else(lonRange[j] < 0, "w", "e"),
                        abs(lonRange[j]))
    
    
    # Prepare the URL that contains the elevation raster
    rasterURL <- paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/",
                        "Elevation/13/TIFF/current/",
                        latLonStr, "/USGS_13_", latLonStr, ".tif")
    
    
    # Set the planned output filename
    outName <- paste0("W1_Watershed_Demand/Output/", latLonStr, ".tif")
    
    
    # Download the raster image and save it as 'outName'
    download.file(rasterURL, outName, mode = "wb")
    
    
    # Add 'outName' to 'dlNames'
    dlNames[which(dlNames == "")[1]] <- outName
    
    
    # Wait a bit before proceeding
    Sys.sleep(runif(1, min = 1.2, max = 1.8))
    
  }
  
}


# Once the files have downloaded successfully, 
# read them in and combine them into a single layer

# However, not all of these rasters may intersect with the watershed boundaries
cat("\n\n")
cat("[2/3]\tConverting rasters into a single spatial object...\n")
cat("\t(This may take a while and a lot of RAM)")
cat("\n\n")


# Iterate through all of the rasters 
for (i in 1:length(dlNames)) {
  
  # Read in the raster as a proxy first
  tempDF <- read_stars(dlNames[i], proxy = TRUE)
  
  
  # Test cropping 'tempDF' to the extent of 'wsBound'
  tempDF <- tempDF |>
    st_crop(bboxCoords, as_points = FALSE)
  
  
  # Confirm that there is overlap between the raster and the watershed
  # `st_dimensions` will give the rows/columns of relevant raster cells 
  # under "from" and "to"
  dimCheck <- st_dimensions(tempDF)
  
  
  # Check for negative values in "from" and "to"
  # (That would mean the watershed is outside of the raster boundaries)
  if (dimCheck[["x"]][["from"]] < 0 || dimCheck[["x"]][["to"]] < 0 ||
      dimCheck[["y"]][["from"]] < 0 || dimCheck[["y"]][["to"]] < 0) {
    next
  }
  
  
  # Read in 'tempDF' again
  # This time, read the actual raster, not a proxy
  tempDF <- read_stars(dlNames[i], proxy = FALSE)
  
  
  # Crop 'tempDF' to the extent of the watershed boundaries
  tempDF <- tempDF |>
    st_crop(bboxCoords, as_points = FALSE)
  
  
  # If 'elevDF' has not been defined yet, initialize it with 'tempDF'
  if (!exists("elevDF")) {
    elevDF <- tempDF
    
  # Otherwise, bind the layers together using `st_mosaic`
  } else {
    
    # `st_mosaic` will default to returning a proxy
    # Use `read_stars` to read in the full raster instead
    # Then, apply `st_crop` again to make sure that the raster fits the bounding box
    elevDF <- st_mosaic(elevDF, tempDF)[[1]] |>
      read_stars(proxy = FALSE) |>
      st_crop(bboxCoords, as_points = FALSE)

  }
  
}


# The final step is to save 'elevDF' to a file
cat("\n\n")
cat("[3/3]\tWriting watershed elevation data to a TIFF file...")
cat("\n\n")


outPath <- paste0("W1_Watershed_Demand/Output/", ws$ID[1], 
                  "_Elevation_", Sys.Date(), ".tif")


stars::write_stars(elevDF, outPath, append = FALSE)


# Output a message about that
cat("\n\n")
message(paste0("Wrote raster to '", outPath, "'!"))
cat("\n\n")


cat("\n\n")
cat("'USGS_Elevation_Raster_Downloader.R' is complete!")
cat("\n\n")


# Clear the environment
base::remove(list = ls())

require(tidyverse)
require(sf)

# STEP 1: Define a data frame with multiple Northing and Easting values
# You can modify this table to include your list of coordinates
coords_df <- data.frame(
  Northing = c(2212993, 2187465, 1995644, 2224576),
  Easting = c(6258764, 6223862, 6371650, 6258393)
)

# Function to convert a single Northing/Easting pair for both NAD27 and NAD83
convert_coords <- function(Northing, Easting) {
  
  # Convert NAD27 to NAD83
  pointVals_NAD27 <- data.frame(x = Easting, y = Northing) %>%
    st_as_sf(coords = 1:2, crs = "epsg:26742") %>%
    st_transform("NAD83") %>%
    st_coordinates()
  
  # Convert NAD83 to NAD83 (essentially verifying no transformation)
  pointVals_NAD83 <- data.frame(x = Easting, y = Northing) %>%
    st_as_sf(coords = 1:2, crs = "epsg:2226") %>%
    st_transform("NAD83") %>%
    st_coordinates()
  
  # Create a results tibble
  tibble(
    Northing = Northing,
    Easting = Easting,
    DATUM = c("NAD27", "NAD83"),
    NAD83_LATITUDE = c(pointVals_NAD27[2], pointVals_NAD83[2]),
    NAD83_LONGITUDE = c(pointVals_NAD27[1], pointVals_NAD83[1])
  )
}

# Apply function to all rows in coords_df
results_df <- coords_df %>%
  pmap_dfr(~ convert_coords(..1, ..2))

# Print results
options(digits = 10)  # Set higher decimal precision globally
print(results_df)

# Optional: Save to CSV for further use
write.csv(results_df, "IntermediateData/Converted_Coordinates.csv", row.names = FALSE)

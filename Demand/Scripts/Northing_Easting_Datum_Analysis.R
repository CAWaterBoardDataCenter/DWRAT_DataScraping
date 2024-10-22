# Given Northing and Easting values (projected coordinates), 
# try different coordinate reference systems (i.e., NAD83 and NAD27)
# Then, output corresponding NAD83 geographic coordinates (latitude and longitude)


require(tidyverse)
require(sf)


# STEP 1
# !!! Input Northing and Easting coordinates here !!!
# !!! Only one number per variable !!!
Northing <- 348200
Easting <- 1649800


# STEP 2
# !!! Hit the "Source" button to run this script !!!
# !!! It's located in the UPPER-RIGHT corner of the script pane !!!!


#### DO NOT CHANGE ANYTHING ELSE IN THIS SCRIPT ####





#### Script Procedure ####

# Create a tibble with different CCS Zone 2 Northing/Easting reference systems
resTibble <- data.frame(DATUM = c("NAD27", "NAD83"),
                        UNITS = c("FT", "FT"),
                        NAD83_LATITUDE = NA_real_,
                        NAD83_LONGITUDE = NA_real_)



# In each row, assume 'Northing' and 'Easting' are given in that reference system
# Then, convert the values to NAD83 geographic coordinates and 
# save the latitude/longitude values


# NAD27 (FT)
pointVals <- data.frame(x = Easting, y = Northing) %>%
  st_as_sf(coords = 1:2, crs = "epsg:26742") %>%
  st_transform("NAD83") %>%
  st_coordinates()


# Assign these values in 'resTibble'
resTibble$NAD83_LATITUDE[resTibble$DATUM == "NAD27" & 
                           resTibble$UNITS == "FT"] <- pointVals[2]

resTibble$NAD83_LONGITUDE[resTibble$DATUM == "NAD27" & 
                            resTibble$UNITS == "FT"] <- pointVals[1]


# NAD83 (FT)
pointVals <- data.frame(x = Easting, y = Northing) %>%
  st_as_sf(coords = 1:2, crs = "epsg:2226") %>%
  st_transform("NAD83") %>%
  st_coordinates()


# Assign these values in 'resTibble'
resTibble$NAD83_LATITUDE[resTibble$DATUM == "NAD83" & 
                           resTibble$UNITS == "FT"] <- pointVals[2]

resTibble$NAD83_LONGITUDE[resTibble$DATUM == "NAD83" & 
                            resTibble$UNITS == "FT"] <- pointVals[1]


# Output the results

cat("\nNAD83 latitude and longitude coordinates assuming different initial datums:\n\n")

print(resTibble)



cat(paste0("\nThe *", 
           resTibble %>%
             mutate(ERROR = abs(NAD83_LONGITUDE - -120)) %>%
             filter(ERROR == min(ERROR)) %>%
             select(DATUM) %>%
             unlist(use.names = FALSE),
           "* datum gives a longitude closer to -120, so it MIGHT be the correct original datum\n"))



# Remove all variables
remove(list = ls())

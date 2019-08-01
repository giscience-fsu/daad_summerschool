# Filename: 02_raster_solution.R (2019-01-08)
#
# TO DO: Solution to the tasks found in 3_raster.Rmd
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. ATTRIBUTE AND SPATIAL RASTER OPERATIONS
# 3. GEOMETRIC RASTER OPERATIONS
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("sf")
library("raster")
library("dplyr")
library("spData")

#**********************************************************
# 2 ATTRIBUTE AND SPATIAL RASTER OPERATIONS----------------
#**********************************************************


# Attach `data("dem", package = "RQGIS")`. Retrieve the altitudinal values of
# the 10th row.
data("dem", package = "RQGIS")
dem[10, ]

# Sample randomly 10 coordinates of `dem` with the help of the
# `coordinates()`-command, and extract the corresponding altitudinal values.
coords = sp::coordinates(dem)
int = sample(1:nrow(coords), 10)
coords = coords[int, ]
extract(dem, coords)


# Attach `data("random_points", package = "RQGIS")` and find the corresponding
# altitudinal values. Plot altitude against `spri`.
data("random_points", package = "RQGIS")
random_points$dem = extract(dem, random_points)
plot(spri ~ dem, data = random_points)

# Compute the hillshade of `dem` (hint: `?hillShade`). Overlay the hillshade
# with `dem` while using an appropriate level of transparency.
slope = terrain(dem, opt = "slope")
aspect = terrain(dem, opt = "aspect")
hshade = hillShade(slope, aspect)
dev.off()
plot(hshade, col = gray.colors(n = 100),legend = FALSE)
plot(dem, add = TRUE, alpha = 0.4)

#**********************************************************
# 3 GEOMETRIC RASTER OPERATIONS----------------------------
#**********************************************************

# Decrease the resolution of `dem` by a factor of 10. Plot the result.
dem_agg = aggregate(dem, fact = 10, fun = mean)
par(mfrow = c(1, 2))
plot(dem)
plot(dem_agg)

# Reproject `dem` into WGS84. Plot the output next to the original object.
dem_wgs84 = projectRaster(dem, crs = st_crs(4326)$proj4string)
par(mfrow = c(1, 2))
plot(dem)
plot(dem_wgs84)

# Randomly select three points of `random_points`. Convert these into a polygon
# (hint: `st_cast`). Extract all altitudinal values falling inside the created
# polygon Use the polygon to clip `dem`. What is the difference between
# `intersect` and `mask`. Hint: `sf` objects might not work as well with
# **raster** commands as `SpatialObjects`. Assuming your polygon of class `sf`
# is named `poly`, convert it into a `SpatialObject` with `as(sf_object,
# "Spatial`)`.

# randomly sample 3 observations
poly = sample(1:nrow(random_points), 3) %>%
  slice(random_points, .) %>%
  st_geometry %>%
  st_cast("LINESTRING", ids = rep(1, 3)) %>%
  st_cast("POLYGON") %>%
  as(., "Spatial")

# extract values falling inside the polygon
extract(dem, poly)

# clip and mask
clip = raster::intersect(dem, poly)
clip_mask = raster::mask(dem, poly)
par(mfrow = c(1, 2))
plot(clip)
plot(clip_mask)



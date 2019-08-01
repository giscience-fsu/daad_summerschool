# Filename: 02_vector_solution.R (2019-01-08)
#
# TO DO: Solution to the tasks found in 2_vector.Rmd
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. ATTRIBUTE OPERATIONS
# 3. SPATIAL ATTRIBUTE OPERATIONS
# 4. GEOMETRIC OPERATIONS
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("sf")
library("dplyr")
library("spData")

#**********************************************************
# 2 ATTRIBUTE OPERATIONS-----------------------------------
#**********************************************************

# Select all observations of `random_points` (`data("random_points, package =
# "RQGIS"`) which have more than 10 species (column `spri`). Plot the geometry
# of all points and add your selection to the plot in another color
data("random_points", package = "RQGIS")
sel = filter(random_points, spri > 10) %>%
  st_geometry
plot(st_geometry(random_points))
plot(sel, pch = 16, col = "salmon", add = TRUE)
  
# Based on `spri` add a categorical column to `random_points`  with 0-5
# corresponding to `low`, 6-10 to `medium` and >=11 to `high`.
random_points = 
  mutate(random_points,
         cat = cut(spri, breaks = c(0, 5, 10, max(spri) + 1),
                   labels = c("low", "medium", "high"), 
                   include.lowest = TRUE)) %>% as.data.frame
# and the base R way
random_points$cat = cut(random_points$spri, 
                        breaks = c(0, 5, 10, max(random_points$spri) + 1), 
                        labels = c("low", "medium", "high"),
                        include.lowest = TRUE)

# Optional: create two points of class `sfg` and convert them into an object of
# class `sf` which has an `id` and a `geometry` column.
p = st_point(c(1, 1))
p_2 = st_point(c(1, 2))
sfc = st_sfc(list(p, p_2))
st_sf(data.frame(id = 1:2), geometry = sfc)

# also possible:
d = data.frame(id = 1:2, x = 1:2, y = 1:2)
st_as_sf(d, coords = c("x", "y"), crs = 4326)

#**********************************************************
# 3 SPATIAL ATTRIBUTE OPERATIONS---------------------------
#**********************************************************

# Filter the Canterbury region from `nz`, and plot all summits of `nz_height`
# that do not intersect with the Canterbury region (both datasets come with the
# `spData` package).
sel = nz %>% filter(Name == "Canterbury")
nz_height[sel, op = st_disjoint] %>%
  st_geometry %>%
  plot
plot(st_geometry(sel), col = "lightgray", add = TRUE)


# What happens if we spatially join the elevation column of `nz_height` to `nz`?
join = st_join(nz, select(nz_height, elevation))
# rows with more than one match are duplicated, rows without a match are also
# given back with NA
# inner join -> only give back rows with a match
st_join(nz, select(nz_height, elevation), left = FALSE)

#**********************************************************
# 4 GEOMETRIC OPERATIONS-----------------------------------
#**********************************************************


# Create two overlapping circles (see below) and compute and plot their
# geometric intersection. Secondly union the circles.
pts = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
# use the buffer function to create circles from points
circles = st_buffer(pts, dist = 1) 
x = circles[1]
y = circles[2]
# intersection
int = st_intersection(x, y)
plot(circles)
plot(int, col = "salmon", add = TRUE)
# union
plot(st_union(x, y))

# Compute the average population (`total_pop_15`) for each `REGION` of
# `us_states`. Plot your result.
agg = us_states %>%
  group_by(REGION) %>%
  summarize(mean = mean(total_pop_15, na.rm = TRUE))
plot(agg[, "mean"])

# Find out about the CRS of `nz`, reproject it into a geographic CRS (EPSG:
# 4326) and plot the original and unproject `nz` object next to each other.
st_crs(nz)  # EPSG: 2193
nz_repr = st_transform(nz, crs = 4326)
par(mfrow = c(1, 2))
plot(st_geometry(nz), axes = TRUE)
plot(st_geometry(nz_repr), axes = TRUE)


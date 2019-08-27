####################################################
# DAAD Summer School 2019
# GEO 408 FSU Jena
# Alexander Brenning and Patrick Schratz
# Date: Summer 2019
####################################################
# Introduction to RSAGA
# Case study: landslides in Ecuador
####################################################

# install.packages(c('RSAGA', 'here'))
library(RSAGA)
library(here)
library(fs)
library(conflicted)

conflict_prefer("here", "here")

# Please open '2_stats_I.Rproj' in RStudio. Package 'here' will then take care
# of all file path handling throughout the session for you.

##############################################
# Calculate terrain attributes using (R)SAGA
##############################################
## RSAGA is better supported with versions <= 2.1.0

# Let RSAGA find SAGA GIS:
# env = rsaga.env()
# If it can't find it, use the path argument to specify its location.
# On my computer:
# env = rsaga.env(path="C:/Progra~1/SAGA-GIS")
# or
# env = rsaga.env(path="C:/Program Files (x86)/SAGA-GIS")
# Usually, the automatic search should be able to find your installation.
env <- rsaga.env()

# See what you've got:
env

#### Skip this because it's slow:
# Explore available libraries and modules (slow!):
modules <- rsaga.get.modules(env = env)
names(modules)
# We are going to use modules from the following libraries:
modules$io_grid
modules$ta_morphometry
modules$ta_hydrology
#### End Skip

# Convert the DEM ASCII grid to SAGA grid format (.sgrd):
rsaga.esri.to.sgrd(here("R/data/dem.asc"), env = env)

# To be able to see what SAGA_CMD call was actually run, we
# can enable the display.command argument:
rsaga.esri.to.sgrd(here("R/data/dem.asc"), env = env, display.command = TRUE)

# Or, we could do the same thing 'the long way', using
# the low-level rsaga.geoprocessor:
rsaga.get.usage("io_grid", "Import ESRI Arc/Info Grid", env = env)
rsaga.geoprocessor("io_grid", "Import ESRI Arc/Info Grid",
  param = list(GRID = here("R/data/dem.sgrd"), FILE = here("R/data/dem.asc")),
  display.command = TRUE, env = env
)
# ...but we'd rather prefer the more convenient higher-level front-end
# function in RSAGA...


# Now let's calculate a bunch of terrain attributes:

# Slope, aspect, plan and profile curvature:
rsaga.slope.asp.curv(here("R/data/dem"),
  out.slope = here("R/data/slope"), out.cplan = here("R/data/plancurv"),
  out.cprof = here("R/data/profcurv"), method = here("R/data/poly2zevenbergen"),
  env = env
)
## SAGA versions older than 2.1.1 use this instead of rsaga.slope.asp.curv:
## rsaga.local.morphometry("dem", out.slope = "slope",
##        out.hcurv = "plancurv", out.vcurv = "profcurv",
##        method = "poly2zevenbergen", env=env)
# Create a hydrologically more meaningful DEM by filling any local sinks:
rsaga.sink.removal("dem", out.dem = here("R/data/sdem"), method = "fill", env = env)
# Use the multiple flow direction algorithm to calculate catchment area and slope:
rsaga.parallel.processing(here("R/data/sdem"),
  out.carea = here("R/data/carea"),
  out.cslope = here("R/data/cslope"), method = "mfd", env = env
)

# Convert terrain attribute grids into ASCII format:
rsaga.sgrd.to.esri(c(
  here(
    "R/data/slope.asc", "R/data/plancurv.asc", "R/data/profcurv.asc",
    "R/data/carea.asc", "R/data/cslope.asc"
  )
), env = env)

# Delete SAGA grids, we don't need them any more, unless you want to view them in SAGA GIS:
file_delete(here(dir_ls("R/data/", glob = "*.mgrd")))
file_delete(here(dir_ls("R/data/", glob = "*.sgrd")))
file_delete(here(dir_ls("R/data/", glob = "*.sdat")))

##############################################
# Prepare the training and test samples:
# Create random point sample and pick values
# of predictor variables from grids
##############################################

# Read grid header --> find out the extent
hdr <- read.ascii.grid.header(here("R/data/dem"))

# Draw a (stratified) random sample of 500 landslide and 500 non-landslide
# grid points from the inventory:
N <- 25000
# Start with 25000 samples, this sample should be large enough
# to find enough landslide and non-landslide observations:
d <- data.frame(
  x = sample(hdr$xllcenter + c(0:(hdr$ncols - 1)) * hdr$cellsize,
    size = N, replace = TRUE
  ),
  y = sample(hdr$yllcenter + c(0:hdr$nrows - 1) * hdr$cellsize,
    size = N, replace = TRUE
  )
)

# Pick the value of the 'mask' grid for each potential sample location:
d <- pick.from.ascii.grid(d, file = here("R/data/mask"))
# Note that this doesn't use SAGA GIS, therefore no 'env' argument!

# Remove points that are outside the study area:
d <- d[ !is.na(d$mask), ]
d$mask <- NULL

# Get the landslide presence/absence information from the inventory:
d <- pick.from.ascii.grid(d, file = here("R/data/slides89"))
d$slides89 <- factor(!is.na(d$slides89))

# Take an equal number of landslide and non-landslide grid cells:
N.sli <- sum(d$slides89 == "TRUE")
sel <- c(sample(which(d$slides89 == "TRUE"), size = N.sli), sample(which(d$slides89 != "TRUE"), size = N.sli))
d <- d[sel, ]

# Pick values of predictor variables at sample locations:
d <- pick.from.ascii.grids(d, file = here(c(
  "R/data/slope.asc", "R/data/plancurv.asc",
  "R/data/profcurv.asc", "R/data/carea.asc",
  "R/data/cslope.asc", "R/data/distroad.asc",
  "R/data/distdeforest.asc"
)), quiet = FALSE)
summary(d)

# This was time consuming... better save the results in the data folder:
#### save(d, file = "landslides.Rd", compress = TRUE)


# Before we start doing more serious landslide modeling, let's just
# find out how we can use RSAGA to apply a statistical model to
# predict landslide susceptibility based on a stack of grids:
fit <- glm(slides89 ~ slope + distroad, family = binomial, data = d)
# It worked, but let's not trust the model's p-values, they are biased
# because of spatial autocorrelation:
summary(fit)

# Apply this preliminary model on a pixel-by-pixel basis to a stack of
# ASCII raster files with equal extent and resolution:
multi.local.function(
  in.grids = c("slope", "distroad"), out.varnames = "glmpred_prelim",
  fun = grid.predict, fit = fit, control.predict = list(type = "response"),
  quiet = FALSE, path = here("R/data")
)

# multi.local.function has several siblings (local.function, focal.function
# and multi.focal.function) that apply user-defined R functions to local
# moving windows.
# Just to give you a more or less meaningful example, to apply a maximum
# filter over a 3-pixel radius circular moving window to get a more
# 'conservative' landslide susceptibility map, we could do this:
focal.function(
  in.grid = "glmpred_prelim", varnames = "glmpred_prelim_max",
  radius = 3, search.mode = "circle", fun = max,
  mw.to.vector = TRUE, mw.na.rm = TRUE, path = here("R/data")
)

# Instead of 'max', we could use a user-defined function such as:
my.max <- function(x) {
  # A not-so-smart way of implenenting 'max' manually:
  return(sort(na.omit(as.vector(x)), decreasing = TRUE)[1])
}
focal.function(
  in.grid = "glmpred_prelim", varnames = "glmpred_prelim_mymax",
  radius = 3, search.mode = "circle", fun = my.max, path = here("R/data")
)

# That's all for now... Now you have a general overview of the
# capabilities of the RSAGA package for geoprocessing!

## Make a 'nice' displace of your results
library(raster)
hillshade <- raster(here("R/data/hillshade.tif"))
pred <- raster(here("R/data/glmpred_prelim.asc"))
plot(hillshade,
  col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL),
  legend = FALSE, main = "Landslide Susceptibility"
)
plot(pred, alpha = 0.60, add = TRUE)
points(d, cex = 0.5)

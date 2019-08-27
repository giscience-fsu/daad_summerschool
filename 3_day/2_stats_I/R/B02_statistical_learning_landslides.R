####################################################
# DAAD Summer School 2019
# GEO 408 FSU Jena
# Alexander Brenning and Patrick Schratz
# Date: Summer 2019
###################################################
# Statistical learning for classification
# Case study: landslides in Ecuador
####################################################


##############################################
# Prepare the training and test samples:
# Transform some of the variables
##############################################

library(ROCR)
library(gam)
library(rpart)
library(randomForest)

# Load the saved training/test data:
(load(here("R/data/landslides.Rd")))

# A function for plotting ROC curves and
# calculating the area under the ROC curve (AUROC)
# using the ROCR package:
auroc <- function(pred, obs, plot = FALSE) {
  stopifnot(is.logical(obs) | is.factor(obs))
  stopifnot(is.numeric(pred))
  stopifnot(length(pred) == length(obs))
  if (is.factor(obs)) stopifnot(nlevels(obs) == 2)
  require(ROCR)
  predobj <- prediction(pred, obs)
  if (plot) plot(performance(predobj, "tpr", "fpr"))
  auroc <- performance(predobj, measure = "auc")@y.values[[1]]
  return(auroc)
}


# Apply some transformations to the data:
my.trafo <- function(x) {
  # Same for >300m from deforestation:
  x$distdeforest[ x$distdeforest > 300 ] <- 300
  # ...and >300m from road:
  x$distroad[ x$distroad > 300 ] <- 300
  # Convert radians to degrees:
  x$slope <- x$slope * 180 / pi
  x$cslope <- x$cslope * 180 / pi
  # Log-transform size of catchment area - it is extremely skewed:
  x$log.carea <- log10(x$carea)
  return(x)
}

d <- my.trafo(d)


##############################################
# Exploratory Data Analysis
##############################################

# Spinograms are a very fine tool for displaying
# conditional probabilities:
par(mfrow = c(2, 4), mex = 0.7, mar = c(5, 4, 2, 3))
with(d, {
  # spineplot(slides89 ~ distslidespast, ylevels = c("TRUE", "FALSE"), xlab = "Dist. from past landslides", ylab = "", yaxlabels = c("Landslide", "No lsl."),
  #        breaks = c(0,10,40,80,100))
  spineplot(slides89 ~ slope, ylevels = c("TRUE", "FALSE"), xlab = "Slope angle", ylab = "", yaxlabels = c("Landslide", "No landsl."))
  spineplot(slides89 ~ plancurv, ylevels = c("TRUE", "FALSE"), xlab = "Plan curvature", ylab = "", yaxlabels = c("Landslide", "No landsl."))
  spineplot(slides89 ~ profcurv, ylevels = c("TRUE", "FALSE"), xlab = "Profile curvature", ylab = "", yaxlabels = c("Landslide", "No landsl."))
  spineplot(slides89 ~ log.carea, ylevels = c("TRUE", "FALSE"), xlab = "Log. contributing area", ylab = "", yaxlabels = c("Lsl.", "No landslide"))
  spineplot(slides89 ~ cslope, ylevels = c("TRUE", "FALSE"), xlab = "Catchment slope", ylab = "", yaxlabels = c("Landslide", "No landsl."))
  spineplot(slides89 ~ distdeforest,
    ylevels = c("TRUE", "FALSE"), xlab = "Dist. to deforestation", ylab = "", yaxlabels = c("Landslide", "No lsl."),
    breaks = c(0, 25, 50, 100, 250, 300)
  )
  spineplot(slides89 ~ distroad,
    ylevels = c("TRUE", "FALSE"), xlab = "Distance to road", ylab = "", yaxlabels = c("Landslide", "No lsl."),
    breaks = c(0, 25, 50, 100, 250, 300)
  )
})



##############################################
# We won't have time to run all of the following models,
# so please pick only ONE of them.
# The following models are available below:
# - Generalized additive model (GAM) - my favourite!
# - Classification tree (CART)
# - Random forest
##############################################


##############################################
# Generalized Additive Model
##############################################

# Fit a generalized additive model (GAM) with a logistic
# link function by stepwise forward variable selection.
# GAMs are semi-parametric models that combine linear and
# nonlinear terms; they are more flexible than the generalized
# linear model, but still interpretable because of their
# additive structure.

library(gam)
# Predictor variables may be included as linear terms,
# nonlinear smoothing spline terms with two equivalent
# degrees of freedom, or not at all:
gam.scope <- list(
  ~ 1 + distroad + s(distroad, 2),
  ~ 1 + I(distdeforest > 0) + distdeforest + s(distdeforest, 2),
  ~ 1 + log.carea + s(log.carea, 2),
  ~ 1 + cslope + s(cslope, 2),
  ~ 1 + plancurv + s(plancurv, 2),
  ~ 1 + profcurv + s(profcurv, 2),
  ~ 1 + slope + s(slope, 2)
)

# Model building by stepwise variable selection based on the AIC:
fit <- step.Gam(gam(slides89 ~ 1,
  data = d,
  family = binomial
),
scope = gam.scope,
direction = "forward", trace = TRUE
)

summary(fit)

# Plot the transformation functions
# (linear relationship: linear term, no smoothing splines applied)
par(mfrow = c(2, 3), mex = 0.7, mar = c(5, 5, 2, 2))
plot(fit, residuals = FALSE, se = TRUE)
# Compare to Brenning (2008) - stepwise variable selection will
# *a* good model, but there may be other good models around...!
# Stepwise procedures are not very robust - small changes to the
# learning sample may completely change the final model.


# Create ROC plots and calculate AUROC on the training set:
par(mfrow = c(1, 1))
pred <- predict(fit, newdata = d, type = "response")
auroc(pred, d$slides89 == "TRUE", plot = TRUE)

# Apply fitted GAM to the entire study area:
library(RSAGA)
multi.local.function(
  in.grids = c("slope", "carea", "cslope",
               "plancurv", "profcurv",
               "distroad", "distdeforest"),
  out.varnames = "gampred", path = here("R/data"),
  fun = grid.predict, control.predict = list(type = "response"),
  fit = fit, trafo = my.trafo, quiet = FALSE
)

# Plot prediction map
library(raster)
gam.raster <- raster(here("R/data/gampred.asc"))
plot(gam.raster)

##############################################
# Classification Tree
##############################################

library(rpart)
fit <- rpart(slides89 ~ slope + plancurv + profcurv +
  log.carea + cslope + distdeforest + distroad,
data = d,
control = rpart.control(cp = 0.01)
)
par(xpd = TRUE, mfrow = c(1, 1))
plot(fit)
text(fit, use.n = TRUE)

# Training set AUROC:
pred <- predict(fit, newdata = d, type = "prob")[, "TRUE" ]
auroc(pred, d$slides89 == "TRUE", plot = TRUE)

multi.local.function(
  in.grids = c("slope", "plancurv", "profcurv", "carea", "cslope", "distroad", "distdeforest"),
  out.varnames = "ctpred", path = here("R/data"),
  fun = grid.predict, control.predict = list(type = "prob"),
  fit = fit, trafo = my.trafo, predict.column = "TRUE",
  quiet = FALSE
)

ct.raster <- raster(here("R/data/ctpred.asc"))
plot(ct.raster)


##############################################
# Random Forest
##############################################

library(randomForest)
fit <- randomForest(slides89 ~ slope + plancurv + profcurv + log.carea + cslope +
  distdeforest + distroad, data = d, importance = TRUE)
fit

# Variable importance plot:
varImpPlot(fit, type = 1)

# Training set AUROC:
pred <- predict(fit, newdata = d, type = "prob")[, "TRUE" ]
auroc(pred, d$slides89 == "TRUE", plot = TRUE)
# AUROC: 1.00!!! (i.e. nicely overfitting...)


multi.local.function(
  in.grids = c("slope", "plancurv", "profcurv", "carea", "cslope", "distroad", "distdeforest"),
  out.varnames = paste("rfpred", sep = ""), path = here("R/data"),
  fun = grid.predict, control.predict = list(type = "prob"),
  fit = fit, trafo = my.trafo, predict.column = "TRUE",
  quiet = FALSE
)

library(raster)
rf.raster <- raster(here("R/data/rfpred.asc"))
plot(rf.raster)

#####################################################
# Apply prediction alternative with raster package
#####################################################
library(raster)

# Load input rasters - can be other file types, e.g. tif, .img, ...
plancurv <- raster(here("R/data/plancurv.asc"))
profcurv <- raster(here("R/data/profcurv.asc"))

# Don't forget the transformations we applied to the data
log.carea <- log10(raster(here("R/data/carea.asc")))
# or
# carea <- raster('carea.asc')
# log.carea <- calc(carea, fun= function(x){log10(x)})
names(log.carea) <- "log.carea"

rad.to.degree <- function(x) {
  x * 180 / pi
}
slope <- calc(raster(here("R/data/slope.asc")), fun = rad.to.degree)
names(slope) <- "slope"

cslope <- calc(raster(here("R/data/cslope.asc")), fun = rad.to.degree)
names(cslope) <- "cslope"

distroad <- raster(here("R/data/distroad.asc"))
distroad[distroad > 300] <- 300

distdeforest <- raster(here("R/data/distdeforest.asc"))
distdeforest[distdeforest > 300] <- 300


# Stack grids
layers <- stack(list(slope, log.carea, plancurv, profcurv, cslope, distroad, distdeforest))
layers
plot(layers)

# Random forest example
predfun.rf <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")[, 2]
}

rf <- raster::predict(layers, fit, fun = predfun.rf, progress = "text")
writeRaster(rf, here("R/data/pred_rf.img"), format = "HFA", overwrite = TRUE)

plot(rf)

# Make nice plot of results
hillshade <- raster(here("R/data/hillshade.tif"))
plot(hillshade,
  col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL),
  legend = FALSE, main = "Landslide Susceptibility"
)
plot(rf, alpha = 0.60, add = TRUE)
points(d, cex = 0.5)


# This the end of an overview of the use of statistical learning techniques
# for spatial modeling of a binary response variable!

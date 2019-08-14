# International Summer School on Geospatial Data Science with R
# GIScience, Friedrich Schiller University Jena
# 25 August to 1 September 2019
#
# Intro to Modelling Natural Hazards
#
# Author: Jason Goetz
#
# Related papers:
#
# Goetz, J. N., Brenning, A., Petschko, H., & Leopold, P. (2015). 
#   Evaluating machine learning and statistical prediction techniques 
#   or landslide susceptibility modeling. Computers & geosciences, 81, 1-11.
#
# Petschko, H., Brenning, A., Bell, R., Goetz, J., & Glade, T. (2014). 
#   Assessing the quality of landslide susceptibility maps-case study 
#   Lower Austria. Natural Hazards and Earth System Sciences, 14(1), 95-118.
#
# Brenning, A., Schwinn, M., Ruiz-Páez, A. P., & Muenchow, J. (2015). 
#   Landslide susceptibility near highways is increased by 1 order of magnitude 
#   in the Andes of southern Ecuador, Loja province. Natural Hazards and Earth 
#   System Sciences, 15(1), 45-57.
#
#**********************************************************
# CONTENTS ------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND IMPORT DATA 
# 2. SPATIAL SAMPLE OF LANDSLIDE AND NON-LANDSLIDE AREAS
# 3. PERFORM EXPLORATORY DATA ANALYSIS
# 4. MODEL VALIDATION AND COMPARISON
# 5. APPLY SPATIAL PREDICTIONS TO GRIDDED DATA
# 6. VISUALIZE SPATIAL PREDICTIONS (SUSCEPTIBLITY MAP)
#
#**********************************************************
# 1 ATTACH PACKAGES AND IMPORT DATA -----------------------
#**********************************************************

# attach packages
library("raster")     # Handling gridded datasets
library("mgcv")       # Generalized additive modelling
library("sperrorest") # For performing model validation

# specify a working directory (data is stored)
setwd("D:/Workspace/landslide_modelling/data")


# import grid of landslide (polygon) areas
landslides <- raster('slides89.asc')

# import the environmental factors (i.e. predictor variables)
elev <- raster('elev.asc')                  # elevation
profc <- raster('profcurv.asc')             # profile curvature
planc <- raster('plancurv.asc')             # plan curvature
distdeforest <- raster('distdeforest.asc')  # distance to deforested areas
distroad <- raster('distroad.asc')          # distance to roads

# the grid data values for hillslope angle need to be transfomred from radians to degrees
slope <- calc(raster('slope.asc'), fun = function(x){x*180/pi})
names(slope) <- "slope"

# log transform (with base 10) of upslope contributing
logcarea <- calc(raster('carea.asc'), fun = function(x){log10(x)})
names(logcarea) <- "logcarea"

# reclassify distances over 300 to just 300 for dist from deforest
rclmatrix <- matrix(c(300, Inf, 300), ncol = 3, byrow = TRUE)
distdeforest <- reclassify(distdeforest, rclmatrix)
names(distdeforest) <- "distdeforest"

# use the same classification for dist from roads
distroad <- reclassify(distroad, rclmatrix)
names(distroad) <- "distroad"

# build a RasterStack object - a collection of RasterLayer objects with same
# spatial extent and resolution
layers <- stack(list(slope, elev, profc, planc, distdeforest,
					distroad, logcarea))
# visualize gridded data
plot(layers)

# import mask of study area boundary
studymask <- raster('mask.asc')
plot(studymask)


#**********************************************************
# 2 SPATIAL SAMPLE OF LANDSLIDE AND NON-LANDSLIDE AREAS ---
#**********************************************************

# to provide data for our spatially predictive models we need to create
# sample of the response variable: landslide and non-landslides

# set the see of R's random number generator to reproduce the random samples
# if the script is run at a later time
set.seed(1234)

# first, we randomly sample cell locations within landslides
smp.slides <- as.data.frame(sampleRandom(landslides, size = 1000, xy = TRUE))
smp.slides[,3] = NULL #remove the raster value column
plot(landslides)
points(smp.slides)

# now, we randomly sample non-landslide areas but first we need to
# mask out landslide areas
studymask <- mask(studymask, landslides, inverse = TRUE)
plot(studymask)

# randomly sample cell locations of non-landslide cells
smp.noslides <- as.data.frame(sampleRandom(studymask, size = 1000, xy = TRUE))
smp.noslides[,3] = NULL
points(smp.noslides)

# extract predictor variable values from grids
# for the landslide samples
df.slides <- extract(layers, smp.slides, sp = TRUE)
df.slides <- as.data.frame(df.slides)
# add xy values
df.slides$x <- smp.slides$x
df.slides$y <- smp.slides$y
# add variable indicating that these are landslides
df.slides$landslide <- "TRUE"

# for the non-landslide sample
df.noslides <- as.data.frame(extract(layers, smp.noslides, sp = TRUE))
df.noslides$x <- smp.noslides$x
df.noslides$y <- smp.noslides$y
df.noslides$landslide <- "FALSE"

# combine the slides and no slides data into one dataframe
ecuador <- rbind(df.slides, df.noslides)

# encode the landslide variable as a factor
ecuador$landslide <- as.factor(ecuador$landslide)


#**********************************************************
# 3 PERFORM EXPLORATORY DATA ANALYSIS ---------------------
#**********************************************************

# To visualize the potential influencing of the predictor factors
# on landslide occurence, we can produce spinograms, which show
# a plot of the conditional probabilites of lanslides occuring

par(mfrow=c(2,4),mex=0.7,mar=c(5,4,2,3))
with(ecuador, {

  spineplot(landslide ~ slope, ylevels = c("TRUE", "FALSE"), 
            xlab = "Slope angle", ylab = "", yaxlabels = c("Landslide", "No landsl."))
  
  spineplot(landslide ~ plancurv, ylevels = c("TRUE", "FALSE"), 
            xlab = "Plan curvature", ylab = "", yaxlabels = c("Landslide", "No landsl."))
  
  spineplot(landslide ~ profcurv, ylevels = c("TRUE", "FALSE"), 
            xlab = "Profile curvature", ylab = "", yaxlabels = c("Landslide", "No landsl."))
  
  spineplot(landslide ~ logcarea, ylevels = c("TRUE", "FALSE"), 
            xlab = "Log. contributing area", ylab = "", yaxlabels = c("Lsl.", "No landslide"))
  
  spineplot(landslide ~ elev, ylevels = c("TRUE", "FALSE"), 
            xlab = "Elevation", ylab = "", yaxlabels = c("Landslide", "No landsl."))
  
  spineplot(landslide ~ distdeforest, ylevels = c("TRUE", "FALSE"), 
            xlab = "Dist. to deforestation", ylab = "", yaxlabels = c("Landslide", "No lsl."),
            breaks = c(0,25,50,100,250,300))
  
  spineplot(landslide ~ distroad, ylevels = c("TRUE", "FALSE"), 
            xlab = "Distance to road", ylab = "", yaxlabels = c("Landslide", "No lsl."),
            breaks = c(0,25,50,100,250,300))
} )


#**********************************************************
# 4 BUILD (STATISTICAL) LANDSLIDE SUSCEPTIBILITY MODELS ---
#**********************************************************

# in this section we are fitting two statistical models, logistic regression (linear)
# and a generalized additive model (nonlinear)

# first define the formula with the response variable (landslide & non-landslides)
# and predictor variables (terrain attributes and distance to road & forest)
fml <- landslide ~ elev + slope + profcurv + plancurv + logcarea +
  distroad + distdeforest

# fit a logistic regression model
model.lr <- glm(formula = fml, family = 'binomial', data = ecuador)
summary(model.lr)

# to automatically fit a GAM with smoothing functions based on the formula used 
# for the GLM ("fml"), we use the following function
my.gam <- function(formula, data, family = binomial, k = 4) {
  response.name <- as.character(formula)[2]
  predictor.names <- labels(terms(formula))
  categorical <- sapply(data[,predictor.names], is.logical) |
    sapply(data[,predictor.names], is.factor)
  formula <- paste(response.name, "~",
                   paste(predictor.names[categorical], collapse = "+"),
                   paste("s(", predictor.names[!categorical], ", k=", k, ")", collapse = "+"))
  formula <- as.formula(formula)
  fit <- gam(formula, data, family = family)
  return(fit)
}

# fit a generalized additive model (GAM)
model.gam <- my.gam(fml, ecuador)
# display gam model formula
formula(model.gam)
# visualize smoothing functions in the GAM 
par(mfrow=c(2,4))
plot(model.gam)


#**********************************************************
# 4 MODEL VALIDATION AND COMPARISON -----------------------
#**********************************************************

# to make certain that performance of our model is not by chance,
# we can perform repeated cross-validation, or in this example, 
# repated cross-validation based on splitting the data into
# spatially defined subsets (i.e., spatial cross-validation)

# since repetead (spatial) cross-validation can be computationally
# intensive, we will perform spatial cv using 5-folds and 50 reptitions

# peform 50 repeated, 5-fold spatial cv for the logistic regression model
lr.results <- sperrorest(formula = fml, data = ecuador, coords = c("x","y"),
                          model_fun = glm, 
                          model_args = list(family = 'binomial'),
                          pred_args = list(type="response"),
                          smp_fun = partition_kmeans, 
                          smp_args = list(repetition = 1:50, nfold = 5))

# let's get the values auroc recorded for each repitition
lr.auroc.test <- unlist(summary(lr.results$error_rep,level=1)[,"test_auroc"])
# and summarize the peformance results
mean(lr.auroc.test)
sd(lr.auroc.test)

# peform 50 repeated, 5-fold spatial cv for the generalized additive model
gam.results <- sperrorest(formula = fml, data = ecuador, coords = c("x","y"),
                         model_fun = my.gam, 
                         pred_args = list(type="response"), 
                         smp_fun = partition_kmeans, 
                         smp_args = list(repetition = 1:50, nfold = 5))

gam.auroc.test <- unlist(summary(gam.results$error_rep,level=1)[,"test_auroc"])
mean(gam.auroc.test)
sd(gam.auroc.test)

# we can also summarize our results as a box plot
results_auroc <- data.frame(LR = lr.auroc.test, GAM = gam.auroc.test)
par(mfrow=c(1,1))
boxplot(results_auroc, ylab = "Area under ROC")

# OK great we have numbers! but where did they come from?
# let's Visualize resampling for non-spatial cross-validation
resamp.cv = partition_cv(ecuador, nfold=5, repetition=1)
plot(resamp.cv, ecuador)

# and resampling for spatial cross-validation using k-means
resamp.spcv = partition_kmeans(ecuador, nfold=5, repetition=1)
plot(resamp.spcv, ecuador)


#**********************************************************
# 5 APPLY SPATIAL PREDICTIONS TO GRIDDED DATA -------------
#**********************************************************

# now that we have validated our model peformance, we can create
# a spatial model prediction by applying the fitted model predictions
# to a gridded dataset (our RasterStack object)
pred.lr<- raster::predict(layers, model.lr, type = "response", progress = TRUE)
pred.gam<- raster::predict(layers, model.gam, type = "response", progress = TRUE)

# export to a raster format
writeRaster(pred.lr, "susc_model_lr.tiff", format = "GTiff", overwrite = TRUE)
writeRaster(pred.gam, "susc_model_gam.tiff", format = "GTiff", overwrite = TRUE)

# visualize spatial make 'nice' displace of your results
par(mfrow=c(1,2), mex=0.7, c(5, 4, 4, 2) + 0.1)
hillshade <- raster('hillshade.tif')

# Logistic regression model
plot(hillshade, 
	col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL),
	legend = FALSE, main = "Logistic regression", axes=FALSE, box=FALSE)
plot(pred.lr, alpha=0.60, add=TRUE, axes=FALSE, box=FALSE)
points(smp.slides, cex = 0.5)

# GAM model
plot(hillshade, 
     col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL),
     legend = FALSE, main = "GAM", axes=FALSE, box=FALSE)
plot(pred.gam, alpha=0.60, add=TRUE, axes=FALSE, box=FALSE)
points(smp.slides, cex = 0.5)


#**********************************************************
# 6 VISUALIZE SPATIAL PREDICTIONS (SUSCEPTIBLITY MAP) -----
#**********************************************************

# to facilitate a 'fair' comparision of maps we classify the maps 
# into classes (levels)having equal precentage of overall areas, in this example,
# these class levels are based on the 50th, 75th, 90th and 95th percentiles
brks.lr <- stats::quantile(getValues(pred.lr), probs = c(.5, .75, .90, .95), na.rm = T)
brks.gam <- stats::quantile(getValues(pred.gam), probs = c(.5, .75, .90, .95), na.rm = T)

# reclassify the prediction grids using the class breaks
m.lr <- matrix(c(0, brks.lr))
m.lr <- cbind(m.lr, c(brks.lr, 1))
rclmat.lr <- cbind(m.lr, c(1, 2, 3, 4, 5))
rclpred.lr <- reclassify(pred.lr, rclmat.lr)

m.gam <- matrix(c(0, brks.gam))
m.gam <- cbind(m.gam, c(brks.gam, 1))
rclmat.gam <- cbind(m.gam, c(1, 2, 3, 4, 5))
rclpred.gam <- reclassify(pred.gam, rclmat.gam)

# export spatial predictions with defined class breaks
writeRaster(rclpred.lr, "recl_susc_model_lr.tiff", format = "GTiff", overwrite = TRUE)
writeRaster(rclpred.gam, "recl_susc_model_gam.tiff", format = "GTiff", overwrite = TRUE)

# define the colours for each class using RGB
class_colours <- c(rgb(255, 255, 128, max=255), # very low
                   rgb(113, 235, 47, max=255),  # low
                   rgb(61, 184, 104, max=255),  # medium
                   rgb(33, 110, 158, max=255),  # high
                   rgb(12, 16, 120, max=255))   # very hight

# Visualize susceptibility model results side by side
par(mfrow=c(1,2))

# logistic regression
plot(hillshade, 
     col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL),
     legend = FALSE, main = "Logistic regression", axes=FALSE, box=FALSE)
plot(rclpred.lr, alpha=0.60, add=TRUE, axes=FALSE, box=FALSE, legend=FALSE,
     col=class_colours)
points(smp.slides, cex = 0.5)
legend("bottomleft", legend = c("Very low", "Low", "Medium", "High", "Very high"), 
       fill = class_colours, title = "Susceptiblity index", bty="n",
       title.adj=0, text.font = 2, cex=0.7)
scalebar(500,  xy=c(715500, 9557100), type='bar', cex=0.6, below="meters")

# generalized additive model
plot(hillshade, 
     col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL),
     legend = FALSE, main = "GAM", axes=FALSE, box=FALSE)
plot(rclpred.gam, alpha=0.60, add=TRUE, axes=TRUE, box=FALSE, legend=FALSE,
     col=class_colours)
points(smp.slides, cex = 0.5)
legend("bottomleft", legend = c("Very low", "Low", "Medium", "High", "Very high"), 
       fill = class_colours, title = "Susceptiblity index", bty="n",
       title.adj=0, text.font = 2, cex=0.7)
scalebar(500,  xy=c(715500, 9557100), type='bar', cex=0.6, below="meters")



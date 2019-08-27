####################################################
# Environmental Statistics and GeoComputation
# GEO 408 FSU Jena
# Alexander Brenning and Jason Goetz
# Date: Winter 2019
####################################################
# Performance assessment using spatial cross-validation
# Case study: landslides in Ecuador
####################################################

# Brenning, A. 2012. Spatial cross-validation and bootstrap for the 
# assessment of prediction rules in remote sensing: the R package 
# 'sperrorest'. IEEE International Symposium on Geoscience and 
# Remote Sensing IGARSS, 5372-5275.
#
# Brenning, A., S. Long & P. Fieguth. 2012. Detecting rock glacier 
# flow structures using Gabor filters and IKONOS imagery. Remote 
# Sensing of Environment, 125: 227-237.
#
# Russ, G. & A. Brenning. 2010. Spatial variable importance assessment
# for yield prediction in Precision Agriculture. In Advances in 
# Intelligent Data Analysis IX, Proceedings, 9th International Symposium, 
# IDA 2010, Tucson, AZ, USA, 19-21 May 2010. Lecture Notes in Computer 
# Science, 6065 LNCS: 184-195.

# Change to appropriate working directory
# with landslides.Rd data file:
setwd("D:/ecuador/data")

install.packages("devtools")
library(devtools)
install_github("pat-s/sperrorest")

# Fewer decimal places - works better for instructor:
options(digits=4)

library(sperrorest)
library(mgcv)
library(rpart)
library(randomForest)

# parallelization mode for sperrorest:
# the default argument will normally do the job:
#par_args <- list(par_mode = "foreach", par_units = NULL, par_option = NULL)
# but there are some problems on the lab computers - use this:
par_args <- list(par_mode = "sequential")
# Also note that warnings and errors may be suppressed
# when using sperrorest in parallel mode!

# Load the saved training/test data:
(load("landslides.Rd"))

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

# Make sure we use the exact same formula in all models:
fo <- slides89 ~ slope + plancurv + profcurv + log.carea + cslope + distroad + distdeforest


#####################################################
# Start by exploring spatial resampling in sperrorest
#####################################################

# Resampling for non-spatial cross-validation:
# 5 folds, 2 repetitions for illustration
resamp <- partition_cv(d, nfold=5, repetition=1:2)
plot(resamp, d)

# Take a look inside:
# first repetition, second fold: row numbers 
# of observations in the test set:
idx <- resamp[["1"]][[2]]$test
# test sample that would be used in this particular 
# repetition and fold:
d[ idx , ]




# Resampling for spatial cross-validation 
# using k-means clustering of coordinates:
resamp <- partition_kmeans(d, coords = c("x","y"),
                           nfold=5, repetition=1:2)
plot(resamp, d)
# Repeat this to get different partitions (depends on
# data set; works better with nfold=10).
# Use the seed1 argument to make your partitioning
# reproducible.


library(rpart)
# A wrapper function that extracts the predicted probabilities from rpart predictions:
mypred_rpart <- function(object, newdata) {
  predict(object, newdata, type="prob")[,2]
}
# Control parameters for rpart:
ctrl <- rpart.control(cp=0.003)

# Perform 5-repeated 5-fold non-spatial cross-validation:
res <- sperrorest(formula = fo, data = d, coords = c("x","y"),
                 model_fun = rpart, model_args = list(control=ctrl),
                 pred_fun = mypred_rpart, 
                 smp_fun = partition_cv, smp_args = list(repetition=1:50, nfold=5),
                 par_args = par_args)
# In "real life" we should use nfold=10 and repetition=1:100 !!!
# plot(res$represampling, d) # may be too large to plot properly
summary(res$error_rep)
# More detailed, for each repetition:
summary(res$error_rep, level=1)
# Let's focus on AUROC on training and test set:
summary(res$error_rep)[c("train_auroc","test_auroc"),1:2]
# Degree of overfitting (the more negative, the worse):
diff( summary(res$error_rep)[c("train_auroc","test_auroc"),"mean"] )

# Let's get the values auroc recorded for each repitition
auroc.test <- unlist(summary(res$error_rep,level=1)[,"test_auroc"])
auroc.train <- unlist(summary(res$error_rep,level=1)[,"train_auroc"])
mean(auroc.test)
mean(auroc.train)

# We can also summarize our results as a box plot
df_auroc <- data.frame(training = auroc.train, testing = auroc.test)
boxplot(df_auroc, ylab = "AUROC")


# Perform 5-repeated 5-fold SPATIAL cross-validation:
res <- sperrorest(formula = fo, data = d, coords = c("x","y"),
                 model_fun = rpart, model_args = list(control=ctrl), 
                 pred_fun = mypred_rpart, 
                 smp_fun = partition_kmeans, smp_args = list(repetition=1:5, nfold=5),
                 par_args = par_args)
# Let's focus on AUROC on training and test set:
summary(res$error_rep)[c("train_auroc","test_auroc"),1:2]
# Degree of overfitting (the more negative, the worse):
diff( summary(res$error_rep)[c("train_auroc","test_auroc"),"mean"] )
# Spatial cross-validation reveals overfitting that was not detected
# by non-spatial cross-validation...


# Now let's tidy up the code and do this for the GAM, CART and random forest.
# (Pick one method in class since this is computationally intensive.)



####################################
# Generalized Additive Model
####################################

library(mgcv)
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

# Check that our wrapper function is working:
#fit <- my.gam(fo,d)
#summary(fit)
#plot(fit)

# Perform 5-repeated 5-fold non-spatial cross-validation:
res <- sperrorest(formula = fo, data = d, coords = c("x","y"),
                  model_fun = my.gam, 
                  pred_args = list(type="response"), 
                  smp_fun = partition_cv, smp_args = list(repetition=1:5, nfold=5),
                  par_args = par_args)
summary(res$error_rep)[c("train_auroc","test_auroc"),1:2]
# Degree of overfitting (the more negative, the worse):
diff( summary(res$error_rep)[c("train_auroc","test_auroc"),"mean"] )


# Perform 5-repeated 5-fold spatial cross-validation:
res <- sperrorest(formula = fo, data = d, coords = c("x","y"),
                 model_fun = my.gam, 
                 pred_args = list(type="response"), 
                 smp_fun = partition_kmeans, smp_args = list(repetition=1:5, nfold=5),
                 par_args = par_args)
summary(res$error_rep)[c("train_auroc","test_auroc"),1:2]
# Degree of overfitting (the more negative, the worse):
diff( summary(res$error_rep)[c("train_auroc","test_auroc"),"mean"] )



####################################
# Classification tree
####################################

library(rpart)
# A wrapper function that extracts the predicted probabilities from rpart predictions:
mypred_rpart <- function(object, newdata) {
  predict(object, newdata, type="prob")[,2]
}
# Control parameters for rpart:
ctrl <- rpart.control(cp=0.003)

# Perform 5-repeated 5-fold non-spatial cross-validation:
res <- sperrorest(formula = fo, data = d, coords = c("x","y"),
                  model_fun = rpart, model_args = list(control=ctrl),
                  pred_fun = mypred_rpart, 
                  smp_fun = partition_cv, smp_args = list(repetition=1:5, nfold=5),
                  par_args = par_args)
summary(res$error_rep)[c("train_auroc","test_auroc"),1:2]
# Degree of overfitting (the more negative, the worse):
diff( summary(res$error_rep)[c("train_auroc","test_auroc"),"mean"] )


# Perform 5-repeated 5-fold SPATIAL cross-validation:
res <- sperrorest(formula = fo, data = d, coords = c("x","y"),
                  model_fun = rpart, model_args = list(control=ctrl), 
                  pred_fun = mypred_rpart, 
                  smp_fun = partition_kmeans, smp_args = list(repetition=1:5, nfold=5),
                  par_args = par_args)
summary(res$error_rep)[c("train_auroc","test_auroc"),1:2]
# Degree of overfitting (the more negative, the worse):
diff( summary(res$error_rep)[c("train_auroc","test_auroc"),"mean"] )





####################################
# Random forest
####################################

library(randomForest)

# A wrapper function that extracts the predicted 
# probabilities from rpart predictions:
mypred.rf <- function(object, newdata) {
  predict(object, newdata, type="prob")[,2]
}

# Perform 5-repeated 5-fold non-spatial cross-validation:
res <- sperrorest(formula = fo, data = d, coords = c("x","y"),
                 model_fun = randomForest, pred_fun = mypred.rf, 
                 smp_fun = partition_cv, smp_args = list(repetition=1:5, nfold=5),
                 par_args = par_args)
summary(res$error_rep)[c("train_auroc","test_auroc"),1:2]
# Degree of overfitting (the more negative, the worse):
diff( summary(res$error_rep)[c("train_auroc","test_auroc"),"mean"] )

# Perform 5-repeated 5-fold spatial cross-validation:
res <- sperrorest(formula = fo, data = d, coords = c("x","y"),
                  model_fun = randomForest, pred_fun = mypred.rf, 
                  smp_fun = partition_kmeans, smp_args = list(repetition=1:5, nfold=5),
                  par_args = par_args)
summary(res$error_rep)[c("train_auroc","test_auroc"),1:2]
# Degree of overfitting (the more negative, the worse):
diff( summary(res$error_rep)[c("train_auroc","test_auroc"),"mean"] )



#####################################################
# Variable importance assessment
#####################################################

# The following code performs a permutation-based variable 
# importance assessment only for rpart, but it will be easy
# to adapt to the situation of other classifiers.

# Of course in 'real life' we would increase imp_permutations and
# use repetition=1:100 to get results that do not depend on a small
# number of particular random partitionings and permutations.

# Perform 3-repeated 5-fold non-spatial cross-validation:
res <- sperrorest(formula = fo, data = d, coords = c("x","y"),
                 model_fun = rpart, model_args = list(control=ctrl), 
                 pred_fun = mypred_rpart, 
                 smp_fun = partition_cv, smp_args = list(repetition=1:3, nfold=5),
                 importance = TRUE, imp_permutations = 50,
                 par_args = par_args)
imp <- summary(res$importance)
nsp.imp <- imp$mean.auroc
names(nsp.imp) <- rownames(imp)
par(mfrow=c(2,1),las=1,mar=c(4,6,1,1))
barplot(sort(nsp.imp),horiz=TRUE,xlab="AUROC Decrease",main="Non-spatial variable importance")

# Perform 5-repeated 5-fold spatial cross-validation:
res <- sperrorest(formula = fo, data = d, coords = c("x","y"),
                  model_fun = rpart, model_args = list(control=ctrl), 
                  pred_fun = mypred_rpart, 
                  smp_fun = partition_kmeans, smp_args = list(repetition=1:3, nfold=5),
                  importance = TRUE, imp_permutations = 50,
                  par_args = par_args)
imp = summary(res$importance)
sp.imp = imp$mean.auroc
names(sp.imp) <- rownames(imp)
barplot(sp.imp[names(sort(sp.imp))],horiz=TRUE,xlab="AUROC Decrease",main="Spatial variable importance")

# Well that's it - now you know how to assess the predictive performance
# of models in a spatial context, or the spatial "transferability" of a
# model. You also learned how to assess the variable importance in the
# context of complex machine-learning models.

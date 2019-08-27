####################################################
# DAAD Summer School 2019
# GEO 408 FSU Jena
# Alexander Brenning and Patrick Schratz
# Date: Summer 2019
###################################################
# Hyperparameter tuning
# Case study: Mapping crop types from Landsat imagery
#####################################################

library(here)
library(conflicted)

conflict_prefer("here", "here")

# Fewer decimal places - works better for instructor:
options(digits = 4)

library(sperrorest)
library(mgcv)
library(rpart)
library(randomForest)

# parallelization mode for sperrorest:
# the default argument will normally do the job:
# par_args <- list(par_mode = "foreach", par_units = NULL, par_option = NULL)
# if you want to test parallelization, use the following
# par_args <- list(par_mode = "foreach")
par_args <- list(par_mode = "sequential")
# (Also note that warnings and errors may be suppressed
# when using sperrorest in parallel mode!)

# Load the data set:
(load(here("R/data/Maipo_fields.Rdata")))

# Make sure we use the exact same formula in all models:
fo <- croptype ~ ndvi01 + ndvi02 + ndvi03 + ndvi04 + ndvi05 +
  ndvi06 + ndvi07 + ndvi08


#####################################################
# Start by exploring sensitivity of classification
# tree to maxdepth hyperparameter;
# turn built-in tuning off: cp = 0
#####################################################

library(rpart)


# Classification trees with different maxdepth, no pruning:
fit <- rpart(fo, data = d, control = rpart.control(cp = 0, maxdepth = 3))
par(xpd = TRUE, mfrow = c(1, 1))
plot(fit)
text(fit, use.n = TRUE)

fit <- rpart(fo, data = d, control = rpart.control(cp = 0, maxdepth = 10))
plot(fit)
text(fit, use.n = TRUE)

# What strategy works best?
# a) "Guess" the "right" maxdepth (or cp) value?
# b) Optimize it as part of the model building process?

# Until now we have only learned how to perform cross-validation
# on models with fixed hyperparameters:

# Control parameters for rpart:
ctrl <- rpart.control(cp = 0, maxdepth = 10)

# Perform 5-repeated 5-fold spatial cross-validation:
res <- sperrorest(
  formula = fo, data = d, coords = c("utmx", "utmy"),
  model_fun = rpart, model_args = list(control = ctrl),
  pred_args = list(type = "class"),
  smp_fun = partition_kmeans, smp_args = list(repetition = 1:5, nfold = 10),
  par_args = par_args
)
# Cross-validation estimate of AUROC:
summary(res$error_rep)["test_accuracy", 1]

# We can use this to explore how hyperparameter values influence
# cross-validation estimates of AUROC. Let's do that, as a first step.

# Let's put this inside a wrapper function:

estimate_acc <- function(maxdepth) {
  # Control parameters for rpart:
  ctrl <- rpart.control(cp = 0, maxdepth = maxdepth)
  # Perform 5-repeated 5-fold spatial cross-validation:
  res <- sperrorest(
    formula = fo, data = d, coords = c("utmx", "utmy"),
    model_fun = rpart, model_args = list(control = ctrl),
    pred_args = list(type = "class"),
    smp_fun = partition_kmeans,
    # simplified settings:
    smp_args = list(repetition = 1:1, nfold = 10),
    par_args = par_args,
    progress = FALSE
  )
  # Cross-validation estimate of accuracy:
  acc <- summary(res$error_rep)["test_accuracy", 1]
  return(acc)
}
# Note that this is a bit 'dirty' because this function accesses global
# variables such as 'd' and 'fo'.

maxdepths <- c(1, 2, 3, 5, 7, 10, 15, 20, 25, 30)
accs <- sapply(maxdepths, estimate_acc)
plot(accs ~ maxdepths, type = "l")
points(accs ~ maxdepths, pch = "+")
cbind(maxdepths, accs)

# What have we learned?



#####################################################
# *Properly* tuning hyperparameters for
# predictive modelling
#####################################################

selftuning_rpart <- function(formula, data,
                             maxdepths = c(1, 2, 3, 5, 7, 10, 15, 20, 25, 30)) {
  accs <- rep(NA, length(maxdepths))
  for (i in 1:length(maxdepths)) {
    ctrl <- rpart.control(cp = 0, maxdepth = maxdepths[i])
    # Note that this sperrorest call MUST use the 'formula' and 'data'
    # objects that were passed to this function - not the 'fo' and 'd'
    # objects!
    res <- sperrorest(
      formula = formula, data = data, coords = c("utmx", "utmy"),
      model_fun = rpart, model_args = list(control = ctrl),
      pred_args = list(type = "class"),
      smp_fun = partition_kmeans,
      # using simplified settings for speed-up:
      smp_args = list(repetition = 1:1, nfold = 10),
      par_args = list(par_mode = "sequential"),
      progress = FALSE
    )
    accs[i] <- summary(res$error_rep)["test_accuracy", 1]
  }
  opt.maxdepth <- maxdepths[ which.max(accs) ]
  cat("Optimal tree depth:", opt.maxdepth, "\n")

  # Note that this sperrorest call MUST use the 'formula' and 'data'
  # objects that were passed to this function - not the 'fo' and 'd'
  # objects!
  fit <- rpart(formula,
    data = data,
    control = rpart.control(cp = 0, maxdepth = opt.maxdepth)
  )
  return(fit)
}

fit <- selftuning_rpart(fo, d)
plot(fit)
text(fit, use.n = TRUE)

# How accurate is this particular fitted model, fit?
# Can we even tell?



# Perform 5-repeated 10-fold spatial cross-validation
# on selftuned_rpart:
res <- sperrorest(
  formula = fo, data = d, coords = c("utmx", "utmy"),
  model_fun = selftuning_rpart,
  pred_args = list(type = "class"),
  smp_fun = partition_kmeans,
  # simplified settings, not patient enough:
  smp_args = list(repetition = 1:1, nfold = 10),
  par_args = par_args
)
# Cross-validation estimate of overall accuracy:
summary(res$error_rep)["test_accuracy", 1]



#####################################################
# (Almost) the same thing using the 'mlr' package
#####################################################

# Advantages
# - Hyperparameter tuning built into mlr package - no need to
#   manually program the optimizer as in our selftuning_rpart() function
# - Runs faster

# install.packages("mlr")
library("mlr")

xy <- d[, c("utmx", "utmy")]
d2 <- d[, all.vars(fo) ] # only response and predictors

task <- makeClassifTask(
  data = d2, target = "croptype",
  coordinates = xy
)
task

lrnr <- makeLearner("classif.rpart", predict.type = "response")

resampling <- makeResampleDesc("SpRepCV", fold = 10, reps = 5)

out <- mlr::resample(
  learner = lrnr, task = spatial.task,
  resampling = resampling, measures = list(acc)
)
mean(out$measures.test$acc)


# use random search to optimize hyperparameter:
tnr <- makeTuneControlRandom(maxit = 10)

# define the outer limits of the randomly selected hyperparameters
ps <- makeParamSet(
  makeNumericParam("cp", lower = 0, upper = 0, default = 0, tunable = FALSE),
  makeIntegerParam("maxdepth", lower = 1, upper = 30)
)
simple_resampling <- makeResampleDesc("SpCV", iters = 10)

selftuning_lrnr <- makeTuneWrapper(
  learner = lrnr,
  resampling = simple_resampling,
  par.set = ps,
  control = tnr,
  show.info = TRUE,
  measures = mlr::acc
)

result <- mlr::resample(
  learner = selftuning_lrnr,
  task = task,
  resampling = resampling,
  extract = getTuneResult,
  measures = mlr::acc
)


# For further details, e.g. on SVM with hyperparameter tuning
# or parallelization, see:
# - Jannes's book chapter:
#   https://geocompr.robinlovelace.net/spatial-cv.html
# - Patrick's blog posts:
#   https://mlr.mlr-org.com/articles/tutorial/handling_of_spatial_data.html
#   https://www.r-spatial.org/r/2018/03/03/spatial-modeling-mlr.html

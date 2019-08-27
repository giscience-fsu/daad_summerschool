####################################################
# DAAD Summer School 2019
# GEO 408 FSU Jena
# Alexander Brenning and Patrick Schratz
# Date: Summer 2019
###################################################
# Feature selection
# Case study: Mapping crop types from Landsat imagery
#####################################################

library(here)
library(conflicted)

conflict_prefer("here", "here")

# Fewer decimal places - works better for instructor:
options(digits = 4)

library(sperrorest)
library(mda)

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


# Add 10 noise variables for illustrative purposes:
for (i in 0:9) {
  d[paste("rdmvar", i, sep = "")] <- rnorm(nrow(d))
}

# fo <- croptype ~ ndvi01 + ndvi02 + ndvi03 + ndvi04 + ndvi05 +
#  ndvi06 + ndvi07 + ndvi08

# Use a larger feature set than previously,
# including the 'noise' variables:
fo <- as.formula(
  paste(
    "croptype ~",
    paste(paste("b", outer(1:8, 2:7, paste, sep = ""), sep = ""),
      collapse = "+"
    ), "+",
    paste(paste("ndvi0", 1:8, sep = ""), collapse = "+"), "+",
    paste(paste("ndwi0", 1:8, sep = ""), collapse = "+"), "+",
    paste(paste("rdmvar", 0:9, sep = ""), collapse = "+")
  )
)
fo


#####################################################
# Plain LDA, no feature selection
#####################################################

# Note that fda with no additional arguments performs lda.

# Perform 1-repeated 10-fold spatial cross-validation:
res_null <- sperrorest(
  formula = fo, data = d, coords = c("utmx", "utmy"),
  model_fun = fda,
  smp_fun = partition_kmeans, smp_args = list(repetition = 1:1, nfold = 10),
  par_args = par_args
)
# Cross-validation estimate of misclassification error rate:
mer_lda <- summary(res_null$error_rep)["test_error", 1]
mer_lda


#####################################################
# LDA with features selected by filter method
#####################################################

library(pROC)

# Take a look at multiclass AUROCs of each predictor versus response:
response <- attr(terms(fo), "response")
predictors <- attr(terms(fo), "term.labels")
maurocs <- sapply(
  predictors,
  function(x) multiclass.roc(d[, response], d[, x])$auc
)
sort(maurocs, decr = TRUE)

# Note that the multiclass.roc function can be very slow for large
# data sets!

filterlda <- function(formula, data, filter.k = Inf) {
  response <- all.vars(formula)[1]
  predictors <- attr(terms(formula), "term.labels")
  if (filter.k < length(predictors)) {
    maurocs <- sapply(
      predictors,
      function(x) multiclass.roc(data[, response], data[, x])$auc
    )
    filter_preds <- names(sort(maurocs, decr = TRUE)[1:filter.k])
    formula <- as.formula(paste(
      response, "~",
      paste(filter_preds, collapse = "+")
    ))
  }
  fit <- fda(formula, data)
  return(fit)
}

# Try it out:
fit <- filterlda(fo, d, filter.k = 4)
fit
formula(fit)

estimate_mer_filterlda <- function(filter.k) {
  res <- sperrorest(
    formula = fo, data = d, coords = c("utmx", "utmy"),
    model_fun = filterlda,
    model_args = list(filter.k = filter.k),
    smp_fun = partition_kmeans,
    # simplified settings:
    smp_args = list(repetition = 1:1, nfold = 10),
    par_args = par_args,
    progress = FALSE
  )
  # Cross-validation estimate of MER:
  err <- summary(res$error_rep)["test_error", 1]
  return(err)
}

ks <- c(1, 2, 3, 4, 6, 10, 15, 20, 35, 50, length(all.vars(fo)) - 1)
mer_filterlda <- sapply(ks, estimate_mer_filterlda)
plot(mer_filterlda ~ ks,
  type = "l", ylim = c(0, 0.5),
  xlab = "# filtered features", ylab = "misclassification error rate",
  main = "LDA with features filtered using mAUROC"
)
points(mer_filterlda ~ ks, pch = "+")
abline(h = mer_lda, lty = "dashed")


#####################################################
# Shrinkage: LDA with ridge penalty
#####################################################

# Use function fda with method=gen.ridge.
# Check sensitivity to penalty hyperparameter lambda.

estimate_mer_rlda <- function(lambda) {
  # Perform 1-repeated 10-fold spatial cross-validation:
  res <- sperrorest(
    formula = fo, data = d, coords = c("utmx", "utmy"),
    model_fun = fda,
    model_args = list(method = gen.ridge, lambda = lambda),
    smp_fun = partition_kmeans,
    # simplified settings:
    smp_args = list(repetition = 1:1, nfold = 10),
    par_args = par_args,
    progress = FALSE
  )
  # Cross-validation estimate of accuracy:
  err <- summary(res$error_rep)["test_error", 1]
  return(err)
}

lambdas <- 10^seq(-8, 11, length = 15)
mer_rlda <- sapply(lambdas, estimate_mer_rlda)
plot(mer_rlda ~ log10(lambdas),
  type = "l", ylim = c(0, 0.5),
  xlab = "log10(lambda)", ylab = "misclassification error rate",
  main = "LDA with Ridge Penalty"
)
points(mer_rlda ~ log10(lambdas), pch = "+")
abline(h = mer_lda, lty = "dashed")


#####################################################
# Dimension reduction using PCA
#####################################################

fo_pca <- as.formula(paste("~", paste(all.vars(fo)[-1], collapse = "+")))
fit_pca <- prcomp(fo_pca, data = d, scale. = TRUE)
summary(fit_pca)
# How many PCs explain 90%, 95% of the feature set's variance?
plot(fit_pca, xlab = "PCs", main = "Principal component analysis of feature set")

pclda <- function(formula, data, pca.k = Inf) {
  response <- all.vars(formula)[1]
  d.response <- data[, response]
  predictors <- all.vars(formula)[-1]
  pca.k <- min(length(predictors), pca.k)

  fo_pca <- as.formula(paste("~", predictors, collapse = "+"))
  fit_pca <- prcomp(fo_pca, data = data, scale. = TRUE, retx = TRUE)
  data <- data.frame(fit_pca$x[, 1:pca.k])
  data[response] <- d.response

  formula <- paste(
    response, "~",
    paste("PC", 1:pca.k, sep = "", collapse = "+")
  )

  fit <- fda(formula, data)

  fit$fit_pca <- fit_pca
  fit$pca.k <- pca.k

  return(fit)
}

# Check it out:
fit <- pclda(fo, d, pca.k = 3)
fit
fit <- pclda(fo, d, pca.k = 20)
fit

# We'll need a wrapper function for the predict method that
# handles the principal component transformation on the test set:

predict_pclda <- function(object, newdata, pca.k = object$pca.k, ...) {
  # Calculate PCs from PCA transformation fitted to the training data
  # and features from the 'newdata' data set:
  newdata <- data.frame(predict(object$fit_pca, newdata)[, 1:pca.k])
  pred <- predict(object, newdata, ...)
  return(pred)
}

# Try out our wrapper function on the training set:
fit <- pclda(fo, d, pca.k = 20)
mean(d$croptype != predict_pclda(fit, d)) # mer; same as above


estimate_mer_pclda <- function(k) {
  res <- sperrorest(
    formula = fo, data = d, coords = c("utmx", "utmy"),
    model_fun = pclda, model_args = list(pca.k = k),
    pred_fun = predict_pclda,
    smp_fun = partition_kmeans,
    # simplified settings:
    smp_args = list(repetition = 1:1, nfold = 10),
    par_args = par_args,
    progress = FALSE
  )
  # Cross-validation estimate of MER:
  err <- summary(res$error_rep)["test_error", 1]
  return(err)
}

# ks <- c(2:(length(all.vars(fo))-1) # too slow
ks <- c(2, 3, 4, 6, 9, 12, 16, 20, 25, 30, 40, 50, length(all.vars(fo)) - 1)

mer_pclda <- sapply(ks, estimate_mer_pclda)
plot(mer_pclda ~ ks,
  type = "l", ylim = c(0, 0.5),
  xlab = "# PCs", ylab = "misclassification error rate",
  main = "PC-LDA"
)
points(mer_pclda ~ ks, pch = "+")
abline(h = mer_lda, lty = "dashed")
# What MER would you have achieved when using PCs
# that explained 90% or 95% of the feature set's variance?

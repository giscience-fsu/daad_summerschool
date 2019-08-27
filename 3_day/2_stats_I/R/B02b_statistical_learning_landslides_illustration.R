####################################################
# Environmental Statistics and GeoComputation
# GEO 408 FSU Jena
# Alexander Brenning and Jason Goetz
# Date: Winter 2019
####################################################
# Statistical learning for classification
# Mapping the predictions in feature space
# using simple models with only slope and distroad
# Case study: landslides in Ecuador
####################################################

# This is where my raster data from Ecuador is located:
setwd("D:/ecuador/data")

library(ROCR)
library(gam)
library(rpart)
library(randomForest)

# Load the saved training/test data:
(load("landslides.Rd"))

# A function for plotting ROC curves and
# calculating the area under the ROC curve (AUROC)
# using the ROCR package:
auroc <- function(pred, obs, plot=FALSE) {
  stopifnot(is.logical(obs) | is.factor(obs))
  stopifnot(is.numeric(pred))
  stopifnot(length(pred) == length(obs))
  if (is.factor(obs)) stopifnot(nlevels(obs)==2)
  require(ROCR)
  predobj <- prediction( pred, obs )
  if (plot) plot(performance(predobj, "tpr", "fpr"))
  auroc <- performance( predobj, measure="auc" )@y.values[[1]]
  return(auroc)
}

# A function for mapping out the classifier 
# in (a 2D subspace of the) feature space:
predict2d <- function(fit, xs, ys, xname, yname, 
                      predfun, trafo,
                      control.predict = list(), 
                      predict.column,
                      return.matrix = FALSE,
                      newdata)
{
  # Convert additional predictors to data.frame:
  newdata <- sapply(newdata, rep, times = length(xs) * length(ys))
  nd2 <- data.frame(xs = rep(xs, times = length(ys)),
                    ys = rep(ys, each = length(xs)))
  colnames(nd2) <- c(xname, yname)
  newdata <- cbind(newdata, nd2)
  if (!missing(trafo))
    newdata <- trafo(newdata)
  args = list(object = fit, newdata = newdata)
  args = c(args, control.predict)
  if (missing(predfun)) {
    pred = do.call(stats::predict, args)
  } else 
    pred = do.call(predfun, args)
  if (!missing(predict.column)) 
    pred = pred[, predict.column]
  if (return.matrix) {
    return(invisible(matrix(pred, byrow = TRUE, ncol = length(xs), nrow = length(ys))))
  } else {
    newdata$prediction <- pred
    return(invisible(newdata))
  }
}

mypredict2d <- function(fit,
                        xs = seq(0,60,length=30), xname = "slope",
                        ys = seq(0,300,length=50), yname = "distroad",
                        data = d,
                        newdata = list(plancurv = 0, profcurv = 0, cslope = 20,
                                       log.carea = 3, distdeforest = 300),
                        plot = TRUE, levels = seq(0,1,by=0.1),
                        col = "black", #cm.colors(n=length(levels)),
                        ...)
{
  nd <- predict2d(fit = fit, xs = xs, ys = ys,
                  xname = xname, yname = yname,
                  newdata = newdata, return.matrix = FALSE, ...)
  if (plot) {
    z <- matrix(nd$prediction,nrow=length(xs),ncol=length(ys))
    contour(x = xs, y = ys, z = z,
            levels = levels, xlab = xname, ylab = yname,
            col = col)
    contour(x = xs, y = ys, z = z, 
            levels = 0.5, lwd = 2, add = TRUE)
    data <- data[sample(1:nrow(data)),]
    points(data[,xname], data[,yname], cex = 0.8, pch = 19,
           col = c("blue","red")[(data$slides89=="TRUE")+1])
  }
  return(invisible(nd))
}

# Only slope and distroad:
my.trafo.simple <- function(x) {
  x$distroad[ x$distroad > 300 ] = 300
  x$slope = x$slope * 180 / pi
  return(x)
}

d <- my.trafo.simple(d)



##############################################
# Generalized Linear Model
##############################################

fit <- glm(slides89 ~ slope + distroad, data = d, family = binomial)
summary(fit)
fit.glm <- fit
mypredict2d(fit.glm, control.predict = list(type="response"))

pred = predict(fit, newdata = d, type = "response")
auroc(pred, d$slides89=="TRUE", plot=FALSE )


library(RSAGA)
multi.local.function(
  in.grids = c("slope", "distroad"),
  out.varnames = "glmpred_simple",
  fun = grid.predict, control.predict = list(type = "response"),
  fit = fit, trafo = my.trafo.simple, quiet = FALSE )

# Plot prediction map
library(raster)
glm.raster <- raster("glmpred_simple.asc")
plot(glm.raster)

##############################################
# Generalized Additive Model
##############################################

library(gam)
fit <- gam(slides89 ~ s(slope,df=2) + s(distroad,df=2), data = d, family = binomial)
summary(fit)
fit.gam <- fit
par(mfrow=c(1,2),mex=0.7,mar=c(5,5,2,2))
plot(fit, residuals = FALSE, se = TRUE)
par(mfrow=c(1,1))
mypredict2d(fit.gam, control.predict = list(type="response"))

pred <- predict(fit, newdata = d, type = "response")
auroc(pred, d$slides89=="TRUE", plot=FALSE )

multi.local.function(
  in.grids = c("slope", "distroad"),
  out.varnames = "gampred_simple",
  fun = grid.predict, control.predict = list(type = "response"),
  fit = fit, trafo = my.trafo.simple, quiet = FALSE )

# Plot prediction map
library(raster)
gam.raster = raster("gampred_simple.asc")
plot(gam.raster)



##############################################
# Classification Tree
##############################################

library(rpart)
fit <- rpart(slides89 ~ slope + distroad, data = d,
            control = rpart.control(cp=0.003))
par(xpd=TRUE, mfrow=c(1,1))
plot(fit)
text(fit, use.n=TRUE)
par(xpd=FALSE)
fit.rpart <- fit

# Training set AUROC:
pred <- predict(fit, newdata = d, type = "prob")[ , "TRUE" ]
auroc(pred, d$slides89=="TRUE", plot=FALSE )

mypredict2d(fit.rpart, predict.column = "TRUE",
                    control.predict = list(type="prob"),
                    levels = c(0.25,0.75))

par(mfrow=c(1,3))
fit <- rpart(slides89 ~ slope + distroad, data = d,
            control = rpart.control(cp=0))
mypredict2d(fit, predict.column = "TRUE",
            control.predict = list(type="prob"),
            levels = c(0.25,0.75))
fit <- rpart(slides89 ~ slope + distroad, data = d,
            control = rpart.control(cp=0.003))
mypredict2d(fit, predict.column = "TRUE",
            control.predict = list(type="prob"),
            levels = c(0.25,0.75))
fit <- rpart(slides89 ~ slope + distroad, data = d,
            control = rpart.control(cp=0.006))
mypredict2d(fit, predict.column = "TRUE",
            control.predict = list(type="prob"),
            levels = c(0.25,0.75))
par(mfrow=c(1,1))


multi.local.function(
  in.grids = c("slope", "distroad"),
  out.varnames = "ctpred_simple",
  fun = grid.predict, control.predict = list(type = "prob"),
  fit = fit.rpart, trafo = my.trafo.simple, predict.column = "TRUE",
  quiet = FALSE )

ct.raster = raster("ctpred_simple.asc")
plot(ct.raster)


##############################################
# Random Forest
##############################################

library(randomForest)
# using ntree=5000 instead of 500 to reduce 
# random variability, get smoother predictions:
fit <- randomForest(slides89 ~ slope + distroad, data = d, 
                   ntree = 5000, importance = FALSE)
fit.rf <- fit
mypredict2d(fit.rf, predict.column = "TRUE",
                    control.predict = list(type="prob"),
                    levels = c(0,0.25,0.5,0.75,1))

# Training set AUROC:
pred <- predict(fit, newdata = d, type = "prob")[ , "TRUE" ]
auroc(pred, d$slides89=="TRUE", plot=FALSE )


multi.local.function(
  in.grids = c("slope", "distroad"),
  out.varnames = paste("rfpred_simple", sep=""),
  fun = grid.predict, control.predict = list(type = "prob"),
  fit = fit, trafo = my.trafo.simple, predict.column = "TRUE",
  quiet = FALSE )

rf.raster = raster("rfpred_simple.asc")
plot(rf.raster)




##############################################
# Support Vector Machine
##############################################

library(e1071)
# Fitting SVM with "arbitrary" hyperparameter
# values, which is NOT A GOOD IDEA:
fit <- svm(slides89 ~ slope + distroad, data = d, 
          type = "C-classification", kernel = "radial",
          probability = TRUE,
          cost = 1, gamma = 1)
fit.svm <- fit

mypredictsvm <- function(object, newdata) {
  pred <- predict(object, newdata, probability = TRUE)
  pred <- attr(pred, "probabilities")[,"TRUE"]
  return(pred)
}

mypredict2d(fit.svm, predfun = mypredictsvm)

# Training set AUROC:
pred <- mypredictsvm(fit, newdata = d)
auroc(pred, d$slides89=="TRUE", plot=FALSE )


multi.local.function(
  in.grids = c("slope", "distroad"),
  out.varnames = paste("svmpred_simple", sep=""),
  fun = grid.predict, predfun = mypredictsvm, control.predict = list(),
  fit = fit.svm, trafo = my.trafo.simple,
  quiet = FALSE )

svm.raster = raster("svmpred_simple.asc")
plot(svm.raster)

# Compare several hyperparameters:
fit.svm.01.01 <- svm(slides89 ~ slope + distroad, data = d, 
                  type = "C-classification", kernel = "radial",
                  probability = TRUE,
                  cost = 0.1, gamma = 0.1)
fit.svm.01.1 <- svm(slides89 ~ slope + distroad, data = d, 
                  type = "C-classification", kernel = "radial",
                  probability = TRUE,
                  cost = 0.1, gamma = 1)
fit.svm.1.01 <- svm(slides89 ~ slope + distroad, data = d, 
                  type = "C-classification", kernel = "radial",
                  probability = TRUE,
                  cost = 1, gamma = 0.1)
fit.svm.1.1 <- svm(slides89 ~ slope + distroad, data = d, 
          type = "C-classification", kernel = "radial",
          probability = TRUE,
          cost = 1, gamma = 1)
fit.svm.10.1 <- svm(slides89 ~ slope + distroad, data = d, 
                 type = "C-classification", kernel = "radial",
                 probability = TRUE,
                 cost = 10, gamma = 1)
fit.svm.1.10 <- svm(slides89 ~ slope + distroad, data = d, 
                 type = "C-classification", kernel = "radial",
                 probability = TRUE,
                 cost = 1, gamma = 10)
fit.svm.10.10 <- svm(slides89 ~ slope + distroad, data = d, 
                 type = "C-classification", kernel = "radial",
                 probability = TRUE,
                 cost = 10, gamma = 10)
par(mfrow=c(2,2))
mypredict2d(fit.svm.1.1, predfun = mypredictsvm)
mypredict2d(fit.svm.10.1, predfun = mypredictsvm)
mypredict2d(fit.svm.1.10, predfun = mypredictsvm)
mypredict2d(fit.svm.10.10, predfun = mypredictsvm)

par(mfrow=c(2,2))
mypredict2d(fit.svm.01.01, predfun = mypredictsvm)
mypredict2d(fit.svm.1.01, predfun = mypredictsvm)
mypredict2d(fit.svm.01.1, predfun = mypredictsvm)
mypredict2d(fit.svm.1.1, predfun = mypredictsvm)


##############################################
# Putting it all together
##############################################

par(mfrow=c(2,3))
mypredict2d(fit.glm, control.predict = list(type="response"))
mypredict2d(fit.svm.01.01, predfun = mypredictsvm)
mypredict2d(fit.rpart, predict.column = "TRUE",
            control.predict = list(type="prob"),
            levels = c(0.25,0.75))
mypredict2d(fit.gam, control.predict = list(type="response"))
mypredict2d(fit.svm.10.10, predfun = mypredictsvm,
            levels = c(0,0.25,0.5,0.75,1))
mypredict2d(fit.rf, predict.column = "TRUE",
            control.predict = list(type="prob"),
            levels = c(0,0.25,0.5,0.75,1))

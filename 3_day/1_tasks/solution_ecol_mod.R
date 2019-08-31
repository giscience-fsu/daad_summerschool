# Filename: solution_ecol_mod.R (08/21/2019)

# TO DO: Ecological modeling

# Author(s): Jannes Muenchow

#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************

# 1. ATTACH PACKAGES AND DATA
# 2. ORDINATION
# 3. ENVIRONMENTAL PREDICTOR RASTER STACK
# 4. MODELING
# 5. SPATIAL CV USING mlr

#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("sf")
library("raster")
library("RQGIS3")
library("vegan")
library("mlr")
library("parallelMap")
library("lattice")
library("latticeExtra")
library("grid")
library("dplyr")
library("vegan")

#**********************************************************
# 2 ORDINATION---------------------------------------------
#**********************************************************

data("study_area", "random_points", "comm", "dem", "ndvi", package = "RQGIS3")
pa = decostand(comm, "pa")
# delete empty plots, i.e. sites without a single species
pa = pa[rowSums(pa) != 0, ]
comm = comm[rowSums(comm) != 0, ]

# Run a PCA
pca_1 = rda(pa)
# first axis explains roughly 20%
head(eigenvals(pca_1) / sum(eigenvals(pca_1)))
biplot(pca_1)

# Run a DCA
dca_1 = decorana(pa, iweigh = TRUE)
# first axis explains 45%
dca_1$evals / sum(dca_1$evals)
# plot(scores(dca_1, display = "sites"))
sc = scores(dca_1, choices = 1:2, display = "sites") %>%
  as.data.frame
sc$id = row.names(sc) %>% as.numeric
plot(scores(dca_1, display = "sites"))
# save your result
saveRDS(pca_1, file = "images/pca_1.rds")
saveRDS(dca_1, file = "images/dca_1.rds")

#**********************************************************
# 3 ENVIRONMENTAL PREDICTOR RASTER STACK-------------------
#**********************************************************

# create environmental-predictor raster stack
ep = run_qgis(alg = "saga:sagawetnessindex",
              DEM = dem,
              SLOPE_TYPE = 1,
              SLOPE = tempfile(fileext = ".sdat"),
              AREA = tempfile(fileext = ".sdat"),
              AREA_MOD = tempfile(fileext = ".sdat"),
              TWI = tempfile(fileext = ".sdat"),
              load_output = TRUE,
              show_output_paths = FALSE)
ep = ep[c("AREA", "SLOPE", "TWI")]
ep = c(dem, ndvi, ep) %>%
  stack()
names(ep) = c("dem", "ndvi", "carea", "cslope", "twi")
ep$carea = log10(ep$carea)
# computing the curvatures
# get_usage("grass7:r.slope.aspect")
curvs = run_qgis("grass7:r.slope.aspect",
                 elevation = dem,
                 pcurvature = file.path(tempdir(), "pcurvature.tif"),
                 tcurvature = file.path(tempdir(), "tcurvature.tif"),
                 load_output = TRUE,
                 show_output_paths = FALSE)
# adding curvatures to ep
ep = addLayer(ep, curvs$pcurvature, curvs$tcurvature) %>%
  # brick saves raster in memory as opposed to stack
  brick
random_points[, names(ep)] = raster::extract(ep, as(random_points, "Spatial"))
# add scores of the first axis, i.e., create a response-predictor matrix
rp = inner_join(random_points, dplyr::select(sc, id, DCA1), by = "id")

# save your result
saveRDS(ep, "images/ep.rds")
saveRDS(rp, file = "images/rp.rds")

#**********************************************************
# 4 MODELING-----------------------------------------------
#**********************************************************

# 4.1 Data exploration=====================================
#**********************************************************

preds = dplyr::select(rp, -one_of("DCA1", "id", "spri")) %>%
  st_drop_geometry %>%
  names
saveRDS(preds, file = "images/preds.rds")
# little data exploration
tmp = select(rp, DCA1, preds) %>%
  st_drop_geometry %>%
  reshape2::melt(id.vars = "DCA1")
# plot the response variable against each predictor
xyplot(DCA1 ~ value | variable, data = tmp,
       scales = list(x = "free"),
       layout = c(3, 3),
       panel = function(x, y) {
         panel.points(x, y, col = "lightblue", pch = 16)
         panel.loess(x, y, col = "black")
       })
# check collinearity among predictors
pairs(rp[, preds] %>% st_drop_geometry())
# possibly delete collinear variables
preds = preds[!preds %in% c("carea", "tcurvature")]
# a more formal approach would be to use the variance inflation factor

# 4.2 Modeling=============================================
#**********************************************************

# model formula
form = preds %>%
  # for a GAM
  # paste0("s(", ., ")") %>%
  paste(collapse = "+") %>%
  paste("DCA1 ~", .) %>%
  as.formula
lm_1 = lm(form, data = rp)

saveRDS(form, "images/form.rds")
saveRDS(lm_1, "images/lm_1.rds")

summary(lm_1)
plot(lm_1)  # mmh, looking at these plots, we should refrain from any inferential statistics, i.e., we should be no means interpret our coefficients and p-values based on this model

# but here we only want to spatially predict our scores

# 4.3 Spatial prediction===================================
#**********************************************************
pred = raster::predict(ep, lm_1) %>%
  # restrict the prediction to the study area
  mask(., study_area) %>%
  trim

# prediction map
# create a color palette
blue = rgb(0, 0, 146, maxColorValue = 255)
lightblue = rgb(0, 129, 255, maxColorValue = 255)
turquoise = rgb(0, 233, 255, maxColorValue = 255)
green = rgb(142, 255, 11, maxColorValue = 255)
yellow = rgb(245, 255, 8, maxColorValue = 255)
orange = rgb(255, 173, 0, maxColorValue = 255)
lightred = rgb(255, 67, 0, maxColorValue = 255)
red = rgb(170, 0, 0, maxColorValue = 255)
pal = colorRampPalette(c(blue, lightblue, turquoise, green, yellow,
                         orange, lightred, red))

# create a hillshade
hs = hillShade(terrain(dem), terrain(dem, "aspect")) %>%
  mask(., study_area)
spplot(extend(pred, 2), col.regions = pal(50), alpha.regions = 0.7,
       scales = list(draw = TRUE,
                     tck = c(1, 0),
                     cex = 0.8),
       colorkey = list(space = "right", width = 0.5, height = 0.5,
                       axis.line = list(col = "black")),
       sp.layout = list(
         # list("sp.points", as(random_points, "Spatial"), pch = 16,
         #      col = "black", cex = 0.8, first = FALSE),
         list("sp.polygons", as(study_area, "Spatial"),
              col = "black", first = FALSE, lwd = 3)
       )
) +
  latticeExtra::as.layer(spplot(hs, col.regions = gray(0:100 / 100)),
                         under = TRUE)

#**********************************************************
# 5 SPATIAL CROSS-VALIDATION (lm)--------------------------
#**********************************************************

# put coordinates into a separate dataframe
coords = st_coordinates(rp) %>% as.data.frame
# create a task
rp = st_drop_geometry(rp)
rp = select(rp, DCA1, preds)
task = makeRegrTask(data = rp, target = "DCA1", coordinates = coords)
# define the learner
# run listLearners to find out which models could be thrown at our task
# lrns = listLearners(task, warn.missing.packages = FALSE)
# dplyr::select(lrns, class, name, short.name, package)
listLearners(task)

# define a learner
lrn = makeLearner(cl = "regr.lm", predict.type = "response")
# simple lm of the stats package
getLearnerPackages(lrn)
helpLearner(lrn)
# so the model being fitted is simply a lm
getLearnerModel(train(lrn, task))
# same as our lm
lm_1

# performance level
perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)
perf_lm = mlr::resample(learner = lrn,
                        task = task,
                        resampling = perf_level,
                        # using R squared as performance measure
                        measures = mlr::rmse)
# find out how much the predicted scores deviate from the true value in %
perf_lm$aggr / diff(range(rp$DCA1)) * 100
# 21%, mediocre result
# so, on average the fitted DCA score is off by 21% compared to the true value

# save your result, e.g.:
saveRDS(perf_lm, "images/perf_lm.rds")
boxplot(perf_lm$measures.test$rmse, ylab = "RMSE", main = "RMSE from 500 models",
        col = "mistyrose2")

#**********************************************************
# 6 SPATIAL CROSS-VALIDATION (random forest)---------------
#**********************************************************

# create task
task = makeRegrTask(data = rp, target = "DCA1", coordinates = coords)
# learner
lrn_rf = makeLearner(cl = "regr.ranger", predict.type = "response")
# performance estimation level with 5 spatial partitions and 100 repetitions
perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)
# five spatially disjoint partitions in the tune level (one repetition)
tune_level = makeResampleDesc(method = "SpCV", iters = 5)
# random search with 50 iterations
ctrl = makeTuneControlRandom(maxit = 50)
# specifying the search space
ps = makeParamSet(
  makeIntegerParam("mtry", lower = 1, upper = ncol(rp) - 1),
  makeNumericParam("sample.fraction", lower = 0.2, upper = 0.9),
  makeIntegerParam("min.node.size", lower = 1, upper = 10)
)
# wrap it all up
wrapped_lrn_rf = makeTuneWrapper(learner = lrn_rf,
                                 # inner loop (tunning level)
                                 resampling = tune_level,
                                 # hyperparameter seach space
                                 par.set = ps,
                                 # random search
                                 control = ctrl,
                                 show.info = TRUE,
                                 # performance measure
                                 measures = mlr::rmse)
# make sure that the modeling goes on even if one model fails
configureMlr(on.learner.error = "warn", on.error.dump = TRUE)

# initialize parallelization
if (Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
  parallelStart(mode = "multicore",
                # parallelize the hyperparameter tuning level
                level = "mlr.tuneParams",
                # just use half of the available cores
                cpus = round(parallel::detectCores() / 1.25),
                mc.set.seed = TRUE)
}

if (Sys.info()["sysname"] == "Windows") {
  parallelStartSocket(level = "mlr.tuneParams",
                      cpus =  round(parallel::detectCores() / 2))
}

# run the spatial cross-validation
set.seed(12345)
perf_rf = mlr::resample(learner = wrapped_lrn_rf,
                          task = task,
                          resampling = perf_level,
                          extract = getTuneResult,
                          measures = mlr::rmse)
# stop parallelization
parallelStop()
# save your result, e.g.:
saveRDS(perf_rf, file = "images/perf_rf.rds")

perf_rf$aggr / diff(range(rp$DCA1)) * 100
# 20%, slightly better than perf_lm
perf_lm$aggr / diff(range(rp$DCA1)) * 100

boxplot(perf_rf$measures.test$rmse, perf_lm$measures.test$rmse,
        col = c("lightblue2", "mistyrose2"),
        names = c("random forest", "lm"), ylab = "RMSE")


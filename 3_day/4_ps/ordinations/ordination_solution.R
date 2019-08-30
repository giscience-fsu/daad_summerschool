# Filename: ordination_solution (2019-08-28)

# TO DO: PCA, DCA, NMDS

# Author(s): Jannes Muenchow

#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************

# 1. ATTACH PACKAGES AND DATA
# 2. ORDINATIONS
# 3. MODELING ORDINATION SCORES

#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("vegan")
library("reshape2")
library("lattice")
library("mgcv")
library("dplyr")

# attach data
data("varechem")
data("varespec")

#**********************************************************
# 2 ORDINATIONS--------------------------------------------
#**********************************************************

# Principal Component Analysis
pca_1 = rda(varespec)
biplot(pca_1)
cumsum(eigenvals(pca_1) / sum(eigenvals(pca_1)))  # 79% explained by first 2 axes

# Detrended Correspondence Analysis
dca_1 = decorana(varespec, iweigh = 1)
plot(dca_1)
# cumulative proportion
cumsum(dca_1$evals / sum(dca_1$evals))[1:3]  # 61% explained by first 2 axes

# NMDS
nmds_1 = metaMDS(varespec, k = 2, try = 500)
nmds_1$stress  # 18
plot(nmds_1)

# ok, PCA seems to be a good choice for the varespec dataset

#**********************************************************
# 3 MODELING ORDINATION SCORES-----------------------------
#**********************************************************

sc = scores(pca_1, choices = 1, display = "sites")
# make sure that varespec and varechem have the same plot order
rownames(varechem) == rownames(varespec)  # perfect
# construct responce predictor matrix
d = data.frame(sc, varechem)
# have a look at the relationships
tmp = reshape2::melt(d, id.var = "PC1")
xyplot(PC1 ~ value | variable, data = tmp,
       scale = list(y = "free", x = "free"),
       panel = function(x, y) {
         panel.points(x, y, pch = 16, col = "lightblue")
         panel.loess(x, y, col = "black")
       })
# have a look at highly correlated predictors (don't use them)
biplot(rda(select(d, -PC1), scale = TRUE))

gam_1 = gam(PC1 ~ s(Baresoil) + s(N), data = d)  # pH and Humdepth strongly correlated with Baresoil
summary(gam_1)  # 70% explained deviance
plot(gam_1)
# very quick model inspection
plot(fitted(gam_1) ~ residuals(gam_1))  # looks okayish, so we probably could use our model also for inference not only for prediction
# make the prediction
fit_vals = fitted(gam_1)
plot(d$PC1 ~ fit_vals)


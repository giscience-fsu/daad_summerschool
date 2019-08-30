## x should have
## x$x:   longitude
## x$y:   latitude
## x$...: things to map
reraster <- function(x, categorical = TRUE) {
  robinson <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +over"
  x <- rasterFromXYZ(x, crs = CRS("+init=epsg:4326"))
  if(categorical) x <- projectRaster(x, crs = CRS(robinson), method = "ngb") # ngb is for categorical variables...
  if(!categorical) x<- projectRaster(x, crs = CRS(robinson), method = "bilinear") # ngb is for categorical variables...
  x
}
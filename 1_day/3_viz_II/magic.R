suppressPackageStartupMessages({
  library("sf")
  library("RColorBrewer")
  library("mapview")
  library("leaflet")
  library("leafpop")
  library("leafem")
})

#+ setup, echo=FALSE
knitr::opts_chunk$set(fig.width=8.83, warning=FALSE, message=FALSE, eval=FALSE)

#' # Some magic
#'
#' ### 0. mapshot
#'
#' `mapview::mapshot()` enables us to save a local copy of our interactive map
#' and let's us export it as a static .png file.
m = mapview(breweries)
mapshot(m,
        url = "/home/timpanse/Desktop/mymap.html",
        file = "/home/timpanse/Desktop/mymap.png",
        remove_controls = c("zoomControl", "layersControl", "homeButton"))

#'
#' Some not-so-well known features that may come in handy one day.
#'
#' ### 1. Mouse location to clipboard
#'
#' Simply press and hold CTRL and click somewhere on the map. This will copy
#' the x/y/z location of the clicked location to the clipboard. First though,
#' here's a way to redirect any leaflet/mapview map straight to the browser
options(viewer = NULL)

mapview(franconia)
clip2sfc('<CTRL+v>')

#' reset viewer back to RStudio viewer
options(viewer = rstudioapi::viewer)

#' ### 2. Extent to clipboard
#'
#' `leafem::addCopyExtent()` let's us copy the extent of the current view
#' to the clipboard:
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCopyExtent(event.code = "KeyE") %>%
  addMouseCoordinates()

#' now navigate and zoom the map to some place and then click "e" on your keyborad.
#' In the following line replace '<CTRL+v>' with whatever is pasted when you
#' hit CTRL+v.
jsonlite::fromJSON('<CTRL+v>', simplifyDataFrame = TRUE)

#' ### 3. File access
#'
#' `leafem::addLocalFile()` enables adding local files to a map directly from disk,
#' i.e. without reading into R first. This is available as a method for `mapview()`
#' so just point `mapview()` to a file on disk.
destfile = tempfile(fileext = ".gpkg")

st_write(st_as_sf(gadmCHE), dsn = destfile)

leaflet() %>%
  addTiles() %>%
  leafem:::addLocalFile(destfile)

#' ### 4. Tile folders
#'
#' `leafem::addTileFolder()` enables adding of local tile pyramids created with
#' e.g. `gdal2tiles.py` to a map. This is again available as a method for `mapview()`.
mapview("/home/timpanse/software/tiles/bicycle-tiles/")

#' ### 5. add features
#'
#' `leafem::addFeatures()` is a type-agnostic version of the `leaflet::add*` functions.
#' For **sf** and **sp** data types it will figure out which `leaflet::add*` function
#' to call and add the respective type to the map.
leaflet() %>%
  addTiles() %>%
  addFeatures(data = franconia) %>%
  addFeatures(data = breweries, color = "purple")

#' ### 6. popups
#'
#' Package **leafpop** has a few functions to help with popup generation for leaflet maps.
#'
#' #### 6.1. popupTable
#'
#' This is what mapview uses for popup generation
leaflet() %>%
  addTiles() %>%
  addFeatures(franconia, popup = popupTable(franconia))

#' We can also subset the data to be shown by using "zcol"
leaflet() %>%
  addTiles() %>%
  addFeatures(franconia, popup = popupTable(franconia,
                                            zcol = c(1, 2, 3)))

#' #### 6.2. image popups
#'
#' `addPopupImages let's us add images to popups for certain map layers.
pnt = st_as_sf(data.frame(x = 174.764474, y = -36.877245),
               coords = c("x", "y"),
               crs = 4326)

img = "http://bit.ly/1TVwRiR"

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = pnt, group = "pnt") %>%
  addPopupImages(img, group = "pnt")

#' #### 6.3 graph popups
#'
#' Same as with images, we can add e.g. ggplot2 or lattice graphs to popups
library("lattice")

pt = data.frame(x = 174.764474, y = -36.877245)
pt = st_as_sf(pt, coords = c("x", "y"), crs = 4326)

p2 = levelplot(t(volcano), col.regions = terrain.colors(100))

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = pt, group = "pt") %>%
  addPopupGraphs(list(p2), group = "pt", width = 300, height = 400)

#' #### 6.4. iframes
#'
#' Currently not (but soon to be) exported is `leafpop:::addPopupIframes()` that
#' let's us embed in popups whatever we want basically, as long as the source is
#' pointing to a valid URL.
vid = "https://www.youtube.com/embed/iApz08Bh53w?autoplay=1"

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = pt, group = "pt") %>%
  leafpop:::addPopupIframes(source = vid, group = "pt")

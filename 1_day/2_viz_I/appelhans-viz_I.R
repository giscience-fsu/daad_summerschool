suppressPackageStartupMessages({
  library("sf")
  library("RColorBrewer")
  library("mapview")
  library("leaflet")
  library("plainview")
  library("slideview")
  library("cubeview")
})

#+ setup, echo=FALSE
knitr::opts_chunk$set(fig.width=8.83, warning=FALSE, message=FALSE, eval=FALSE)

#' # Some notes on using color in maps
#'
#' Usually, I would give a little lecture on the importance of color in mapping
#' but since this years User!2019 conference in Toulouse I can simply refer to:
#'
#' https://user2019.r-project.org/static/pres/t253801.pdf
#'
#' which are the slides of a talk by Achim Zeileis entitled:
#'
#' "colorspace: A Toolbox for Manipulating and Assessing Color Palettes"
#'
#' A correspoding video should be posted at the RConsortium
#' [youtube channel](https://www.youtube.com/channel/UC_R5smHVXRYGhZYDJsnXTwg/)
#' in the not too distant future.
#'
#' In essence, there are many ways to represent data with colors
#' and we need to think about the nature of the data before selecting colors for
#' our visualisations. As a rule of thumb, hcl based colors are much better than
#' RGB colors as they perceptually map much better to data.
#'
#' The colorspace web page at:
#'
#' http://colorspace.r-forge.r-project.org/index.html
#'
#' provides a wealth of additional in depth information on color spaces and the
#' HCLwizard tool at:
#'
#' http://hclwizard.org:64230/hclwizard/
#'
#' is a nice online tool to create and investigate lots of color palettes.
#'
#' Here's acomparison of some palettes, mapping `1:100` to several color scales:
#'
#' ![](figures/color_grid.png)
#'
#' which would translate as follows to a real-world example:
#'
#' ### RGB-based rainbow color scheme:
#'
#' ![](figures/wrld_sst_tim.colors.png)
#'
#' ### HCL-based color scheme:
#'
#' ![](figures/wrld_sst_inferno.png)
#'
#' -----
#'
#' # Improved rainbow color map
#'
#' Just last week Google published a new rainbow color map that is supposed to
#' be perceptually uniform and ahere to color deficiency while preserving as much
#' detail as possible.
#'
#' <blockquote class="twitter-tweet"><p lang="en" dir="ltr">Turbo, An Improved Rainbow Colormap for Visualization : <a href="https://t.co/E7nJGLclCZ">https://t.co/E7nJGLclCZ</a><a href="https://twitter.com/hashtag/rstats?src=hash&amp;ref_src=twsrc%5Etfw">#rstats</a> implementation : <a href="https://t.co/mMcfuCW0yI">https://t.co/mMcfuCW0yI</a> <a href="https://t.co/wqVtHDw7BM">pic.twitter.com/wqVtHDw7BM</a></p>&mdash; Julien Barnier (@lapply) <a href="https://twitter.com/lapply/status/1164445786568036352?ref_src=twsrc%5Etfw">August 22, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
#'
#' -----
#'
#'
#' # 1. leaflet
#'
#' ### A very basic call
#+ eval=TRUE
leaflet() %>%
  addTiles()

#' ### Setting the view

leaflet() %>%
  addTiles() %>%
  setView(lng = 11.58, lat = 50.92, zoom = 14)

#' ### Using other basemaps & layers control
#+ eval=TRUE
basemaps = c("CartoDB.DarkMatter", "CartoDB.Positron")
#'
leaflet() %>%
  addProviderTiles(basemaps[1], group = basemaps[1]) %>%
  addProviderTiles(basemaps[2], group = basemaps[2]) %>%
  setView(lng = 11.58, lat = 50.92, zoom = 14) %>%
  addLayersControl(baseGroups = basemaps)

#' ### Adding data

leaflet() %>%
  addProviderTiles(basemaps[1], group = basemaps[1]) %>%
  addProviderTiles(basemaps[2], group = basemaps[2]) %>%
  addPolygons(
    data = mapview::franconia,
    group = "franconia"
  ) %>%
  addPolylines(data = atlStorms2005) %>%
  addLayersControl(
    baseGroups = basemaps,
    overlayGroups = "franconia",
    position = "topleft"
  )

#' ### Adding popups & lables
#+ eval=TRUE
leaflet() %>%
  addProviderTiles(basemaps[1], group = basemaps[1]) %>%
  addProviderTiles(basemaps[2], group = basemaps[2]) %>%
  addPolygons(data = mapview::franconia,
              group = "franconia",
              popup = ~district,
              label = ~NUTS_ID) %>%
  addPolylines(data = atlStorms2005,
               label = ~Name) %>%
  addLayersControl(baseGroups = basemaps,
                   overlayGroups = "franconia",
                   position = "topleft")

#'
#' -----
#'
#' # 2. mapview
#'
#' `mapview` is based on leaflet and wraps much of its functionality into a
#' data-driven API that is designed to be quick and comprehensive, yet customisable.
#'
#' ### Basic usage
#+ eval=TRUE
mapview()

#' ### with some data
#+ eval=TRUE
mapview(franconia)
m = mapview(breweries)

#' ### mapview structure
#+ eval=TRUE
str(m, 3)
m@object[[1]]
class(m@map)

#' ### mapview methods
#+ eval=TRUE
showMethods("mapView")
#'

#' ### no or specific CRS? no problem
#+ eval=TRUE
brew = st_set_crs(breweries, NA)
mapview(brew)

brew = st_transform(breweries, 3035)
mapview(brew)
mapview(brew, native.crs = TRUE)

#' ### styling options & legends
#+ eval=TRUE
mapview(franconia, color = "grey95", col.regions = "white")

#'
mapview(breweries, zcol = "founded", layer.name = "Year of <br> foundation")
mapview(breweries, zcol = "founded", at = seq(1300, 2200, 200))
mapview(franconia, zcol = "district", legend = FALSE, label = NULL)

#'
clrs = colorRampPalette(brewer.pal(3, "Set1"))
mapview(franconia, zcol = "district", col.regions = clrs, alpha.regions = 1)

cols = c("orange", "purple", "forestgreen")
mapview(franconia, zcol = "district", col.regions = cols, alpha.regions = 1)

#' ### multiple layers
#+ eval=TRUE
mapview(breweries) + franconia

#'
mapview(breweries, col.regions = "red") +
  mapview(franconia, col.regions = "grey") +
  trails
mapview(list(breweries, franconia, trails))

mapview(list(breweries, franconia),
        zcol = list("founded", "district"),
        legend = list(FALSE, TRUE),
        homebutton = list(TRUE, FALSE)) +
  trails

#' ### burst
mapview(franconia, burst = TRUE)
mapview(franconia, burst = TRUE, hide = TRUE)

#+ eval=TRUE
mapview(franconia, zcol = "district", burst = TRUE)

#' ### view extents
#+ eval=TRUE
viewExtent(breweries)

#' which is equal to
mapview(st_bbox(breweries))
viewExtent(breweries) + mapview(breweries, zcol = "founded")
viewExtent(poppendorf) + poppendorf[[5]]

#' ### basemaps
mapview(gadmCHE, map.types = c("Stamen.Toner", "NASAGIBS.ViirsEarthAtNight2012"))

# see https://leaflet-extras.github.io/leaflet-providers/preview/ for more options

#' ### changing options globally
#+ eval=TRUE
mapviewOptions(basemaps = c("CartoDB.DarkMatter", "Esri.OceanBasemap"),
               raster.palette = colorRampPalette(rev(brewer.pal(9, "Greys"))),
               vector.palette = colorRampPalette(brewer.pal(9, "YlGnBu")),
               na.color = "magenta",
               layers.control.pos = "topright")
#'
mapview(breweries, zcol = "founded")

#+ eval=TRUE
mapview(poppendorf[[5]])

#+ eval=TRUE
mapviewOptions()
mapviewOptions(default = TRUE)

mapview(poppendorf[[4]])

#' ### Raster specific functions
#'
#' Apart from the standard `mapview()` call for raster layers and stacks, we
#' have some special functionality to aid raster visualisation.
#'
mapview(poppendorf[[4]], query.type = "click")

#' ### viewRGB
#'
#' to view RGB composites of any combination of raster layers
#'
#+ eval=TRUE
viewRGB(poppendorf)

#'
raster::nlayers(poppendorf)
viewRGB(poppendorf, r = 4, g = 3, b = 2)
viewRGB(poppendorf, 5, 4, 3)
viewRGB(poppendorf, 5, 4, 3, quantiles = c(0.25, 0.75)) # black/white outside this range

#' ### plainview
#+ eval=TRUE
mapview(poppendorf[[4]], native.crs = TRUE)

#' which is the same as
library("plainview")

plainview(poppendorf[[4]])
plainview(poppendorf[[1]], at = seq(8000, 15000, 500))

#' also enables RGB views of raster stacks
#+ eval=TRUE
plainview(poppendorf, 4, 3, 2)

#'
plainview(poppendorf, 5, 4, 3, quantiles = c(0.5, 1))

#' ### slideview
library("slideview")

slideview(poppendorf[[1]], poppendorf[[5]])
slideview(poppendorf[[1]], poppendorf[[5]], legend = FALSE)

#' ### cubeview
library("raster")
library("cubeview")

kili_data = system.file("extdata", "kiliNDVI.tif", package = "mapview")
kiliNDVI = stack(kili_data)

cubeView(kiliNDVI)

clr = viridisLite::viridis
cubeView(kiliNDVI, at = seq(-0.15, 1.2, 0.2), col.regions = clr)

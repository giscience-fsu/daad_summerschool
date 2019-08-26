suppressPackageStartupMessages({
  library("sf")
  library("leaflet")
  library("leafgl")
  library("colourvalues")
})

#+ setup, echo=FALSE
knitr::opts_chunk$set(fig.width=8.83, warning=FALSE, message=FALSE, eval=FALSE)

#+ echo=FALSE
# roads = st_read("/media/timpanse/d8346522-ef28-4d63-9bf3-19fec6e13aab/bu_lenovo/software/testing/mapview/switzerland/roads.shp")
#
# nms = names(sort(table(roads$type)))
# nms = nms[1:(length(nms)-2)]
# roads = roads[roads$type %in% nms, ]
#
# st_write(roads, dsn = "/home/timpanse/software/data/large.gpkg", layer = "roads")
#
# buildings = st_read("/media/timpanse/d8346522-ef28-4d63-9bf3-19fec6e13aab/bu_lenovo/software/testing/mapview/switzerland/buildings.shp")
# buildings = buildings[!is.na(buildings$type), ]
#
# st_write(buildings, dsn = "/home/timpanse/software/data/large.gpkg", layer = "buildings")
#
# buildings = st_read("/media/timpanse/d8346522-ef28-4d63-9bf3-19fec6e13aab/bu_lenovo/software/testing/mapview/switzerland/buildings.shp")
# inv = st_is_valid(buildings)
# table(inv)
# buildings[!inv, ] = lwgeom::st_make_valid(buildings[!inv, ])
# building_centroids = st_point_on_surface(buildings)
#
# st_write(building_centroids,
#          dsn = "/home/timpanse/software/data/large.gpkg",
#          layer = "building_centroids")


#' # leafgl
#'
#' `leafgl` is a webgl renderer for leaflet and enables viewing of very large data sets.
#' As such there are some restrictions as compared to the original `leaflet::add*`
#' functions. Most importantly, popups can only refer to one column present in the data.
#' Furthermore, point and line feature weight can not be mapped to a value at present,
#' though I am hopeful that this will be possible in the future.
#'
#' I've prepared a hefty data file to show just how performant things can be with
#' `leafgl`. This file is available at
#'
#' https://drive.google.com/file/d/10IiP50cORXJY9SAqoarMKSWlADel6VBf/view?usp=sharing
#'
#' so download and below adjust the path to read the file and its layers.
#' There are three layers in the data:

#+ eval=TRUE
st_layers("/home/timpanse/software/data/large.gpkg")

#' Let's visualise all of these.
#'
#' ### Polygons
buildings = st_read("/home/timpanse/software/data/large.gpkg", layer = "buildings")

clrs = colour_values_rgb(buildings$type, include_alpha = FALSE) / 255

leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto") %>%
  addGlPolygons(data = buildings,
                color = clrs,
                # popup = "type",
                opacity = 1,
                group = "buildings") %>%
  addLayersControl(baseGroups = c("OSM", "Carto"),
                   overlayGroups = "buildings")

#' ### Points
centroids = st_read("/home/timpanse/software/data/large.gpkg",
                    layer = "building_centroids")

clrs = colour_values_rgb(centroids$type, include_alpha = FALSE) / 255

leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto") %>%
  addGlPoints(data = centroids,
              color = clrs,
              # popup = "type",
              opacity = 1,
              weight = 5,
              group = "centroids") %>%
  addLayersControl(baseGroups = c("OSM", "Carto"),
                   overlayGroups = "centroids")

#' ### Lines
roads = st_read("/home/timpanse/software/data/large.gpkg",
                layer = "roads")

clrs = colour_values_rgb(roads$type, include_alpha = FALSE) / 255

leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto") %>%
  addGlPolylines(data = roads,
                 color = clrs,
                 # popup = "type",
                 opacity = 1,
                 group = "roads") %>%
  addLayersControl(baseGroups = c("OSM", "Carto"),
                   overlayGroups = "roads")

#' ### mapdeck
#'
#' Another package for interactive visualisation of large data sets is
#' [`mapdeck`](https://github.com/SymbolixAU/mapdeck/blob/master/README.md).
#' It requires a token/key to be able to use the Mapbox service it is build on.
#' Just follow the link provided at the site linked above, get your token.
#' Once you have your token, you can use mapdeck:
#'
library("mapdeck")

key = "pk.eyJ1IjoidGltLXNhbGFiaW0iLCJhIjoiY2pvYmJ5cGh6MG1jeTN3bXBob2ZwcjVxcyJ9.xEbtwwdDkpXpehYjjYzwoQ"

mapdeck(token = key) %>%
  add_scatterplot(data = centroids, fill_colour = "type")

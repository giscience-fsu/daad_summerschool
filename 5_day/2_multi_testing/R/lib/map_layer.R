# plots the world contour lines in maps
data(World)

map_layer <- tm_shape(World) + 
  tm_borders() + 
  tm_format("World", earth.boundary = TRUE, frame = FALSE) +
  tm_layout(between.margin = 0.1)

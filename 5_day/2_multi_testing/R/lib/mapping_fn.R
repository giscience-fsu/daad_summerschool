spalette <- c("#0000ff", "#ff0000") # blue and red
hpalette<- c("#e3e3e3", "#ff0000", "#ff6464", '#6464ff',  "#0000ff") # light red, red, white, light blue, blue,  


ref<- function(data, method){
  data <- data %>%
    dplyr::select(x = lon, y = lat, mk_z_stat, method) %>%
    mutate(mode = ifelse(abs(mk_z_stat) <= qnorm(1-ALPHA/2), 1, # not significant
                         ifelse(mk_z_stat > qnorm(1-ALPHA/2) & !data[method], 2, # significant and positive without correction
                                ifelse(mk_z_stat < qnorm(ALPHA/2) & !data[method], 3, # significant and negative without correction
                                       ifelse(mk_z_stat > qnorm(1-ALPHA/2) & data[method], 4, # significant and positive with correction
                                              ifelse(mk_z_stat < qnorm(ALPHA/2) & data[method], 5, NA)))))) # significant and negative with correction
  # data$mode<- factor(data$mode, levels = c('1', '2', '3', '4', '5'))
  data
}

remap<- function(data, method){
  tmp<- data %>% ref(method) %>% reraster 
  my_palette<- c("#ffffff", "#ffd2d2", "#d2d2ff", "#ff0000", "#0000ff") # white, light red, light blue, red, blue
  tmp_map<- tm_shape(tmp) +
    tm_raster("mode", palette = my_palette,
            legend.show = TRUE, auto.palette.mapping = FALSE,
            style = 'fixed', breaks = 0:5+.5,
            colorNA = "#e3e3e3") +
    tm_legend(legend.outside=T, legend.outside.position="bottom") + 
  map_layer
  tmp_map
}

make_maps<- function(data, ...){
  names<- names(data)
  out<- vector(mode = 'list', length = length(names))
  for(i in 1:length(names)){
    out[[i]]<- tm_shape(data) + 
      tm_raster(names[i], legend.show = TRUE, ...,
                legend.is.portrait = FALSE,
                colorNA = "#e3e3e3") +
      tm_legend(legend.outside=T, legend.outside.position="bottom") + 
      map_layer
  }
  names(out)<- names
  return(out)
}

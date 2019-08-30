raster_to_list_fn<- function(raster){
  # This function takes a RasterStack (TODO: other raster objects)
  # and it outputs the format used for perm tests (a named list of matrices)...
  # require(raster)
  nrows<- dim(raster)[1]
  ncols<- dim(raster)[2]
  time_points<- dim(raster)[3]
  
  out<- vector("list", length = time_points)
  for (i in 1:time_points){
    out[[i]]<- as.matrix(raster[[i]])
  }
  names(out)<- as.character(1:time_points)
  
  sel<- !is.na(as.vector(out[[1]]))
  
  if(length(out) > 1){ # when imputing image time series it selects only grid cells with < 10% missing data
    sel<- as.numeric(!is.na(as.vector(out[[1]])))
    
    for(i in 2:time_points){
      sel <- as.numeric(!is.na(as.vector(out[[i]]))) + sel 
      summary(sel)
    }
    sel<- sel > (time_points*.9) # keep only data points with 90% or more data
  }
  
  wh.sel<- which(as.vector(sel), arr.ind = TRUE)
  V = sum(sel)
  Y = matrix(NA, nrow=time_points, ncol=V)
  for (time_point in 1:time_points) {
    Y[ time_point , ] = as.vector(out[[time_point]])[sel]
  }
  
  # also elimiate if scores are constant (e.g., all 0)
  # constant<- apply(Y, 2, function(x) diff(range(x)) < .Machine$double.eps ^ 0.5)
  # if(sum(constant)>0){
  #   wh_constant<- which(constant)
  #   Y<- Y[,-wh_constant]
  #   sel<- sel[-wh_constant]
  #   wh.sel<- which(as.vector(sel), arr.ind = TRUE)
  #   }
  # manually, ckecked, only two grid cells are highlighted...no use for this data
  return(list(Y=Y, sel = sel, wh.sel = wh.sel, nrow = nrows, ncol = ncols))
  
}

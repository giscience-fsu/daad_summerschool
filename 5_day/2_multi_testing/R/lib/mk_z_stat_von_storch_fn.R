mk_z_stat_von_storch_fn<- function(x){
  if((sum(is.na(x))/length(x))>0) return (NA)
  if (any(is.finite(x) == FALSE)) {
    x <- x[-c(which(is.finite(x) == FALSE))]
    warning("The input vector contains non-finite numbers. An attempt was made to remove them")
  }
  xn <- (x[-1] - (x[-length(x)] * rk_fn(x))) 
  # this is von Storch's correction for temporal autocorrelation
  
  z<- mk_z_stat_fn(xn)
  return(z)
}
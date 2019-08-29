pw_fn<- function(x){
    if (any(is.finite(x) == FALSE)) {
      x <- x[-c(which(is.finite(x) == FALSE))]
      warning("The input vector contains non-finite numbers. An attempt was made to remove them")
    }
    xn <- (x[-1] - (x[-length(x)] * rk_fn(x)))
    z<- mk_z_stat_fn(xn)
    return(z)
}

pw_only_fn<- function(x){
  if (any(is.finite(x) == FALSE)) {
    x <- x[-c(which(is.finite(x) == FALSE))]
    warning("The input vector contains non-finite numbers. An attempt was made to remove them")
  }
  xn <- (x[-1] - (x[-length(x)] * rk_fn(x)))
  return(xn)
}

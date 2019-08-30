S_fn<- function(x){
  #returns Kendall's S statistic
  n<- length(x)
  S0_fn<- function (i,x) -sign(x[1:(n-i)] - x[(i+1):n])
  sum(unlist(lapply(1:(n-1),S0_fn,x)))
}
rk_fn<- function(x,k=1){
  # computes lag-k serial correlation coefficient of vector x , r_k
  n<- length(x)
  mean_x<- mean(x)
  num<- (1/(n-k))*sum((x[1:n-k] - mean_x)*(x[(1+k):n]-mean_x))
  denom<- (1/n)*sum((x[1:n] - mean_x)^2)
  rk<- num/denom
  rk<- (n*rk + 1)/(n-4)# correction as in https://www.sciencedirect.com/science/article/pii/S0378375811002370?via%3Dihub
  return(rk)
}

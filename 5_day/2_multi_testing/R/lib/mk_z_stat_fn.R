mk_z_stat_fn<- function(x, y = NULL){
  #computes the Z test statistic for Kendalls S
  if(any(is.na(x))) return(NA) 
  s<- S_fn(x) #from cmk.R
  n<- length(x)
  var_s<- n*(n-1)*(2*n+5)/18
  if (length(unique(x)) < n) {
    tmp <- unique(n)
    for (i in 1:length(tmp)) {
      tie <- length(which(x == tmp[i]))
      if (tie > 1) {
        var_s = var_s-tie*(tie-1)*(2*tie+5)/18
      }
    }
  }
  z<- NA
  if(s>0) z<- (s-1)/sqrt(var_s)
  if(s==0) z<- 0
  if(s<0) z<- (s+1)/sqrt(var_s)
  return(z)
}
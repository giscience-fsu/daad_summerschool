sen0 <- function(y,x){
  zyp.slopediff <- function(i, xx, yy, n) (yy[1:(n - i)] - yy[(i + 1):n])/(xx[1:(n - i)] - xx[(i + 1):n])
  n <- length(y)
  if (missing(x)) x <- c(1:n)
  slopes <- unlist(lapply(1:(n - 1), zyp.slopediff, x, y, n))
  return(median(slopes[is.finite(slopes)], na.rm=TRUE))
}

sen <- function (y,x){
  n <- length(y)
  if (missing(x)) x <- c(1:n)
  coef = sen0(y,x)
  mysenboot = function(y,x,B=599) {
    smpfun = function(b) {
      smp = sample(length(y),size=length(y),replace=TRUE)
      sen0( y=y[smp], x=x[smp]  )
    }
    sapply(1:B, smpfun)
  }
  bt = mysenboot(y=y,x=x)
  pval0 = mean(bt >= 0, na.rm=TRUE)
  pval1 = mean(bt <= 0, na.rm=TRUE)
  pval = 2*min(pval0, pval1)
  res <- c(coef = coef, pval=pval)
  return(res)
}
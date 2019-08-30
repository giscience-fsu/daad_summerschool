bh_fn = function(pval, alpha = .05) {
  # returns a TRUE/FALSE vector where TRUE is significant using the bh correction
  ppp = order(pval, decreasing=FALSE)
  m<- length(pval)
  thr = ((1:m)/m)*alpha
  bh = pval[ppp] < thr
  for (i in length(bh):1) {
    if (is.na(bh[i])) next # moves to next i in for loop
    if (bh[i]) {
      bh[1:i] = TRUE
      break
    }
  }
  res.bh = rep(NA,m)
  res.bh[ppp] = bh
  return(res.bh)
}

bh2_fn = function(pval, alpha = .05) {
  # returns the count of significant p-values using the bh correction
  ppp = sort(pval, decreasing=FALSE)
  m<- length(pval)
  thr = ((1:m)/m)*alpha
  i=m
  while(!(ppp[i]<thr[i])){
    i = i-1
  }
  #res.bh = rep(NA,m)
  #res.bh[ppp] = bh
  return(i)
}

by_fn = function(pval, alpha = .05) {
  # returns a TRUE/FALSE vector where TRUE is significant using the by correction
  ppp = order(pval, decreasing=FALSE)
  m<- length(pval)
  thr = ((1:m)/(m*sum(1/(1:m))))*alpha
  by = pval[ppp] <= thr
  for (i in length(by):1) {
    if (is.na(by[i])) next 
    if (by[i]) {
      by[1:i] = TRUE
      break
    }
  }
  res.by = rep(NA,m)
  res.by[ppp] = by
  return(res.by)
}

by2_fn<- function(pval, alpha = .05) {
  # returns the count of significant p-values using the by correction
  ppp = sort(pval, decreasing=FALSE)
  m<- length(pval)
  thr = ((m:1)/(m*sum(1/(1:m))))*alpha
  i=m
  while(!(ppp[i]<thr[i])){
    i = i-1
  }
  #res.bh = rep(NA,m)
  #res.bh[ppp] = bh
  return(i)
}

walker_fn<- function(pval, alpha = .05){
  # returns a TRUE/FALSE vector where TRUE is significant using the walker min-p correction
  pmin<- min(pval)
  m<- length(pval)
  alpha_w<- 1-(1-alpha)^(1/m)
  res.walker<- pval < alpha_w
  return(res.walker)
}

bonf_fn<- function(pval, alpha = .05){
  # returns a TRUE/FALSE vector where TRUE is significant using the bonf correction
  pval < (alpha/length(pval))
}

holm_fn<- function(pval, alpha = .05){
  #step up method. 
  # returns a TRUE/FALSE vector where TRUE is significant using the holm correction
  ppp = order(pval, decreasing=FALSE)
  m<- length(pval)
  thr<- rev(alpha/(m-(m:1)+1))
  out = pval[ppp] < thr
  for (i in length(out):1) {
    if (out[i]) {
      out[1:i] = TRUE
      break
    }
  }
  res.out= rep(NA,m)
  res.out[ppp] = out
  return(res.out)
}
holm2_fn<- function(pval, alpha = .05){
  #step up method. returns count of sig pixel
  ppp = sort(pval, decreasing=FALSE)
  m<- length(pval)
  thr<- alpha/(m -(1:m)+1)
  i=1
  while(is.na(ppp[i])) i<- i+1 
  while(ppp[i]<= thr[i]){
    i= i+1
    if(i==(m+1)) return(m)
  }
  #out = ppp <= thr
  
  #res.out= rep(NA,m)
  #res.out[ppp] = out
  return(i-1)
}

hochberg_fn<- function(pval, alpha = .05){
  # step down method. 
  # returns a TRUE/FALSE vector where TRUE is significant using the holm correction
  ppp = order(pval, decreasing=FALSE)
  m<- length(pval)
  thr<- rev(alpha/(m-(m:1)+1))
  out<-  pval[ppp] <= thr
  if(sum(out) > 0){
      for (i in 1:length(out)) {
      if (!out[i]) {
        out[1:(i-1)] = TRUE
        break
        }
      }
    }
  res.out= rep(NA,m)
  res.out[ppp] = out
  return(res.out)
  
}

hochberg2_fn<- function(pval, alpha = .05){
  #step up method. returns count of sig pixels
  ppp = sort(pval, decreasing=FALSE)
  m<- length(pval)
  thr<- alpha/(m -(1:m)+1)
  i=length(pval)
  while(is.na(ppp[i])) i<- i-1 
  
  while(!(ppp[i]<= thr[i])){
    i= i-1
    if(i==0) return(0)
  }
  count = i
  
  #res.out= rep(NA,m)
  #res.out[ppp] = out
  return(count)
}
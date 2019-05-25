fi=function(i,m,M){
  library(expm)
  p=matrix(c(0),1)
  fi=matrix(c(0),1)
  sm=matrix(rep(0,2),2)
  for(c in 2:m){
    n=c
    for(a in 1:n) {
      p[a]=(M%^%a)[i,i]
    }
    fi[1]=p[1]
    fp=fi%*%t(p)
    sm=matrix(rep(0,n-1),n-1)
    for (a in 2:n-1) {
      for (b in 1:a) {
        sm[a]=sm[a]+fp[b,a]
      }
    }
    fi[n]=p[n]-sm[n-1]
  }
  return(list(fi,sum(fi)))
}
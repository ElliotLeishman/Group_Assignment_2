
dloops <- function(n,nreps){
  p=rep(0,2*n)
  nloops=0
  for (i in 1:nreps){
    b <- sample(1:(2*n), 2 * n, replace = FALSE)
    I=list()
    for (k in 1:(2*n)){
      l=1
      s=b[k]
      if (!s%in%I){
        if (s==k){
          I=append(I,s)
          nloops=nloops+1
          p[l]=p[l]+1
        }
        else{
          while (s!=k){
            l=l+1
            I=append(I,s)
            s=b[s]
          }
          nloops=nloops+1
          p[l]=p[l]+1
        }
      }
    }
  }
  p=p/nloops
  return (p)
  cat(p)
}
b <-sample(1:(200), 200, replace = FALSE)
b




dloops <- function(n,nreps){
  p=rep(0,2*n)
  for (i in 1:nreps){
    b <- sample(1:(2*n), 2 * n, replace = FALSE)
    for (k in 1:2*n){
      s=b[k]
      while(s!=b[k]){
        s=b[s]
      }
        p[s]=p[s]+1}
  }
  p=p/sum(p)
  print(p)
  }









dloops <- function(n,nreps){
  p=array(0,2*n)
  for (i in 1:nreps){
    s=array(NA,2*n)
    b <- sample(1:(2*n), 2*n, replace = FALSE)
    s[1] <- b[1]
    for (i in 2:(2*n)){
      s[i] <- b[s[i-1]]
      if(s[i]==b[1]){
        p[sum(!is.na(s))-1]<-p[sum(!is.na(s))-1]+1
        break
      }
    }
    if (s[i]!=b[1]){
      p[sum(!is.na(s))]<-p[sum(!is.na(s))]+1
    }
  }
  print(p)
  p=p/sum(p)
  print(p)
}









check <- function(k, a){
  ## Function checks if a number k is in a vector a if so it returns 1 else 0.
  if (k %in% a){return(1)}
  else{return(0)}
}




dloops <- function(n,nreps){
  p=array(0,2*n)
  for (h in 1:nreps){
    s=array(NA,c(2*n,2*n))
    b <- sample(1:(2*n), 2*n, replace = FALSE)
    for (j in b)
      if (check(j,s)==0){
        s[j,1] <- b[j]
        for (i in 2:(2*n)){
          s[j,i] <- b[s[j,i-1]]
          if(s[j,i]==b[j]){
            p[sum(!is.na(s[j,]))-1]<-p[sum(!is.na(s[j,]))-1]+1
            break
          }
        }
        if (s[j,i]!=b[j]){
          p[length(s[j,])]<-p[length(s[j,])]+1
        }
      }
  }
  print(p[1:100])
  p_len<-p[1:100]/sum(p[1:100])

  return(p_len)
}
p <- dloops(50,10000)
p1 <- p[1:100]
p1
scatter.smooth(1:100,p1)

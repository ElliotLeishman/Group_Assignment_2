simulation_experiment <- function (b,k,strategy){
    box_values <- b
    n=length(b)/2
    choice <- c() # Generates the choice of boxes the prisoner will take depending on the strategy
    if (strategy == 1){
      choice[1] <- box_values[k]
      i=1
      while (choice[i]!=k && i<=n){
        choice[i+1] <- box_values[choice[i]]
        i=i+1
      }
    }
    if (strategy == 2){
      random <- sample(1:(2*n), 1, replace = FALSE)
      choice[1] <- box_values[random]
      i=1
      while ((choice[i]!=k) && (i<=n)){
        choice[i+1] <- box_values[choice[i]]
        i=i+1
      }
    }
    if (strategy == 3){
      choice <-box_values[sample(1:(2*n),n, replace = FALSE)]
    }
    return (k %in% choice)
}


pone <- function(n, k, strategy, nreps){
  count = 0 # initialise counter
  for (i in 1:nreps){
    b <- sample(1:(2*n), 2 * n, replace = FALSE)
    if (simulation_experiment(b,k,strategy)==TRUE){
      count=count+1
    }
  }
 count=count/nreps
 return (count)
}

  
pall <-  function(n, strategy, nreps){
  count=0
  for (i in 1:nreps){
    b <- sample(1:(2*n), 2 * n, replace = FALSE)
    k=1
    while (simulation_experiment(b,k,strategy)==TRUE && k<2*n){
      k=k+1
    }
    if (k==2*n){
      count=count+1
    }
  }
  count=count/nreps
  return (count)
}  



dloopsold  <- function(n,nreps){
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



check <- function(k, a){
  ## Function checks if a number k is in a vector a if so it returns 1 else 0.
  if (k %in% a){return(1)}
  else{return(0)}
}




dloops <- function(n,nreps){
  p=array(0,2*n)
  for (h in 1:nreps){
    s=array(0,c(2*n,2*n))
    b <- sample(1:(2*n), 2*n, replace = FALSE)
    for (j in b){
      if (check(j,s)==0){
        s[j,1] <- b[j]
        k=1
        while ((s[j,k]!=j) && (k<(2*n))) {
          s[j,k+1]=b[s[j,k]]
          k=k+1
        }
        p[k]=p[k]+1
        }
    }
  }
  p_len<-p[1:(2*n)]/sum(p[1:(2*n)])
  
  return(p_len)
}


p <- dloops(50,10000)
p1 <- p[1:100]
p1
scatter.smooth(1:100,p1)
1-sum(p1[51:100])



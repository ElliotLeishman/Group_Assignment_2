simulation_experiment <- function (n,b,k,strategy){
    box_values <- b
    choice <- 1:n # Generates the choice of boxes the prisoner will take depending on the strategy
    if (strategy == 1){
      choice[1] <- box_values[k]
      for (i in 2:n){
        choice[i] <- box_values[choice[i-1]]
      }
    }
    if (strategy == 2){
      random <- sample(1:(2*n), 1, replace = FALSE)
      choice[1] <- box_values[random]
      for (i in 2:n){
        choice[i] <- box_values[choice[i-1]]
      }
    }
    if (strategy == 3){
      choice <- sample(1:(2*n),n, replace = FALSE)
    }
    return (choice)
}


pone <- function(n, k, strategy, nreps){
  count = 0 # initialise counter
  for (i in 1:nreps){
    b <- sample(1:(2*n), 2 * n, replace = FALSE)
    if (k %in% simulation_experiment(n,b,k,strategy)){
      count=count+1
    }
  }
 count=count/nreps
 return (count)
}

  
pall <-  function(n, strategy, nreps){
  count_all=0
  for (i in 1:nreps){
    b <- sample(1:(2*n), 2 * n, replace = FALSE)
    count=0
    for (k in 1:(2*n)){
      if (k %in% simulation_experiment(n,b,k,strategy)){
        count=count+1
      }
    }
    if (count==2*n){
      count_all=count_all+1
    }
  }
  count_all=count_all/nreps
  return (count_all)
}  



dloops <- function(n,nreps){
  p=rep(0,2*n)
  nloops=0
  for (i in 1:nreps){
    b <- sample(1:(2*n), 2 * n, replace = FALSE)
    I=list()
    for (k in 1:(2*n)){
      l=1
      s=b[k]
      if (s==k){
        nloops=nloops+1
        p[l]=p[l]+1
      }
      else if (!s%in%I){
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
  p=p/nloops
  return (p)
  cat(p)
}
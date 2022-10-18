## Need an introduction describing the script/ problem, also should describe
## each strategy


Box_Choice <- function(n, k, box_values, strategy){
  ## This function will be called when we need a selection of boxes as chosen 
  ## by either strategy 1, 2 or 3. Its inputs are:
  ##    n - The number of boxes to be chosen,
  ##    k - The number of specific prisoner,
  ##    box_values - The number inside each box (to be generated before function
  ##    is called),
  ##    strategy - Determines which strategy we should use out of 1,2 or 3.
  ## The function outputs a vector, named choice, of n numbers between 1 and 2n 
  ## inclusive which have been chosen by following the indicated strategy.
  
  choice <- 1:n # Feel like this should be filled with zeros
  if (strategy == 1){
    choice[1] <- box_values[k]
    for (i in 2:n){
      choice[i] <- box_values[choice[i-1]]
    }
  }
  if (strategy == 2){
    random <- sample(1:(2*n), 1, replace = FALSE) ## Selects the random number
    ## to start from.
    choice[1] <- box_values[random]
    for (i in 2:n){
      choice[i] <- box_values[choice[i-1]]
    }
  }
  if (strategy == 3){
    choice <- sample(1:(2*n),n, replace = FALSE)
  }
  return(choice)
}

check <- function(k, a){
  ## Function checks if a number k is in a vector a if so it returns 1 else 0.
  if (k %in% a){return(1)}
  else{return(0)}
}
# test cases
check(3, c(3,4,6,8,0,3)); check(-1, c(3,1,4,6,8,0,3))


Pone <- function(n, k, strategy, nreps){
  count <- 0
  for (i in nreps){
    box_values <- sample(1:(2 * n), 2 * n, replace = FALSE) # Add to next line?
    count <- count + check(k, Box_Choice(n, k, box_values, strategy))
  }
}
# test cases
pone(50, 33, 1, 5); pone(50, 4, 2, 100); pone(50, 4, 3, 100);



pall <- function(n, strategy, nreps){
  A <- array(0, c(nreps, 2 * n))
  for (i in 1:nreps){
    box_values <- sample(1:(2 * n), 2 * n, replace = FALSE)
    for (k in 1:(2 * n)){
      A[i,k] <- check(k, Box_Choice(n, k, box_values, strategy))
    }
  }
  x <- apply(A, 1, sum) # Sum rows 
  x <- match(x, 2 * n)  # Finds entries equalling 2n
  x <- x[which(x!="NA")]# Remove NAs
  return(length(x)/ nreps)
}

# test cases
pall(50, 1, 1000);pall(50, 2, 1000);pall(50, 3, 1000)

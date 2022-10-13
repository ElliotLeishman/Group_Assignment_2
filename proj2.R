## For writing

n = 35
# say n = 10 ie there are 20 prisoners


## Define the strategies
box_values <- sample(1:(2*n), 2 * n, replace = FALSE)
k = 4
# list of opened boxes
choice <- 1:n
choice[1] <- box_values[k]
for (i in 1:n){
  choice[i+1] <- box_values[choice[i]]
}
cat(choice)

#  strat_2 <-
b <- sample(1:(2*n), 2 * n, replace = FALSE)
k <- sample(1:(2*n), 1, replace = FALSE)
# list of opened boxes
choice <- 1:n
choice[1] <- b[k]
for (i in 1:n){
  choice[i+1] <- b[choice[i]]
}
cat(choice) 
  
  
  
  
#
strat_3 <-  sample(1:(2*n), n, replace = FALSE)
# Random choice can we assume that they will open the same box twice?

strat_3







## Question 1 - just done with strategy 3
pone <- function(n, k, strategy, nreps){
  count = 0 # initialise counter
  
  
  for (i in 0:nreps){
    choice <- 1:n
    # Generates the choice of boxes the prisoner will take depending on the strategy
    if (strategy == 1){
      box_values <- sample(1:(2*n), 2 * n, replace = FALSE)
      choice[1] <- box_values[k]
      for (i in 1:n){
        choice[i+1] <- box_values[choice[i]]
      }
    }
    if (strategy == 2){
      box_values <- sample(1:(2*n), 2 * n, replace = FALSE)
      random <- sample(1:(2*n), 1, replace = FALSE)
      choice[1] <- box_values[random]
      for (i in 1:n){
        choice[i+1] <- box_values[choice[i]]
      }
    }
    if (strategy == 3){
      choice <- sample(1:(2*n),n, replace = FALSE)
    }
    if (k %in% choice){
      count <- count + 1
    }
    }
  cat(count)
  
}  
pone(10,4,2,10)

# Write a function to check if a number is in a vector
check <- function(target){
  
  for (i in 0:4){
    b <- sample(1:(2*n),n, replace = FALSE);cat(b)
    if (target %in% b){
      cat("yes")
    }else{cat("no")}
  }
}


check(8)





  

  
  
  
  
  
  










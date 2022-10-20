## Statistical Programming, Practical 2
## Github repo: https://github.com/ElliotLeishman/Group_Assignment_2.git
## Christos Giannikos (s2436019), Elliot Leishman (s1808740), 
## Patrick Renaud (s2462989)

## We have 2n prisoners’ number between 1 and 2n. Each prisoner enters a room 
## filled with 2n boxes, each boxes in turn has a number between 1 and 2n inside 
## it. Each prisoner gets to choose n boxes, if every prisoner chooses a box 
## containing their own number all prisoners go free. In this assignment we 
## investigate the effect different strategies for picking the boxes has on the 
## probability that the prisoners all go free. 
## Specifically, we investigate the following three strategies:
## 1: Each prisoner starts with the box corresponding to their number, eg. The 
## first prisoner will open the first box. If the number inside this box is the 
## prisoners’ number then the prisoner has found their number. If it isn’t, they 
## move to the box corresponding to that number. This is repeated until the 
## prisoner has selected n boxes.
## 2: Exactly the same as strategy 1 but each prisoner starts with a random box.
## 3: Each prisoner chooses n boxes at random.

##General Outline: 
## We define a function called Box_Choice which returns the box choices a 
## prisoner will make depending on the strategy they have chosen. Next, we 
## define a check function which simply checks if a vector contains a specific 
## determined element. We then define a function Pone which determines the 
## probability an individual prisoner will find their number given a strategy. 
## Function Pall then determines the probability that all prisoners go free. 
## Finally, we define a function dloop to investigate the presence of
## loops in a random shuffling of the cards inside the boxes.



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
  
  choice <- array(0, c(n)) ## Intialise choice vector
  if (strategy == 1){ ## Strategy 1
    choice[1] <- box_values[k] ## Sets first element as the number in box k.
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
    choice <- sample(1:(2*n),n, replace = FALSE) ## Generates n random integers 
    ## between 1 and 2n.
  }
  return(choice)
}

check <- function(k, a){
  ## Function checks if a number k is in a vector a if so it returns 1 else 0.
  if (k %in% a){return(1)}
  else{return(0)}
}


pone <- function(n, k, strategy, nreps){
  ## Function which runs a stochastic model to estimate the probability one
  ## prisoner finds their number. Its inputs are:
  ##    n - number of boxes opened by prisoner,
  ##    k - prisoner number,
  ##    strategy - strategy deployed by prisoner,
  ##    nreps - number of times we run stochastic model.
  ## It outputs a real number.
  count <- 0 ## Initialise counter
  for (i in 1:nreps){
    box_values <- sample(1:(2 * n), 2 * n, replace = FALSE) ## Generates the 
    ## numbers in each box randomly.
    count <- count + check(k, Box_Choice(n, k, box_values, strategy)) ## If the 
    ## vector returned by the Box_Choice function contains the prisoners number
    ## we add one to the counter.
  }
  return(count/nreps)
}


pall <- function(n, strategy, nreps){
  ## Function runs a stochastic simulation to estimate the probability that all 
  ## prisoners find their number and hence are released. It has the inputs n, 
  ## strategy and nreps, which are already defined in previous functions. It
  ## returns a real number between 0 and 1.
  ## It works by first initializing a zero array of dimensions nreps x 2n. Then
  ## generating the numbers inside each box, since these numbers should be same 
  ## for all prisoners in each simulation. We then replace the entries of the
  ## array using the check and Box_Choice functions to store data on if each
  ## prisoner finds their number. We then manipulate this data to return the 
  ## probability.
  
  
  A <- array(0, c(nreps, 2 * n)) ## Set up empty array.
  for (i in 1:nreps){
    box_values <- sample(1:(2 * n), 2 * n, replace = FALSE)
    for (k in 1:(2 * n)){
      A[i,k] <- check(k, Box_Choice(n, k, box_values, strategy))
    }
  }
  x <- apply(A, 1, sum) ## Sum rows 
  x <- match(x, 2 * n)  ## Finds entries equalling 2n, these are the ones where 
  ## all prisoners find their number.
  x <- x[which(x!="NA")]## Remove NAs
  return(length(x)/ nreps)
}

## We now estimate the probability a prisoner finds their number by running the
## function Pone, for each strategy, first for n = 5. 
pone(5, 1, 1, 1000) ;pone(5, 1, 2, 1000);pone(5, 1, 3, 1000)
## and then for n = 50.
pone(50, 1, 1, 1000);pone(50, 1, 2, 1000);pone(50, 1, 3, 1000)
## We have set k = 1 as the choice of k is arbitrary and will not affect the 
## probabilities.

## Now we estimate the joint probability all prisoners are freed by running the
## function Pall, for each strategy, again for n = 5 and n = 50.
pall(5, 1, 1000);pall(5, 2, 1000);pall(5, 3, 1000)
pall(50, 1, 1000);pall(50, 2, 1000);pall(50, 3, 1000)


## The results from running the function suggest that all three strategies give 
## approximately the same chance of suggest for one prisoner. However, rather 
## surprisingly, for all the prisoners to achieve success, the first strategy is 
## much better at around 33% chance of success. We hypothesise this is due to 
## this method not being truly independent since the prisoners all start at the 
## box predetermined by their number. 

## It is worth pointing out that if strategies 2 and 3 are indeed independent as 
## the simulation would suggest, the joint probability of two prisoners being 
## set free (ie n=1) is 0.5^2 or 0.25. This is less than the probability of 100 
##prisoners being released if they follow strategy 1.

dloop <- function(n,nreps){
  ## This is a function that receives as arguments the number of prisoners 
  ## divided by two (n) and the number of total reiterations (nreps) of the 
  ## simulation experiment, in order to estimate the probability of each loop 
  ## of length from 1 to 2n occurring at least once in a random shuffling of 
  ## cards (i.e. numbers of prisoners) to boxes. As a result, it returns a 
  ## vector p_len of 2*n elements, where p[l] is the estimated probability (by 
  ## simulation) of a loop of length l appearing in a random shuffling of cards
  ## to boxes. 
  ## After initializing a 2*n vector p for the desired probabilities to zero, in
  ## each of the nreps iterations, the function examines each of the boxes in 
  ## sequence. If the card inside the examined box is not already part of a loop
  ## that the function has so far determined, it determines the loop including
  ## this element (indexed j). This is achieved by storing the elements of the 
  ## loop in the j-th row of the array s (so to exclude the elements of the loop
  ## that will be tested later) and increasing p[l] at the end, where l is the 
  ## length of the determined loop. Finally, it divides the number of the 
  ## observed loops of length l by the total number of loops observed, thus 
  ## rendering the elements of p_len probabilities. 
  
  p=array(0,2*n) #initializing probabilities to zero 
  for (h in 1:nreps){
    s=array(0,c(2*n,2*n))# initializing the s array where each nonzero row will
    ## contains the elements of a loop
    b <- sample(1:(2*n), 2*n, replace = FALSE) #creating a random distribution
    ## of the cards (numbers of prisoners) to boxes 
    for (j in b){
      if (check(j,s)==0){ ##first exclude any elements already in a loop 
        s[j,1] <- b[j]##setting the first element of the loop 
        l=1 ## initializing the length of the loop 
        while ((s[j,l]!=j) && (l<(2*n))){ ## keep increasing the depth of the 
          ## loop until we find the desired element j
          s[j,l+1]=b[s[j,l]] ## adding newly determined elements of the loop
          l=l+1
        }
        p[l]=p[l]+1 ##increase the number of observed loops of length l 
      }
    }
  }
  p_len<-p[1:(2*n)]/sum(p[1:(2*n)]) ##divide by the number of total loops 
  
  return(p_len)
}

## We will now provide an example of estimating the probabilities of observing 
## loops of length l, l=1,...,2*n for n=50, i.e. for 100 prisoners 
p <- dloop(50,10000)

## Furthermore, we will try to access the probability of no loops longer than 
## n=50 appearing in a random reshuffling of cards to boxes. This probability 
## can be estimated by 1 minus the sum the estimated probabilities of observing 
## a loop of length l, l=51,52,...,100 in a random shuffling of 100 cards, i.e.
  p_under_50=1-sum(p[51:100])
  
## This probability is equal to the probability that 100 prisoners succeed in 
## finding their cards when acting under strategy 1, i.e pall(100,1,), since the
## existence or not of a loop larger than n is what determines whether they 
## succeed or not. As a result, we expect it to be close to 0.3333, since in 
## theory it is equal to 1-(1/51+1/52+...+1/100)=1/3.
  
  
## Finally, we will try to visualize the probabilities of p-under_50 that we 
## calculated with a simple scatterplot
## First, we create a data frame with the data we desire to visualize
  data = data.frame( x=1:100, y=p)

## Basic scatterplot
## We place on the x-axis the possible lengths of the loops in a random 
## shuffling of 100 cards and on the y-axis the probabilities of observing 
## loops of length l
plot(data$x, data$y,
     xlab="length of loops", ylab="probability of observation",
     main="A simple visualizatuation")



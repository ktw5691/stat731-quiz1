#
#         Theory of Statistics - CQAS 721
#
#            Probability as Frequency
#       
#      source('stat-721-computing-session-1.R')
#
#            Fall 2013, Ernest Fokoue 
#
#
#


   graphics.off()
   rm(list=ls())

#  This section loads all the needed libraries (packages)

   library(IPSUR)
   library(prob)
   library(distrEx)
   library(distr)

#  The randomness generated a computer is not entirely random indeed
#  It is referred to as pseudo-randomness

   set.seed(8262009)

#
#  Random sampling as the foundation of stochastic experiments
#  The R function sample()
#  We can sample with replacement or without replacement
#


#
#  Let's use the function sample() to compute the probability of seeing HEAD
#  From a fair coin toss
#
#           Pr[H] = lim n(H)/n as n -> infinity 
#
#  Basically the probability of the event if the relative frequency
#   of occurrence of that event out of n independent random trials
#
#     See the R Script source('stat-721-prob-as-frequency.R')
#
#  For now, we use the function sample() to get the random draws needed 
#

   n <- 10000
   f <- 0
   for(i in 1:n)
   {
      f <- f + ifelse(sample(c('H','T'))[1]=='H',1,0)
   }

   cat('\n Pr[Head] = ',f/n,'\n')

#  Exercise 1: (a) Redo the same to compute the probability that a fair die lands on 5
   
#  What does a sequence of outcomes from a fair coin toss look like?

   sequence <- NULL
   for(i in 1:5) sequence <- c(sequence, sample(c('H','T'),1))
   print(sequence)

#  
#  Activity: (a) Change the length of the sequence using 4, 5, 7 , 10, 100
#            (b) How does the length of the sequence impact its characteristics?
#

#  
#  Let's now turn to the random experiment of rolling more than one die
#  We first roll two dice and compute the probability of identical outcomes

   n <- 10000
   f <- 0
   for(i in 1:n)
   f <- f + ifelse((sample(6)[1]==sample(6)[1]),1,0)
   print(f/n)

#  Exercise 2:  Roll two dice and compute empirical probability that
#  (a) The first yields an outcome greater than the second
#  (b) The sum of the outcomes is less than 9
#  (c) In each case above compare your empircal probabilities to the theoretical counterparts

#  Exercise 3:  Toss three coins dice and compute empirical probability that
#  (a) Getting at least one head
#  (c) Compare your empircal probabilities to its theoretical counterpart


#
#  Using the sample() command to draw subsets with or without replacement
#           sample(S, k, replace=T) 
#

#   Activity: let S = {-2, 0, 1, 3} with each equally likely
#   Compute Pr[X=-2] empirically (using relative frequencies)
#

#   Allowing repetitions through iterations

    n <- 10000
    f <- 0
    for(i in 1:n)
    f <- f + ifelse(sample(c(-2,0,1,3),1)==-2,1,0) 
    print(f/n)    

#   Using the replace=T to achieve repetitions and make the command compact

    f <- length(which(sample(c(-2,0,1,3),n, replace=T)==-2))
    print(f/n)

#
#   Exercise 4: Calculate empirically the probability that X in non negative
#

#
#  How does one go about computing the probabilities when the events are
#                       Not Equally Likely
#
#

#  Exercise: Suppose that bernoulli trial has success probability 3/4
#            Write the set A from which to draw the samples to reflect 
#            This characteristic

   A <- c('S','F','S','S')
   print(length(which(sample(A, 10000, replace=T)=='S'))/10000)

#  We can draw from the set A with or without replacement
#  What is the probability of at least one success in 2 draws

   n <- 10000
   f <- 0
   for(i in 1:n)
   f <- f + ifelse(sum(sample(A,2, replace=T)=='S')==0,1,0)
   print(1-f/n)
 

#  Direct calculation gives

   print(1-dbinom(0, size=2, prob=0.75))


#  What does a sequence of outcomes from a loaded coin toss look like?
#         D = {'H','T','H','H'}    Pr[H] = 3/4
#

   D <- c('H','T','H','H') 
   sequence <- NULL
   for(i in 1:5) sequence <- c(sequence, sample(D,1))
   print(sequence)

#  
#  Activity: (a) Change the length of the sequence using 4, 5, 7 , 10, 100
#            (b) How does the length of the sequence impact its characteristics?
#

#  Exercise 5: Redo the above with success probability 2/3 and number of successes 3
#  Exercise 6: Modify the code on probability as frequency to display probability
#              of event occuring 60% of the time
 
#  Clearly the above approach does work. However there are much easier ways
#  Using the already coded common distributions like the binomial in this case
#

################################################################################
#
#                 Sample Spaces, Counting and Combinatorics
#         
################################################################################

#  The formulas used in the four possible combinations of 
#  replace and ordered are as follows.

#  When replace = TRUE and ordered = TRUE, the value is n^k.
#  When replace = FALSE and ordered = TRUE, the value is n!/(n-k)!.
#  When replace = FALSE and ordered = FALSE, the value is n!/[k!(n-k)!].
#  When replace = TRUE and ordered = FALSE, the value is (n-1+k)!/[(n-1)!k!].
#

#
#  From the alphabet A={a,b,c,d,e} a dictionary is built with  words of   
#  Lengths 2, 3, and 4
#  If the academy allows a letter to appear more than once in a word
#  But considers the order of letters irrelevant
#  (a) How many two letter words are there
#  (b) How many three letter words are there
#  (c) How many four letter words are there  
#  (d) How many words are in this dictionary
#  (e) Enumerate the words of the dictionary
#

   A  <- c('a','b','c','d','e')

#  Counting and combinatorics

   nw.2 <- nsamp(n=5, k=2, replace=T, ordered=F)
   nw.3 <- nsamp(n=5, k=3, replace=T, ordered=F)
   nw.4 <- nsamp(n=5, k=4, replace=T, ordered=F)
 
   ds <- nw.2 + nw.3 + nw.4


#  Combinatorial Enumeration

   dictionary <- NULL
   w2 <- urnsamples(A, size=2, replace=T, ordered=F)
   for(i in 1:nrow(w2)) 
   { 
     newword <- ''
     for(j in 1:2){newword <- paste(newword, as.character(w2[i,j]),sep='')}
     dictionary <- c(dictionary,newword)
   }

   w3 <- urnsamples(A, size=3, replace=T, ordered=F)
   for(i in 1:nrow(w3)) 
   { 
     newword <- ''
     for(j in 1:3){newword <- paste(newword, as.character(w3[i,j]),sep='')}
     dictionary <- c(dictionary,newword)
   }


   w4 <- urnsamples(A, size=4, replace=T, ordered=F)
   for(i in 1:nrow(w4)) 
   { 
     newword <- ''
     for(j in 1:4){newword <- paste(newword, as.character(w4[i,j]),sep='')}
     dictionary <- c(dictionary,newword)
   }

   cat('\n Here is your dictionary'\n')
   print(dictionary)

   cat('\n Dictionary size by enumeration ', length(dictionary), '\n')

   cat('\n Dictionary size by theorerical calculation ', ds, '\n')

#
#  Exercise: Build the dictionary from scratch with the following characteristics
#   (a) Letters cannot be repeated and order does not matter
#   (b) Letters cannot be repeated but order matters
#   (c) Letter can be repeated and order matters
#    For each of the scenarios,  (1) Display the dictionary (2) Compute the size
#    Also provide the theorerical calculation before the enumeration
# 


################################################################################
#
#     Sample Spaces, Probability Spaces, Events and Discrete Probabilities
#         
################################################################################

#  Discrete Distribution  

#  Recall that an event is a subset of the sample space S
#  We use the functions subset(), %in%, isin()

#
#  Exercise: Let S ={-2,0,1,3} and Pr[X=x] = a(x^2+1) for x in S
#  (a) Find the value of a
#  (b) Create the probability space
#  (c) Generate a barplot of the pmf of X 


#
#  Probability Spaces for Equally Likely Events
#
 
   pmf.coin <- probspace(tosscoin(1))           # Tossing a fair coin
   print(pmf.coin)
   barplot(pmf.coin$probs, names=pmf.coin[,1], col='blue', ylab='Pr[X=x]')

   pmf.three.coins <- probspace(tosscoin(3))    # Tossing 3 fair coins
   pmf.two.dice <- probspace(rolldie(2))        # Rolling 2 fair 6-faces dics

   pmf.two.dice.2 <- rolldie(2, makespace=T)    # Rolling 2 fair 6-faces dics

#  Warning: Do NOT Try rolldie() with more than 7


#
#  Probability Spaces for Non Equally Likely Events
#

   pmf.coin.loaded <- probspace(tosscoin(1), probs=c(3/4,1/4))
   print(pmf.coin.loaded)
   barplot(pmf.coin.loaded$probs, names=pmf.coin.loaded[,1], col='blue', ylab='Pr[X=x]')


#
#  Compound Events: Let's play cards
#  

   S <- cards()
   A <- which(S$suit=='Heart')
   B <- which(S$rank %in% 7:9)

#  Activity 
#  (a) Find Pr[S]
#  (b) Find Pr[A]
#  (c) Find Pr[B]
#  (d) Find Pr[A and B]
#  (e) Find Pr[A or B]
#  (f) Find Pr[A|B]
#  (g) Find Pr[B|A]

   
   AandB <- intersect(A,B) 
   AorB  <- union(A,B)
   n  <- nrow(S)
   nA <- length(A)
   nB <- length(B)
   nAandB <- length(AandB)
   nAorB  <- length(AorB)

   cat('\n Pr[A] =  ', nA/n ,'\n')
   cat('\n Pr[A|B] =  ', nAandB/nB ,'\n')
   cat('\n Pr[A]+Pr[B]-Pr[A and B] =  ', (nA+nB-nAandB)/n ,'\n')
   cat('\n Pr[A or B] =  ', (nAorB)/n ,'\n')



#
#   Famous birthday problems
#
    nmax <- 99
    birth <- numeric(99)

    for(j in 1:nmax) 
    {
      n <- j + 1
      m <- 1000
      f <- 0
      for(i in 1:m)
      { 
        f<-f+ifelse(length(unique(sample(365, n, replace=T)))<n,1,0)
      }
      birth[j] <- f/m
    } 

    plot(2:100, birth, xlab='n=number of people in the room', 
         ylab='Probability of at least two identical birthdays',
         type='l')

    id <- min(which(birth>0.5))
    abline(v=id, col='blue', lwd=2)
    abline(h=0.5, col='red', lwd=2) 

#
#   Famous birthday problems (theoretical)
#
    nmax <- 99
    birth <- numeric(99)

    for(j in 1:nmax) 
    {
      n <- j + 1
      birth[j] <- 1-prod((365-1:n)/365)
    } 

    windows()
    plot(2:100, birth, xlab='n=number of people in the room', 
         ylab='Probability of at least two identical birthdays',
         type='l')

    id <- min(which(birth>0.5))
    abline(v=id,  col='green', lwd=2)
    abline(h=0.5, col='purple', lwd=2) 

    
#
#  The probability of 'appearing' on the air on a radio program is 1/4   
#  What is the probability of a caller needing more than 5 calls to 'appear'
#  It is assumed that the calls succeed or fail independently
#

   n <- 10000
   x <- 1+rgeom(n, prob=0.25)
   probA <- length(which(x>5))/n
   cat('\n Empirical Pr[X>5] = ', probA,'\n')
   cat('\n Theoretical Pr[X>5] ', 1-pgeom(4,prob=0.25),'\n')
 
#
#  The probability of 'appearing' on the air on a radio program is 1/4   
#  What is the probability of a caller needing more than 7 calls to 'appear'
#  Given that he has already tried 5 times
#  It is assumed that the calls succeed or fail independently
#

   n   <- 100000
   x   <- 1+rgeom(n, prob=0.25)
   A   <- which(x>5)
   nA  <- length(A)
   AB  <- which(x > 7)   # Really the same as  which(x > 7)
   nAB <- length(AB)
 
   probA  <- nA/n
   probAB <- nAB/nA
  
   cat('\n Empirical Pr[X > 7 | X > 5] = ', probAB,'\n')
   cat('\n Theoretical Pr[X > 7| X > 5] = Pr[X > 2] = 1-Pr[X<=2] ', 1-pgeom(1,prob=0.25),'\n')

#
#  Exercise: rbinom(n, size=7, prob=0.6)
#  X = Number of successes out of 7 trials with success prob=0.6
#  Compute Pr[X > 5 | X > 3] using frequencies
#  Compare it Pr[X > 2]  Note: use frequencies or theory
#
 

#
#  Joint distribution and marginal distribution
#
   
   S <- rolldie(2, nsides=4, makespace=TRUE)
   marginal(S, vars="X1")
 
   S <- addrv(S, U = X1+2*X2)
   marginal(S, vars="U")   


#  Plotting discrete distributions

   X<-Binom(size=5, prob=.75)
   plot(X)

#
#  Continuous Distributions
#

#      
#  Let   f(x) = 3 x^2 for 0 < x < 1  
#  compute Pr[1/5 < X < 3/5]
#
   
   f <- function(x){ifelse(x<0 | x > 1,0,3*x^2)}

#  Plot the pdf
  
   windows()
   cord.x <- c(1/5,seq(1/5,3/5,0.01),3/5) 
   cord.y <- c(0,f(seq(1/5,3/5,0.01)),0) 
   curve(f, 0, 1, xlim=c(0,1), main='Univariate PDF')
   polygon(cord.x,cord.y,col='gold')
   abline(h=0)

   probE <- integrate(f, lower=1/5, upper=3/5)   
   cat('\n Pr[1/5 < X < 3/5] is \n')      
   print(probE)

#  The CDF in this case is
   
   F <- function(x){ifelse(x<0,0,ifelse(x<1,x^3,1))}

   probE <- F(3/5) - F(1/5)

#  Plot the cdf

   windows() 
   curve(F, 0, 2, xlim=c(0,2), col='gold', main='Univariate CDF')
   abline(h=0)

#      
#  Let   f(x) = 3/ x^4 for x > 1  
#  compute Pr[2 < X < 4]
# 
   f <- function(x){3/(x^4)}
   probE <- integrate(f, lower=2, upper=4)   
   cat('\n Pr[2 < X < 4] is \n')      
   print(probE)

#  Monte Carlo Approach

   n <- 10000
   x<-runif(n, 2,4)
   probE.MC <- ((4-2)/n)*sum(3/(x^4))
   cat('\n Monte Carlo Pr[2 < X < 4] =',probE.MC,'\n')      

#
#  Exercise:
#    
#  Let   f(x) = 3 x^2 for 0 < x < 1  
#  compute Pr[1/5 < X < 3/5] using Monte Carlo
#

   
#
#  Computing the Buffon's needle probability using relative frequencies
#          We must input L, D and m = number of trials
#


   L <- 2
   D <- 4   
   
   buffon <- function(L, D, n)
   {
      X <- runif(n, 0, D/2)
      theta <- runif(n, 0, pi/2)
      f <- length(which(X < (L/2)*cos(theta)))

      return(f/n)
   }

#
#  Theoretical probability of Buffon's needle given L and D is
#               (2*L)/(pi*D)
#    
 
   theo.buffon <- (2*L)/(pi*D) 

#
#         Seeing the effect of the number of trials 
#  Frequentist probability is a limit as m tends to infinity
#

   mat.buffon <- NULL
   n.max      <- 500

   for(i in 1:n.max)
   {
       n <- 200*i
       freq.buffon <- buffon(L, D, n)
       mat.buffon <- rbind(mat.buffon, c(n, round(theo.buffon,3), round(freq.buffon,3)))
   }
   
   mat.buffon <- data.frame(mat.buffon)
   colnames(mat.buffon) <- c('n','Theoretical','Empirical')

#  Display the last ten iterations

   print(mat.buffon[(n.max-10):n.max,])

   # If n.max is large, you should see no difference between the theoretical and the empirical

#  Plot the results
   
   plot(mat.buffon[,1],mat.buffon[,3], xlab='n=number of trials', ylab='Relative Frequency=Pr[B]=n(B)/n', lty=2, type='l', col='red')
   lines(mat.buffon[,1],mat.buffon[,2], lwd=3, col='green', lty=1)
   legend('topright', inset=0.02, c('Relative frequency', 'Theoretical Probability'), col=c('red','green'), lty=c(2,1))

#  Exercise: Explore with difference values of n.max and L and D 
#  (a) How does n.max affect the results 
#  (b) How does the change of L and/or D affect the computations
#

#
#  Exercise: f(x,y) = k xy  0 < x < 1 and 0 < y < 1   
#  (a) Find k
#  (b) Find Pr[A] = Pr[X<1/2]
#  (c) Find Pr[B] = Pr[Y>3/4]  
#  (d) Find Pr[A and B] = Pr[X < 1/2 and Y > 3/4]
#  (e) Find Pr[A|B] = Pr[X < 1/2 | Y > 3/4] 
#  (f) Compare (e) to (a) and Comment use = 10000, 100000, 1000000
#  (g) Compare your results to their theoretical counterparts

   n   <- 1000000 
   x   <- runif(n,0,1)
   y   <- runif(n,0,1)
   pxy <- 4*(x*y) 
    
   pA  <- sum(pxy[which(x < 1/2)])/n
   pB  <- sum(pxy[which(y > 3/4)])/n
   pAB <- sum(pxy[which(x < 1/2 & y > 3/4)])/n 

   pAgB <- pAB/pB

   cat('\n Pr[A] = ', pA, '\n') 
   cat('\n Pr[A|B] = ', pAgB, '\n')


#
#  Exercise: f(x,y) = k xy  0 < x < 2 and 0 < y < 2   
#  (a) Find k
#  (b) Find Pr[A] = Pr[X>1]
#  (c) Find Pr[B] = Pr[Y<1]  
#  (d) Find Pr[A and B] = Pr[X > 1 and Y < 1]
#  (e) Find Pr[A|B] = Pr[X > 1 | Y < 1] 
#  (f) Compare (e) to (a) and Comment use = 10000, 100000, 1000000
#  (g) Compare your results to their theoretical counterparts



#
#  Exercise: f(x,y) = k exp{-(5x+2y)}  x > 0 and y > 0
#  (a) Find a
#  (b) Find Pr[A] = Pr[X<1]
#  (c) Find Pr[B] = Pr[Y>1]  
#  (d) Find Pr[A and B] = Pr[X < 1 and Y > 1]
#  (e) Find Pr[A|B] = Pr[X < 1 | Y > 1] 
#  (f) Compare (e) to (a) and Comment
#  (g) Compare your results to their theoretical counterparts

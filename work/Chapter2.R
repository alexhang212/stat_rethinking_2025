##just working through questions from the book

# 2E1
# (2) Pr(rain|Monday)

# 2E2
# (3) The probability that it is Monday, given that it is raining.

# 2E3
# (1) Pr(Monday|rain)

# 2E4
# There is just a possibility that there is 0.7 water?

# Working through chapter:
# 
# define grid
N = 20

p_grid <- seq( from=0 , to=1 , length.out=N )
# define prior
prior <- rep( 1 , N )
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
prior <- exp( -5*abs( p_grid - 0.5 ) )

# compute likelihood at each value in grid
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid,likelihood)

plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )

##quadratic estimation
library(rethinking)
globe.qa <- quap(
  alist(
    W ~ dbinom( W+L ,p) , # binomial likelihood
    p ~ dunif(0,1) # uniform prior
  ) ,
  data=list(W=6,L=3) )
# display summary of quadratic approximation
precis( globe.qa )

W <- 6
L <- 3
curve( dbeta( x , W+1 , L+1 ) , from=0 , to=1 )
# quadratic approximation
curve( dnorm( x , 0.67 , 0.16 ) , lty=2 , add=TRUE )


## question 2M1/ SM2:
# define grid
N = 20

p_grid <- seq( from=0 , to=1 , length.out=N )
# define prior
prior <- rep( 1 , N )
prior <- ifelse( p_grid < 0.5 , 0 , 1 )



# compute likelihood at each value in grid
##3 options:
# likelihood <- dbinom( 3 , size=3 , prob=p_grid )
# likelihood <- dbinom( 3 , size=4 , prob=p_grid )
likelihood <- dbinom( 5, size=7 , prob=p_grid )


# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )


# SM3:
#Prob land given Earth: (P (land| earth)) = 0.3
#Prob land given mars: (P(land|mars)) = 1

# Prob mars = 0.5
# Prob earth = 0.5

##Probability of earth given land (P(earth|land)) = bayes theorem: 
# (prob land given earth * prob earth )/ Pr(land)
#Pr(land) is probability hit land, so 0.5*0.3 + 0.5*1

Prob = (0.3*0.5)/(0.5*0.3+0.5)
Prob

# SM4: just count, 3 possible B, B/B and B/W. So other B is 2/3

# SM5: 4/5

#SM6: B/B, B/W, B/W, W/W, W/W, W/W
# second B is 0.5

#SM7: B/B, B/W, W/W
# first is B, second is W.


##SH1:
## Pr(twins| spA) = 0.1
## Pr(twins| spB) = 0.2
## Pr(spA) = 0.5
## Pr(spB) = 0.5

## Probability of twins in first toss is: 
0.5*0.1 + 0.5*0.2 #0.15

#probability that a species will produce 2 twins in a row:
0.5*0.1*0.1 + 0.5*0.2*0.2
#0.025

#probability of twins in sceond toss given twins in first toss
0.025/0.15
#0.17

#2H2: probability it is species A given first is twins
#Pr(spA|twins1)
#Pr(twins|spA) * Pr(SpA) / Pr(twins)
(0.1*0.5)/0.15
##0.3333

##2H3:
##Prior is Pr(A) is 0.3333
## Pr(A|Singleton) = Pr(Singleton|A) * Pr(A)/ Pr(Singleton)

(0.9*(1/3))/(0.9*0.5+0.8*0.5)

#2H4
# Pr(TestA|spA) = 0.8
# Pr(TestA|spB) = 0.35 

##Posterior probability that it is A given tested A.
# Pr(SpA|TestedA) = Pr(TestA|spA) * Pr(spA) / Pr(TestA)

(0.8*0.5)/((0.8*0.5)+0.35*0.5)

##Take into account birth stuff:
# Pr(SpA| TestedA, twins, singleton)
# just use previous prior and update it
prevprior = (0.9*(1/3))/(0.9*0.5+0.8*0.5)

(0.8*prevprior)/((0.8*0.36)+0.35*(1-0.36))
#0.55




## week 1 hw
library(rethinking)
## Suppose the globe tossing data (Lecture 2, Chapter 2) had turned out to
# be 3 water and 11 land. Construct the posterior distribution

N = 20

p_grid <- seq( from=0 , to=1 , length.out=N )
# define prior
prior <- rep( 1 , N )

# compute likelihood at each value in grid
#likelihood of land:
likelihood <- dbinom( 11 , size=14 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid , posterior , type="b" ,
      xlab="probability of land" , ylab="posterior probability" )

##Q2: Using the posterior distribution from 1, compute the posterior predictive
# distribution for the next 5 tosses of the same globe. I recommend you use
# the sampling method.

#simulate predictive distribution
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
dens(samples)

w <- rbinom( 1e4 , size=5 , prob=samples )
simplehist(w)


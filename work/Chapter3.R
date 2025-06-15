##Chapter 3 questions
rm(list=ls())
library(rethinking)

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

hist(samples)
#3E1
sum(samples<0.2)/10000

#3E2
sum(samples>0.8)/10000

#3E3
sum(samples<0.8&samples>0.2)/10000

#3E4
quantile(samples,0.2)

#3E5
quantile(samples,0.8)

#3E6
HPDI(samples,0.66)

#3E7
PI(samples,0.66)

##3M1
p_grid <- seq( from=0 , to=1 , length.out=1000 )
# prior <- rep( 1 , 1000 )
prior <- ifelse( p_grid < 0.5 , 0 , 1 )


likelihood <- dbinom( 8, size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(p_grid,posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

##3M2:
HDPI90 <- HPDI(samples,0.9)

#3M3:
w <- rbinom( 1e4 , size= 15, prob=samples )
dens(w)

#probability of getting this outcome:
sum(w==8)/1e4

simplehist(w)

##3M4
w <- rbinom( 1e4 , size= 9, prob=samples )
dens(w)

#probability of getting this outcome:
sum(w==6)/1e4

simplehist(w)

##3M5
##narrower, more informative

##3M6
PI(posterior,0.99)

##3H1

birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
            0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
            1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
            1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
            1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
            0,0,0,1,1,1,0,0,0,0)

sum(birth1) + sum(birth2) #number of boys is 111
data(homeworkch3)

#3H1
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 111 , size=200 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(p_grid,posterior)
p_grid[ which.max(posterior) ] #max posterior:

#3H2
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
HPDI50 <- HPDI(samples,0.5)
HPDI50
HPDI89 <- HPDI(samples,0.89)
HPDI89
HPDI97 <- HPDI(samples,0.97)
HPDI97

##3H3:
NumBoys <- rbinom(1e4, 200,samples)

dens(NumBoys)
abline(v=111)

##3H4
NumBoys <- rbinom(1e4, 100,samples)
sum(birth1) #birth 1 has 51 boys
dens(NumBoys)
abline(v=51) #looks off now!

#3H5:
#count number of boys after first birth is girl
# BoyafterGirl <- sum(birth1==0 & birth2 ==1) #39
AfterGirlBirth <- birth2[birth1==0]

sim <-  rbinom(1e4, length(AfterGirlBirth),samples)
dens(sim)
abline(v=BoyafterGirl)

##data is not independent, more likely to get boy after getting a girl

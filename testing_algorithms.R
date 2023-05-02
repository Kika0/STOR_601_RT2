library(tidyverse)
library(here)
source("NSGS.R")
source("KN.R")
source("s_S_inventory.R")
# testing the algorithm on (s,S) inventory system from Koenig and Law (1985)

# NSGS ----
# From Wilcox (1984) table we have h(n_0=20,k=5,1-a/2=0.975)=3.4
# alternatively, for n_0=10, h(n_0=10,...)=3.174
NSGS(a=0.05,IZ=0.1,n_0=10,h=3.174,k=5)

# perform 50 times
nsgs <- list()
for (i in 1:2) {
  nsgs[[i]] <- NSGS(a=0.05,IZ=0.1,n_0=10,h=3.174,k=5)
}

# example of (s,S) inventory system
s_S(s=20,S=40,n=100000,RandomSeed = 1)

# create dataframe of feasible solutions
i <- 1:5
s <- c(20,20,40,40,60)
S <- c(40,80,60,100,100)
E_Y <- rep(NA,5)

five_sols <- data.frame(i=i,s=s,S=S,E_Y=E_Y)
# simulate million times to get an accurate estimate E[Y] (objective value)
for (i in 1:5) {
 E_Y[i] <- mean(s_S(s=five_sols[i,2],S=five_sols[i,3],n=1000000,RandomSeed = 1))
}
five_sols <- data.frame(i=1:5,s=s,S=S,E_Y=E_Y)

# define s_S inventory function that only takes solution integer index i as input
s_S_int <- function(i,seed) {
  s_S(s=five_sols[i,2],five_sols[i,3],n=1,RandomSeed = seed)
}

# KN ----
# test KN with same parameters
KN(a=0.05,IZ=0.1,n_0=10,h=3.174,k=5)

# perform 50 times
kn <- list()
for (i in 1:50) {
  kn[[i]] <- KN(a=0.05,IZ=0.1,n_0=10,h=3.174,k=5,replication=i)
}

# print in df format
solution_index <- rep(1:5,50)
macroreplication <- rep(1:50,times=1,each=5)
for (i in 1:50) {
  optimal_solution <- 
  number_of_scenarios <- 
}
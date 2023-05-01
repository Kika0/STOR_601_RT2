library(tidyverse)
library(here)
source("NSGS.R")
source("s_S_inventory.R")
# testing the algorithm on (s,S) inventory system from Koenig and Law (1985)

# NSGS ----
# From Wilcox (1984) table we have h(n_0=20,k=5,1-a/2=0.975)=3.4
NSGS(a=0.05,IZ=0.1,n_0=20,h=3.4,k=5)

# perform 1000 times
nsgs <- c()
for (i in 1:1000) {
  nsgs[i] <- NSGS(a=0.05,IZ=0.1,n_0=20,h=3.4,k=5)
}

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
five_sols <- data.frame(i=i,s=s,S=S,E_Y=E_Y)


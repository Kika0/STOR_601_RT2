library(tidyverse)
library(here)
source("NSGS.R")
# testing the algorithm on (s,S) inventory system from Koenig and Law (1985)

# NSGS ----
# From Wilcox (1984) table we have h(n_0=20,k=5,1-a/2=0.975)=3.4
NSGS(a=0.05,IZ=0.1,n_0=20,h=3.4,k=5)

# perform 1000 times
nsgs <- c()
for (i in 1:1000) {
  nsgs[i] <- NSGS(a=0.05,IZ=0.1,n_0=20,h=3.4,k=5)
}

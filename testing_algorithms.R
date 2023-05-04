library(tidyverse)
library(here)
source("NSGS.R")
source("KN.R")
source("s_S_inventory.R")
# testing the algorithm on (s,S) inventory system from Koenig and Law (1985)

# example of (s,S) inventory system ---------
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

# NSGS ----
# From Wilcox (1984) table we have h(n_0=20,k=5,1-a/2=0.975)=3.4
# alternatively, for n_0=10, h(n_0=10,...)=3.692
NSGS(a=0.05,IZ=1,n_0=10,h=3.692,k=5)

# perform 50 times
nsgs <- list()
for (i in 1:2) {
  nsgs[[i]] <- NSGS(a=0.05,IZ=1,n_0=10,h=3.692,k=5)
}

# print in df format
solution_index <- rep(1:5,50)
macroreplication <- rep(1:50,times=1,each=5)
optimal_solution <- c()
number_of_replications <- c()
is_in_subset <- c()
for (i in 1:50) {
  nsgs <- NSGS(a=0.05,IZ=1,n_0=10, h=3.692, k=5, replication=i)
  optimal_solution[(5*(i-1)+1):(5*i)] <- rep(nsgs[[1]],5)
  number_of_replications[(5*(i-1)+1):(5*i)] <- nsgs[[2]]
  is_in_subset[(5*(i-1)+1):(5*i)] <- nsgs[[3]]
}

nsgs_solutions <- data.frame(macroreplication = macroreplication, solution_index=solution_index, 
                           optimal_solution = optimal_solution,number_of_replications = number_of_replications,
                           is_in_subset = is_in_subset) %>% 
  mutate(solution_index = factor(as.character(solution_index),levels = c("1","2","3","4","5")))
nsgs_solutions %>% view()

# compare Y for macroreplications to show CRN
data.frame(K_Y,N_Y) %>% stargazer(summary = FALSE)
# KN ----
# test KN with the same parameters
KN(a=0.05,IZ=1,n_0=10,k=5)

# perform 50 times
kn <- list()
for (i in 1:50) {
  kn[[i]] <- KN(a=0.05,IZ=0.1,n_0=10,k=5,replication=i)
}

# print in df format
solution_index <- rep(1:5,50)
macroreplication <- rep(1:50,times=1,each=5)
optimal_solution <- c()
number_of_replications <- c()
for (i in 1:50) {
  kn <- KN(a=0.05,IZ=1,n_0=10,k=5, replication=i)
  optimal_solution[(5*(i-1)+1):(5*i)] <- rep(kn[[1]],5)
  number_of_replications[(5*(i-1)+1):(5*i)] <- kn[[2]]
}

kn_solutions <- data.frame(macroreplication = macroreplication, solution_index=solution_index, 
                           optimal_solution = optimal_solution,number_of_replications = number_of_replications) %>% 
  mutate(solution_index = factor(as.character(solution_index),levels = c("1","2","3","4","5")))
kn_solutions %>% view()

# Boxplots of how many simulation
# NSGS
ggplot(nsgs_solutions,aes(x=solution_index,y=number_of_replications)) + geom_boxplot()

# KN
ggplot(kn_solutions,aes(x=solution_index,y=number_of_replications)) + geom_boxplot()

# put together in one plot
together <- nsgs_solutions %>% rbind((kn_solutions %>% mutate(is_in_subset=rep(NA,250))) )
Procedure <- c(rep("NSGS",250),rep("KN",250))
together <- together %>% mutate(Procedure=factor(Procedure,levels=c("NSGS","KN")))
ggplot(together,aes(x=solution_index,y=number_of_replications,fill=Procedure)) + geom_boxplot() +
  scale_fill_manual(values=c("#C11432","#009ADA")) + xlab("Solution index") + ylab("Number of replications") +
  theme_minimal()

nsgs_solutions %>% filter(solution_index==1) %>% pull(number_of_replications) %>% mean()
nsgs_solutions %>% filter(solution_index==2) %>% pull(number_of_replications) %>% mean()
kn_solutions %>% filter(solution_index==1) %>% pull(number_of_replications) %>% mean()
kn_solutions %>% filter(solution_index==2) %>% pull(number_of_replications) %>% mean()
# (s,S) second example of inventory system ----
i <- 1:4950
s <- c()
S <- c()
for (j in 1:99) {
  s <- c(s,rep(j,(100-j)))
  S <- c(S,(j+1):100)
}
E_Y <- rep(NA,4950)

many_sols <- data.frame(i=i,s=s,S=S,E_Y=E_Y)
# simulate million times to get an accurate estimate E[Y] (objective value) 
for (i in 1:4950) {
  E_Y[i] <- mean(s_S(s=many_sols[i,2],S=many_sols[i,3],n=10000,RandomSeed = 1))
}
many_sols <- data.frame(i=1:4950,s=s,S=S,E_Y=E_Y)
many_sols %>% arrange(E_Y) %>%  view()
# save file to save future computation (took 2 hours to run E_Y with 10^4 for each of 4950 solutions)
#write_rds(many_sols,"s_S_fesible_solutions_with_expected_values.rds")

# run 4950 feasible solutions KN algorithm ----
# define s_S inventory function that only takes solution integer index i as input
# WARNING: unlikely to run due to computational cost of 4950^2
s_S_int <- function(i,seed) {
  s_S(s=many_sols[i,2],many_sols[i,3],n=1,RandomSeed = seed)
}
# test KN with the same parameters
KN(a=0.05,IZ=1,n_0=10,k=4950)

# alternative approach
few_sols <- many_sols %>% filter((s %% 10)==0) %>% filter((S %% 10)==0) 
s_S_int <- function(i,seed) {
  s_S(s=few_sols[i,2],few_sols[i,3],n=1,RandomSeed = seed)
}
# test KN with the same parameters
KN(a=0.05,IZ=1,n_0=10,k=45)
few_sols %>% arrange(E_Y) %>% view()

# second stage local search
few_more_sols <- many_sols %>% filter((s %% 2)==0) %>% filter((S %% 2)==0) %>% filter(s >= 12 & s <=28) %>% 
  filter(S>=42 & S<=58)

s_S_int <- function(i,seed) {
  s_S(s=few_more_sols[i,2],few_more_sols[i,3],n=1,RandomSeed = seed)
}
# test KN with the same parameters
KN(a=0.05,IZ=0.1,n_0=10,k=45)

# print in df format
solution_index <- rep(1:45,5)
macroreplication <- rep(1:5,times=1,each=45)
optimal_solution <- c()
number_of_replications <- c()
for (i in 1:5) {
  kn <- KN(a=0.05,IZ=1,n_0=10,k=45, replication=i)
  optimal_solution[(45*(i-1)+1):(45*i)] <- rep(kn[[1]],5)
  number_of_replications[(45*(i-1)+1):(45*i)] <- kn[[2]]
}

kn_many_solutions <- data.frame(macroreplication = macroreplication, solution_index=solution_index, 
                           optimal_solution = optimal_solution,number_of_replications = number_of_replications) %>% 
  mutate(solution_index = factor(as.character(solution_index))) %>% mutate(top=factor(kn_many_solutions$solution_index==12))
kn_many_solutions %>% view()

# plot
 
ggplot(kn_many_solutions,aes(x=solution_index,y=number_of_replications)) + geom_boxplot(aes(fill=top)) +
  scale_fill_manual(values=c("#C11432")) + xlab("Solution index") + ylab("Number of replications") +
  theme_minimal()+theme(axis.text.x = element_blank(),legend.position = "none") +
  scale_fill_manual(kn_many_solutions$top,values = c('#009ADA','#66A64F'))
kn_many_solutions %>% group_by(solution_index) %>% summarise(n=mean(number_of_replications)) %>% arrange(desc(n)) %>% view()
# nsgs_solutions %>% filter(solution_index==1) %>% pull(number_of_replications) %>% mean()
# nsgs_solutions %>% filter(solution_index==2) %>% pull(number_of_replications) %>% mean()
# kn_solutions %>% filter(solution_index==1) %>% pull(number_of_replications) %>% mean()
# kn_solutions %>% filter(solution_index==2) %>% pull(number_of_replications) %>% mean()
# -------

kn_many_solutions %>% group_by(optimal_solution) %>% count() %>% mutate(n=n/45) %>%
  left_join(few_more_sols %>% rowid_to_column(),by=c("optimal_solution"="rowid")) %>% 
  arrange(desc(n))

# save as latex table
library(stargazer)
table_kn <- kn_many_solutions %>% group_by(optimal_solution) %>% count() %>% mutate(n=n/45) %>%
  left_join(few_more_sols %>% rowid_to_column(),by=c("optimal_solution"="rowid")) %>% 
  arrange(desc(n)) 
stargazer(table_kn %>% mutate(E_Y=round(E_Y,3)) %>% as.data.frame(),summary = FALSE,          type='latex',
          header = F,
          title="")


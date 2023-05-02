# KN algorithm

KN <- function(a,IZ,n_0,h,k) {

  # initiate placeholder for Y_i(x_i)
  Y <- data.frame(matrix(ncol=k,nrow = 0))
  
  # initiate placeholder for mean
  Y_bar <- c()
  # initiate placeholder for S_2[i,j]
  S_2 <- data.frame(matrix(ncol = k,nrow = 0))
  # initiate placeholder for W[i,j]
  W <- data.frame(matrix(ncol = k,nrow = 0))  
  # initiate subset I with integer index of all solutions
  I <- 1:k
  # initiate placeholder for the total simulations N[i]
  N <- c()
  # define \eta
  eta <- 1/2* (((2*a)/(k-1))^((-2)/(n_0-1))  -1)
  # define h^2
  h_2 <- 2*eta*(n_0-1)
  
 # -----------------------------------------------------------
  # simulate each feasible solution n_0 times 
  for (i in I) {
    Y[1:n_0,i] <- rnorm(n_0,mean=0.1*i,sd=1)
    # calculate the sample mean
    Y_bar[i] <- mean(Y[1:n_0,i])
  }
  
  # for each pair i not equal j, calculate S_2[i,j]
  for (i in 1:k) {
    for (j in 1:k) {
      S_2[i,j] <- 1/(n_0-1)*sum((Y[,i]-Y[,j] - (Y_bar[i]-Y_bar[j]) )^2)
    }
    # replace case when i=j with NA
    S_2[i,i] <- NA
  }
  # set initial value for r
  r <- n_0
#  ------------------------------------------------------------
  
  while(length(I) >= 2) {
  
    # Update I_old
    I_old <- I
    # for each pair i not equal j in I_old, calculate W[i,j]
    for (i in I_old) {
      for (j in I_old) {
        W[i,j] <- max(0,( IZ/(2*r)*  (  (h_2*S_2[i,j])/(IZ^2) ) -r  ))
      }
      # replace case when i=j with NA
      W[i,i] <- NA
    }
    # empty placeholder I_p
    I_p <- c()
    # create subset I
    for(i in I_old) {
      if (sum(Y_bar[i] <= (Y_bar + W[i,]),na.rm = TRUE)==(k-1)) {
        I_p <- c(I,i)
      }
    }
    
    # update I
    I <- I_p
  
  for (i in I) {
      # simulate x_i once
      Y[nrow(Y)+1,i] <- rnorm(n=1,mean=0.1*i,sd=1)
      # calculate the sample mean
      Y_bar[i] <- mean(Y[1:nrow(Y),i])
    }
  # increment r
    r <- r+1
    
  } # end while loop
  
  # count number of simulations
  number_of_simulations <- c()
  for (i in 1:k) {
    number_of_simulations[i] <- sum(!is.na(Y[,i]))
  }
  return(list(optimal_solution=which.min(Y_bar),number_of_simulations=number_of_simulations))
}
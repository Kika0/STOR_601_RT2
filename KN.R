# KN algorithm

KN <- function(a,IZ,n_0,h,k) {

  # initiate placeholder for Y_i(x_i)
  Y <- data.frame(matrix(ncol=k,nrow = 0))
  # initiate placeholder for mean
  Y_bar <- c()
  # initiate placeholder for S_2[i,j]
  S_2 <- data.frame(matrix(ncol = k,nrow = 0))
  # initiate subset I with integer index of all solutions
  I <- 1:k
  # initiate placeholder for the total simulations N[i]
  N <- c()
  # define \eta
  eta <- 1/2* (((2*\alpha)/(k-1))^((-2)/(n_0-1))  -1)
  # define h^2
  h_2 <- 2*eta/(n_0-1)
  
  -----------------------------------------------------------
  # simulate each feasible solution n_0 times 
  for (i in 1:k) {
    Y[1:n_0,i] <- rnorm(n_0,mean=0.1*i,sd=1)
    # calculate the sample mean
    Y_bar[i] <- mean(Y[1:n_0,i])
  }
  
  # for each pair i not equal j, calculate W[i,j]
  for (i in 1:k) {
    for (j in 1:k) {
      S_2[i,j] <- 1/(n_0-1)*sum((Y[,i]-Y[,j] - (Y_bar[i]-Y_bar[j]) )^2)
    }
    # replace case when i=j with NA
    S_2[i,i] <- NA
  }
  # set initial value for r
  r <- n_0
  ------------------------------------------------------------
  
  while(length(I) >= 2) {
  # create subset I
  for(i in 1:k) {
    I[i] <- sum(Y_bar[i] <= (Y_bar + W[i,]),na.rm = TRUE)==(k-1)
  }
  
  if (sum(I)==1)
  {
    return(which(I==TRUE))
  }
  
  for (i in 1:k) {
    if (I[i]==TRUE) {
      # calculate number of simulations N[i]
      N[i] <- max(n_0,ceiling((h/IZ)^2*s_2[i]))
      # simulate (N_i-n_0)-times
      Y[(n_0+1):N[i],i] <- rnorm(n=(N[i]-n_0),mean=0.1*i,sd=1)
      # calculate the sample mean
      Y_bar[i] <- mean(Y[1:N[i],i])
    }
    else
    {N[i] <- n_0}
  }
  } # end while loop
  return(list(optimal_solution=which.min(Y_bar),number_of_simulations=N,is_in_subset=I))
}
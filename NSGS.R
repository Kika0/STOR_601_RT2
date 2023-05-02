# NSGS algorithm

NSGS <- function(a,IZ,n_0,h,k) {
  t <- qt(c((1-a/2)^(1/(k-1))), df=n_0-1)
# initiate placeholder for Y_i(x_i)
  Y <- data.frame(matrix(ncol=k,nrow = 0))
# initiate placeholder for mean
  Y_bar <- c()
# initiate placeholder for marginal sample variance
  s_2 <- c()
# initiate placeholder for W[i,j]
  W <- data.frame(matrix(ncol = k,nrow = 0))
# initiate placeholder for I (boolean to show x_i is in subset)
  I <- c()
# initiate placeholder for the total simulations N[i]
  N <- c()
  
  # simulate each feasible solution n_0 times 
  for (i in 1:k) {
    Y[1:n_0,i] <- rnorm(n_0,mean=0.1*i,sd=1)
    # calculate the sample mean
    Y_bar[i] <- mean(Y[1:n_0,i])
    # calculate the marginal sample variance 
    s_2[i] <-  1/(n_0-1)*sum((Y[,i]-Y_bar[i])^2)
  }
  
  # for each pair i not equal j, calculate W[i,j]
  for (i in 1:k) {
    for (j in 1:k) {
      W[i,j] <- t*sqrt((s_2[i]+s_2[j])/n_0)
    }
    # replace case when i=j with NA
    W[i,i] <- NA
  }
  
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
  return(list(optimal_solution=which.min(Y_bar),number_of_simulations=N,is_in_subset=I))
}
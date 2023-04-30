# NSGS algorithm

NSGS <- function(a,IZ,n_0,h,k) {
  t <- qt(c((1-a/2)^(1/(k-1))), df=n_0-1)
# initiate placeholder for Y_i(x_i)
  Y <- data.frame(matrix(ncol=k,nrow = 0))
  for (i in 1:k) {
    colnames(Y)[i] <- paste0("x",i)
  }
# initiate placeholder for mean
  Y_bar <- c()
# initiate placeholder for marginal smaple variance
  s_2 <- c()
  
  # simulate each feasible solution n_0 times 
  for (i in 1:k) {
    Y[1:n_0,i] <- rnorm(n_0,mean=i,sd=1)
    # calculate the sample mean
    Y_bar[i] <- mean(Y[1:n_0,i])
    # calculate the marginal sample variance 
    s_2[i] <-  1/(n_0-1)*sum((Y[,i]-Y_bar[i])^2)
  }
  
  # for each pair i not equal j, calculate W[i,j]
  for 
}
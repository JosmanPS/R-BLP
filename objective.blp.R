objective.blp <- function(thet, delta){
  
  # INPUT
  # delta : current value of delta vector
  # thet : current value of thet2 vector (from outer loop)
  
  # OUTPUT
  # thet1 : a vector with the value of the linear part of theta, with the current delta and thet2
  # error : the prediction error with thet1
  # GMMvalue : the value of the GMM objective function
  
  #Compute the lienar part of theta
  thet1 <- solve(t(x1) %*% Z %*% solve(phi) %*% t(Z) %*% x1) %*% t(x1) %*% Z %*% solve(phi) %*% t(Z) %*% delta
  
  #Compute the prediction error
  mean.ut <- x1 %*% thet1
  error <- delta - mean.ut
  
  #Compute the value of the GMM objective
  GMM <- t(error) %*% Z %*% solve(phi) %*% t(Z) %*% error
  
  list(thet1 = thet1, mean.ut = mean.ut, error = error, GMMvalue = GMM)
  
}
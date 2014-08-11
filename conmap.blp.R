conmap.blp <- function(thet, x.start = rep(0, nbrn*nmkt), tol.in = 1e-01, max.it = 1500) {
  #Probar para distintos valores de tol.in
  
  # Implements the BLP (1995) contraction mapping
  
  # INPUT
  # thet : current value of thet2
  # x.start : J*T zero vector as start value for delta (default); other
  # option is to use delta.0 (analytical delta)
  # tol.in : inner-loop convergence criterion
  # max.it : max. number of iterations consistent with SQUAREM and DF-SANE
  
  # OUTPUT
  # delta : fixed point (FP)
  # iter : number of FP iterations
  # convergence : 0 if iter < max.it, 1 otherwise
  
  theta.in <- thet
  delta.in <- x.start
  # Define local variables:
  obs.s <- S_jt
  it <- 0
  # Start BLP (1995) loop:
  repeat {
    cat("BLP iteration", it, "\n")
    pred.s <- predict.blp(theta.in, delta.in)[[2]]
    delta <- delta.in + log(obs.s) - log(pred.s)
    if (sqrt(crossprod(delta - delta.in)/length(delta.in)) < tol.in) {
      break
    }
    delta.in <- delta
    it <- it + 1
    if (it > max.it) break
  }
  list(delta = delta, iter = it, convergence = as.numeric(it > max.it))
  
  
}
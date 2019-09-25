data= iris

X <- model.matrix(formula, data)
y <- as.matrix(data[all.vars(data)])


QR.regression <- function(y, X)
  {
  nr <- length(y)
  nc <- NCOL(X)
  
  # Householder transformations
  for (j in seq_len(nc))
  {
    id <- seq.int(j, nr)
    sigma <- sum(X[id,j]^2)
    s <- sqrt(sigma)
    diag_ej <- X[j,j]
    gamma <- 1.0 / (sigma + abs(s * diag_ej))
    kappa <- if (diag_ej < 0) s else -s
    X[j,j] <- X[j,j] - kappa
    if (j < nc)
      for (k in seq.int(j+1, nc))
      {
        yPrime <- sum(X[id,j] * X[id,k]) * gamma
        X[id,k] <- X[id,k] - X[id,j] * yPrime
      }
    
    yPrime <- sum(X[id,j] * y[id]) * gamma
    y[id] <- y[id] - X[id,j] * yPrime
    
    X[j,j] <- kappa
    
  } 
  # end Householder
  
  # residual sum of squares
  rss <- sum(y[seq.int(nc+1, nr)]^2)
  
  # Backsolve
  beta <- rep(NA, nc)
  for (j in seq.int(nc, 1))
  {
    beta[j] <- y[j]
    if (j < nc)
      for (i in seq.int(j+1, nc))
        beta[j] <- beta[j] - X[j,i] * beta[i]
      beta[j] <- beta[j] / X[j,j]
  }
  
  # set zeros in the lower triangular side of X (which stores) 
  # not really necessary, this is just to return R for illustration
  for (i in seq_len(ncol(X)))
    X[seq.int(i+1, nr),i] <- 0
  
  list(R=X[1:nc,1:nc], y=y, beta=beta, rss=rss)
}

pre_posterior <- function(x, n, size, px=FALSE) {
  
  eta <- sum(x == 0) / length(x)
  
  if(max(x) > 0) {
    if(var(x[x != 0])) shape <- mmbeta(x[x != 0]) else shape <- NULL
  }
  
  p <- unname(sample(x, n, replace=TRUE))
  
  update <- function(phi, x, size, eta, shape) {
    m <- rbinom(1, size, phi)
    if(max(x) > 0) {
      if(m < 1 && eta) {
        x.star <- unname(x)
        if(diff(range(x)) == 1) x.star <- rep(0, length(x))
        if(var(x[x != 0])) { 
          x.star[x.star != 0] <- rbeta(length(x) - sum(x == 0), shape[1], 
            shape[2] + size)
        }  
      } else {
        if(diff(range(x)) == 1) x.star <- rep(1, length(x))
        if(var(x[x != 0])) {
          x.star <- unname(rbeta(length(x), shape[1] + m, shape[2] + (size - m)))
        } else {
          x.star <- rep(x[x != 0], length.out=length(x))
        }  
      }
    } else {x.star <- rep(0, length(x))}  
    x.star <- sample(x.star)
    attr(x.star, 'm') <- m
    return(x.star)
  }
  
  if (px) {
    ans <- lapply(p, 
      function(y) rep(y, n))
  } else {
    ans <- lapply(p, 
      function(y) update(phi=y, x=x, size=size, eta=eta, shape=shape))
  }  
  
  attr(ans, 'probability') <- p
  
  return(ans)
  
}


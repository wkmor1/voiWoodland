mmbeta <- function(x) {
  m <- mean(x)
  v <- var(x)
  aOm <- ((m * (1 - m)) / v) - 1
  a <- m * aOm
  b <- (1 - m) * aOm
  return(c(a, b))
}
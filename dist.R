cosine <- function(x) {
  y <- x %*% t(x)
  mag <- diag(y)
  res <- 1 - y / mag
  return(res)
}
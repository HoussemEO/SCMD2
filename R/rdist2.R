#' simulation de CM n fois
#' @export
#' @param x numerical vector
#' @param p numerical vector
#' @param n int
rdist2<-function(x,p,n)
{
  t <- c(1:n)
  for (j in 1:n) {
    y=rdist(x,p)
    t[j] <- y

  }
  return(t)
}

#' STAT DiSC
#' @export
#' @param x numerical vector
STATD<-function(x)
{
  par(mfrow= c(1,2))
  hist(x, col = rainbow(30))
  boxplot(x, col = 'red' )
  par(mfrow = c(1, 1))
  data.frame( min = min(x),
              max = max(x),
              median = median(x),
              mean = mean(x))
}

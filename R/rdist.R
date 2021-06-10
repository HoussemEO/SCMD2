#' simulation de CM une seule fois
#' @export
#' @param x numerical vector
#' @param p numerical vector
rdist<-function(x,p)
{
  n=length(p)
  r=runif(1)
  b=p[1];
  if((r>=0) & (r<=b))
  {
    y=x[1]
    return(y)
  }
  else
  {
    a=p[1]
    b=b+p[2]
    for(i in 2:n)
    {
      if((r>=a) & (r <=b))
      {
        y=x[i]
        return(y)
      }
      else
      {
        a=b
        b=b+p[i+1]
        i=i+1
      }
    }
  }
}

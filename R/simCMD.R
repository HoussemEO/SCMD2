#' simulation d'une CM à temps discret
#' @export
#' @param x numerical vector
#' @param ei numerical vector
#' @param Pt numerical matrix
#' @param n integer
simCMD<-function(x,ei,Pt,n)
{
  y<-c(rep(0,n+1))
  t<-c(0:n)
  y[1]<-rdist(x,ei)
for(i in 1:n){
  y[i+1]<-rdist(x,Pt[y[i],])
}
  par(mfrow = c(2,1))
      plot(t,y,type = "p",main = "graphe des états pour tous temps de 0 à n" ,pch=5,xlim=c(0,n),ylim=c(0,length(ei)+1),xlab = "temps" ,ylab = "états",col="red")
plot(t,y,type = "s",main = "graphe des états pour tous temps de 0 à n" ,pch=5,xlim=c(0,n),ylim=c(0,length(ei)+1),xlab = "temps" ,ylab = "états",col="blue")
return(y)
}




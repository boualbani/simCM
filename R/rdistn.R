rdistn <- function ( x , p , n )
{
  p=rdistn(x,p,n)

  plot(p,col="yellow",main="le nuage de point")
  hist( x , col = rainbow( 10 ))
  boxplot( x , col = "green" )
  par( mfrow = c( 2 , 2 ))
  data.frame (moy=mean(p),med=median(p),var=var(p),ecart=sd(p),max=max(p),min=min(p))
}

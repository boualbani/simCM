# 'chaine de markov a temps discret
# '@export
# '@param z vecteur numerique representant les ?tapes
# '@param mu vecteur numerique la distribution initiale
# '@param p matrice de transition
# '@param n nombre numerique representant le nombre de pas

Simr  <- function ( z , mu , P , n )
{
  X  <- c(rep( 0 , n  +  1 ))
  t  <- c( 0  :  n )
  X [ 1 ] <- rdist( z , mu )

  for ( i   in   1  :  n ){
    X [ i  +  1 ] <- rdist( z , P [ X [ i ],])
  }

  plot( t , X  )
  par( mfrow   = c( 2 , 1 ))
  return ( X )
}





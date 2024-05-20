pb1 <- function(valori, probabilitati) 
{
  
  if(any(probabilitati<0) || sum(probabilitati)!=1)
  {
    stop("Probabilitati negative sau suma tuturor probabilitatilor nu este 1")
  }
  
  if(length(valori) != length(probabilitati)) 
  {
    stop("Numar inegal de valori si de probabilitati.")
  }
  
  nr <- runif(1)
  probabil_cumulate <- cumsum(probabilitati)
  index <- match(TRUE, nr <= probabil_cumulate)
  
  return(valori[index])
}

valori <- c(23, 55, 43, 12, 1, 44)
probabilitati <- c(0.1, 0.2, 0.2, 0.05, 0.4, 0.05)
valoare_finala <- pb1(valori, probabilitati)
print(valoare_finala)
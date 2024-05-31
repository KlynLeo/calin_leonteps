#1 a)
calcul_probabilitati <- function(distributie, parametri, m) {
  k <- 0:m
  if(distributie == "Poisson") {
    probabilitati <- dpois(k, parametri)
  } 
  else if(distributie == "Geometric") {
    probabilitati <- dgeom(k, parametri)
  } 
  else if(distributie == "B") {
    probabilitati <- dbinom(k, parametri[1], parametri[2])
  }
  return(probabilitati)
}

#exemplu de utilizare
prob_poisson <- calcul_probabilitati("Poisson", 2, 10)
prob_geometric <- calcul_probabilitati("Geometric", 0.3, 10)
prob_binomial <- calcul_probabilitati("B", c(5, 0.4), 10)

print(prob_poisson)
print(prob_geometric)
print(prob_binomial)

#b)
plot_probability<- function(x, pmf, title="Probabilitatea functiilor de masa")
  
{
  plot(x, pmf, type="h", lwd=2, xlab="Valori", ylab="Probabilitati", main=title)
}

#exemplu de utilizare
x <- c(1, 2, 3, 4, 5)
pmf <- c(0.1, 0.2, 0.3, 0.2, 0.2)

plot_probability(x, pmf, title="Exemplu")


#c) 
PoissonK0 <- function(lambda) {
  k0 <- 0
  p <- dpois(k0, lambda)
  
  while(p <= 1 - 0.000001) {
    k0 <- k0 + 1
    p <- sum(dpois(0:k0, lambda)) 
  }
  
  return(k0)
}

#exemplu de utilizare
lambda <- 2
k0 <- PoissonK0(lambda)
print(k0)

#2 a)
Functie_a <- function(fisier) {
  date <- read.csv(fisier)
  esantionP <- date$P
  esantionS <- date$S

  frec_abs1 <- as.vector(table(esantionP))
  frec_abs2 <- as.vector(table(esantionS))
  
  frec_rel1 <- as.vector(table(esantionP) / length(esantionP))
  frec_rel2 <- as.vector(table(esantionS) / length(esantionS))
  
  media1 <- mean(esantionP)
  media2 <- mean(esantionS)
  

  rezultate <- list(
    "Frecvente absolute esantion Probabilitati" = frec_abs1,
    "Frecvente absolute esantion Statistica" = frec_abs2,
    "Frecvente relative esantion Probabilitati" = frec_rel1,
    "Frecvente relative esantion Statistica" = frec_rel2,
    "Media esantion Probabilitati" = media1,
    "Media esantion Statistica" = media2
  )
  
  return(rezultate)
}

# Exemplu de utilizare
rezultate <- Functie_a("note_PS.csv")
print(rezultate)


#A2 b)
Functie_b=function(fisier,e)
{
  date=read.csv(fisier,header=TRUE)
  esantion=date[[e]]
  
  q1=quantile(esantion,0.25)
  q3=quantile(esantion,0.75)
  iqr=q3-q1
  
  lim_inf=q1-1.5*iqr
  lim_sup=q3+1.5*iqr
  
  newesantion=esantion[esantion>=lim_inf & esantion<=lim_sup]
  cat("Eșantionul curățat:", newesantion, "\n")
  
  hist(newesantion,breaks=seq(1,10,by=1),xlab=e,ylab="Frecventa")
}
Functie_b("note_PS.csv","P")
#se foloseste metoda cuartilelor (IQR)

#1

parabola_area <- function() {
  f <- function(x) -2*x^2 + 5*x - 2
  a <- 0
  b <- 2
  supr_ex <- integrate(f, a, b)$value
  n <- 10000
  x <- runif(n, a, b)
  y <- runif(n, 0, max(f(x)))
  inside <- y <= f(x)
  prop_inside <- mean(inside)
  supr_aprox <- (b - a) * max(f(x)) * prop_inside
  eroare <- abs(supr_ex - supr_aprox) / supr_ex
  result <- list(supr_aprox = supr_aprox, supr_ex = supr_ex, eroare = eroare)
  return(result)
}

result <- parabola_area()
cat("Aria estimata:", format(result$supr_aprox, digits = 6), "\n")
cat("Aria exacta obtinuta prin integrare:", format(result$supr_ex, digits = 6), "\n")
cat("Eroarea relativa:", format(result$eroare, digits = 2), "%\n")


#2 1 b) 

f <- function(x) exp(x)
a <- 1
b <- 4
exact <- integrate(f, a, b)$value
n <- 10000
x <- seq(a, b, length.out = n+1)
y <- f(x)
approx <- (b-a)/(2*n) * (sum(y) - y[1] - y[n+1] + 2 * sum(y[2:n]))
abs_error <- abs(exact - approx)
rel_error <- abs_error / exact * 100
cat("Valoarea exacta: ", exact, "\n")
cat("Valoarea aproximativa: ", approx, "\n")
cat("Eroare absoluta: ", abs_error, "\n")
cat("Eroare relativa: ", rel_error, "%\n")

# d) 

monte_carlo_integral <- function() {
  f <- function(x) 1 / (4*x^2 - 1)
  a <- 1
  b <- Inf
  exact_value <- log(3/4)
  n <- 100000
  x <- runif(n, a, b)
  integral <- (b - a) * mean(f(x))
  abs_error <- abs(integral - exact_value)
  rel_error <- abs_error / exact_value
  result <- list(integral = integral, exact_value = exact_value, abs_error = abs_error, rel_error = rel_error)
  return(result)
}
result <- monte_carlo_integral()
cat("Valoare estimata: ", format(result$integral, digits = 8), "\n")
cat("Valoare exacta: ", format(result$exact_value, digits = 8), "\n")
cat("Eroare absoluta: ", format(result$abs_error, digits = 8), "\n")
cat("Eroare relativa: ", format(result$rel_error, digits = 2), "%\n")


#2 2
f <- function(u) {
  exp(-2*u^2)
}

lambda <- 3
N <- 50000
x <- rexp(N, lambda)
aprox <- sum(f(x))/(lambda*N)
exact <- sqrt(pi)/8
eroare <- abs(aprox - exact)
cat("Estimate:", aprox, "\n")
cat("True value:", exact, "\n")
cat("Error:", eroare, "\n")


#3

mecanic1 <- 3/4
mecanic2 <- 1/4
lambda1 <- 12
lambda2 <- 4
N <- 10000
suma <- 0
for (i in 1:N) {
  x <- mecanic1*rexp(1, lambda1) + mecanic2*rexp(1, lambda2)
  suma <- suma + x
}
media <- suma/N
cat("Media estimata:", media, "\n")


#4
n <- 100000
x <- rgeom(n, 0.3)
y <- rgeom(n, 0.5)
p <- mean(x > y^2)
n <- 100
error <- 1
while (eroare > 0.005) {
  x <- c(x, rgeom(n, 0.3))
  y <- c(y, rgeom(n, 0.5))
  p2 <- mean(x > y^2)
  eroare <- qnorm(0.975)*sqrt(p2*(1-p2)/length(x))
  n <- n + 100 
}
cat("Probabilitatea estimata: ", p, "\n")
cat("Numar de rulari: ", length(x), "\n")

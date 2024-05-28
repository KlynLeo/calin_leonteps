#II6.

zconf <- function(fisier, alfa) 
{
  x=scan(fisier);
  xn=mean(x);
  n=length(x);
  sigma=sqrt(9);
  critical_z=qnorm((1-alfa/2), 0, 1);
  a=xn-critical_z*sigma/sqrt(n);
  b=xn+critical_z*sigma/sqrt(n);
  interval=c(a, b);
  return(interval);
}

zconf("history.txt", 0.05);

#III4.
sample_data <- scan("history.txt")
sample_mean <- mean(sample_data)
sample_sd <- sd(sample_data)
n <- length(sample_data)
se <- sample_sd / sqrt(n)

calculate <- function(sample_mean, se, alfa, n) {
  critical_t <- qt(1 - alfa / 2, n - 1)
  a <- sample_mean - critical_t * se
  b <- sample_mean + critical_t * se
  interval <- c(a, b)
  return(interval)
}

alfa_95 <- 0.05
interval_95 <- calculate(sample_mean, se, alfa_95, n)

alfa_99 <- 0.01
interval_99 <- calculate(sample_mean, se, alfa_99, n)

list(
  "95% Confidence Interval" = interval_95,
  "99% Confidence Interval" = interval_99
)

#IV2.

n <- 150
X <- 20
p0 <- 0.10
alpha <- 0.05
phat <- X / n

z <- (phat - p0) / sqrt(p0 * (1 - p0) / n)
z_critical <- qnorm(1 - alpha)
reject_H0 <- z > z_critical

list(
  "Proporția observată" = phat,
  "Statistica z" = z,
  "Valoarea critică" = z_critical,
  "Respingerea ipotezei nule" = reject_H0
)


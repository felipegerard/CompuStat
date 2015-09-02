
fun <-  function(x){
  n <- length(x)
  (2*pi)^(-n/2)*exp(-0.5*sum(x*x))
}

int_trap <- function(f, x){
  N <- length(x)
  (x[N] - x[1])*mean(f)
}

int_trap_mult <- function(f, a, b, N=20){
  n <- length(a)
  x <- seq(a[1], b[1], l=N)
  if(n <= 1){
    fx <- sapply(x, f)
  } else{
    fx <- numeric(N)
    for(i in 1:length(x)){
      g <- function(y){
        f(c(x[i], y))
      }
      fx[i] <- int_trap_mult(g, a[-1], b[-1], N=N)
    }
  }
#   print(x)
#   print(fx)
  I <- int_trap(fx, x)
  return(I)
}

# Veamos si coincide
n <- 2 # Dimensión de la normal multivariada
int_trap_mult(fun, rep(-2,n), rep(2,n), N=1100)
library(pracma)
(erf(sqrt(2)))^n

u <- seq(-5,5,l=50)
fu <- sapply(u, fun)
qplot(u,fu, geom='line')
int_trap(fu, u)


### Ahora MonteCarlo

# Indicadora de todas las variables en [-2,2]
phi <- function(x){
  as.numeric(all(x >= -2 & x <= 2))
}

n <- 2 # Dimensión de la normal multivariada
N <- 1000 # Número de muestras
phix <- numeric(N)
for(i in 1:N){
  x <- rnorm(n, 0, sqrt(1))
  phix[i] <- phi(x)
}
I <- mean(phix)
I

# Intervalos de confianza
alpha <- 0.05
s <- sd(phix)
lower <- I - s/sqrt(N)*qnorm(1 - alpha/2, 0, 1)
upper <- I + s/sqrt(N)*qnorm(1 - alpha/2, 0, 1)
I
lower
upper




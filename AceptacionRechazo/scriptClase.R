f <- function(x){
  (2/sqrt(2*pi))*exp(-x^2/2)
}
g <- function(x) exp(-x)

plot(f, ylim=c(0,2), xlim=c(0,5))
plot(g, add=TRUE, col='red', ylim=c(0,2), xlim=c(0,5))

h <- function(x){
  f(x)/g(x)
}
plot(h, xlim=c(0.1,4), ylim=c(0,2))

M <- 2

# Aceptación y rechazo para generar
genera_una <- function(...){
  maxiter <- 10000
  iter <- 1
  while(TRUE & iter < maxiter){
    # 1) Exponencial(lambda = 1)
    Y <- rexp(1, 1)
    U <- runif(1)
    # 2) Aceptar o rechazar
    if(U <= f(Y)/(M*g(Y))){
      X <- Y # Z es el valor absoluto de una normal
      break
    }
    iter <- iter + 1
  }
  X
}

genera_muchas <- function(n = 1){
  sapply(1:n, genera_una)
}

hist(genera_muchas(10000))
plot(f, ylim=c(0,2), xlim=c(0,5))


genera_normales <- function(n = 1){
  sample(c(-1,1), size = n, replace = T)*genera_muchas(n = n)
}

hist(genera_normales(10000))


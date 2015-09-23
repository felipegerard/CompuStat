
# Quiero integral de 0 a 2 de N(0,1)

real <- pnorm(2) - 1/2
real

n <- 100
N <- 10000
estim_mc <- rep(0,N)
estim_is <- rep(0,N)
for(i in 1:N){
  ### MonteCarlo crudo
  U <- runif(n, 0, 2) # Ojo: de 0 a 2
  phi <- function(x) 2*dnorm(x)
  estim_mc[i] <- mean(phi(U))
  
  ### Una mejor g: Importance Sampling
#   plot(phi, xlim=c(0,2))
  
  # Exponencial (lambda = 1) truncada a [0,2] --> Usar Método de la Función Inversa
  U <- runif(n, 0, 1) # Ojo: de 0 a 1
  X <- -log(1 - (1 - exp(-2))*U)
#   qplot(X)
  
  # Ahora sí
  fun <- function(x) dexp(x)/(1-exp(-2)) # Densidad de lambda truncada (sin indicadora)
  phi <- function(x) dnorm(x)/fun(x)
  estim_is[i] <- mean(phi(X))
}

dat <- rbind(
  data.frame(id='mc', estim=estim_mc),
  data.frame(id='is', estim=estim_is)
)

ggplot(dat) +
  geom_density(aes(estim)) +
  geom_vline(aes(xintercept=real)) +
  facet_wrap(~id)










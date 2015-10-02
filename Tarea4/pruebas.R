library(dplyr)
library(ggplot2)


dist <- function(n) runif(n)
phi <- function(x) x
alpha <- 0.05


## Montecarlo
nsamp <- 1000
x <- dist(nsamp)
phix <- phi(x)
theta_hat <- mean(phix)
s <- sd(phix)
IC <- data.frame(Estimate=theta_hat,
                 sd = s,
                 LI=theta_hat + qnorm(alpha/2)*s/sqrt(nsamp),
                 UI=theta_hat + qnorm(1 - alpha/2)*s/sqrt(nsamp))
IC$IC_length <- IC$UI-IC$LI
IC
aux <- data.frame(x=seq(IC$LI,IC$UI,l=1000))
aux$y <- dnorm(aux$x, theta_hat, s/sqrt(nsamp))
qplot(x, y, data=aux, geom='line')


## Bootstrap
nsamp <- 1000
nboot <- 10000

x <- dist(nsamp)
theta_hat <- mean(phi(x))
thetas <- numeric(nboot)
for(i in 1:nboot){
  xb <- sample(x, nsamp, replace = TRUE)
  thetas[i] <- mean(phi(xb))
}
s <- sd(thetas)
# Intervalos no centrados
IC_BS <- data.frame(Estimate=theta_hat,
                    sd=s,
                    LI=as.numeric(quantile(thetas, alpha/2)),
                    UI=as.numeric(quantile(thetas, 1 - alpha/2))) %>%
  mutate(IC_length = UI - LI)
IC_BS
ggplot() +
  geom_histogram(aes(thetas, ..density..)) +
  geom_line(data=aux, aes(x,y))














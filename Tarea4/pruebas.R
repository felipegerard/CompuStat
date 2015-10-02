library(dplyr)
library(ggplot2)


dist <- function(n) runif(n)
dist <- function(n) rgamma(n, 1, 5)
phi <- function(x) x
alpha <- 0.05

nsamp <- 10
x <- dist(nsamp)
phix <- phi(x)

## Asintótico
theta_hat <- mean(phix)
s <- sd(phix)
IC <- data.frame(Estimate=theta_hat,
                 sd = s,
                 LI=theta_hat + qnorm(alpha/2)*s/sqrt(nsamp),
                 UI=theta_hat + qnorm(1 - alpha/2)*s/sqrt(nsamp))
IC$IC_length <- IC$UI-IC$LI
IC
aux <- data.frame(x=seq(IC$LI,IC$UI,l=100))
aux$y <- dnorm(aux$x, theta_hat, s/sqrt(nsamp))
#qplot(x, y, data=aux, geom='line')

## Bootstrap
nboot <- 10000
theta_hat <- mean(phix)
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












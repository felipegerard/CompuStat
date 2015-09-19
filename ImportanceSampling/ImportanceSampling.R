require(ggplot2)
require(plyr)
require(parallel)

mc.intervals <- function(phi, N, q=runif, alpha=0.05, mc.cores=4, control=list()){
  # N puede ser un vector
  results <- mclapply(mc.cores=mc.cores, N, function(nsim){
    x <- q(nsim)
    phix <- sapply(x, phi)
    estim <- mean(phix)
    s2 <- var(phix)
#     if(length(control) > 0){
#       sample <- function(n) runif(n, interval
    quant <- qnorm(alpha/2, lower.tail = F)
    int_upper <- estim + sqrt(s2/nsim)*quant
    int_lower <- estim - sqrt(s2/nsim)*quant
    return(data.frame(N=nsim, Estimate=estim, LI=int_lower, UI=int_upper))
  })
  return(ldply(results))
}

phi <- function(x) 2*sqrt(4 - x^2)
#N <- seq(1000, 100000, 1000)
N <- rep(1000, 1000, 5000)
X.dens <- function(nsim) runif(nsim, 0, 2)

system.time(data <- mc.intervals(phi, rev(N), X.dens, mc.cores=6))

ggplot(data, aes(1:length(N), Estimate, ymin=LI, ymax=UI)) +
  geom_ribbon(fill='blue', alpha=0.3) +
  geom_line()

qplot(data$Estimate)

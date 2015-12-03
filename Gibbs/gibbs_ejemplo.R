
x <- iris$Sepal.Length

n <- 10000
burnin <- 5000
mus <- rep(0,n)
sigmas2 <- rep(1,n)

for(i in 2:n){
  mus[i] <- rnorm(1, sum(x)/(1 + sigmas2[i-1]), sqrt(sigmas2[i-1]/(1 + sigmas2[i-1])))
  sigmas2[i] <- 1/rgamma(1, length(x)/2 + 1, rate = 1 + sum((x - mus[i])^2)/2)
}

mus <- mus[-(1:burnin)]
sigmas2 <- sigmas2[-(1:burnin)]

hist(mus)
mean(mus)

hist(sigmas2)
mean(sigmas2)

hist(x)
mean(x)
var(x)

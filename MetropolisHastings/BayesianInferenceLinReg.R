library(Rcpp)
library(ggplot2)
library(dplyr)

# TAREA: Hacer ejemplo de regresión lineal con iris. Quitar especie y hacer una regresión de Sepal.Length ~ .


# ESTIMATION OF A MEAN
data(iris)
N <- nrow(iris)
f <- Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
y <- iris$Sepal.Length
x <- iris[c('Sepal.Width','Petal.Length','Petal.Width')]
# 1) FRECUENTIST APPROACH
mod.lm <- lm(f, iris)
summary(mod.lm)
pr1 <- predict(mod.lm, newdata = iris, interval = 'confidence')
pr2 <- predict(mod.lm, newdata = iris, interval = 'prediction')
pred <- cbind(pr1, pr2[,-1]) %>%
  as.data.frame
names(pred) <- c('fit','lwr.conf','upr.conf','lwr.pred','upr.pred')
head(pred) # No se puede graficar porque está en 4 dimensiones


# BAYESIAN APPROACH

# beta_j ~ N(0,10)
# Se puede jugar con los parámetros de la inicial aquí y en 1). Cuando es muy plana los coeficientes se parecen mucho a los de la regresión lineal
prior.beta <- function(x) dnorm(x, 0, 10)
plot(prior.beta, col="darkblue", xlim=c(-50,50), lwd="2", main="Prior for mean", ylab="density")

# 1) logposterior distribution (up to a constant)
cppFunction('
  double objdens(NumericMatrix X, NumericVector y, NumericVector theta){
    double lkh, logprior, yhat;
    int m=X.nrow(), p=X.ncol();
    NumericVector aux(m);
    // Compute loglikelihood
    lkh=0;
    for (int i=0; i<m; i++){
      aux = X(i,_)*theta;
      yhat = std::accumulate(aux.begin(), aux.end(), 0.0);
      lkh += -.5*pow(y[i] - yhat,2);
    }
    // Compute logprior
    logprior = 0.0;
    for(int j=0; j<p; j++){
      logprior = R::dnorm(theta[j], 0.0, 100, true); // Aquí la inicial!!
    }
    // Log of target density
    return lkh + logprior;
}')
objdens(as.matrix(x), y, 1:3)

# 2) Proposal: random walk in the same dimension as the number of parameters
cppFunction('
  NumericVector proposal(NumericVector theta){
    int nparam = theta.size();
    double jump = 0.1; 
    NumericVector newtheta(nparam);
    for (int i=0; i<nparam; i++){
      newtheta[i] = R::rnorm(theta[i], jump);
    }
    return newtheta;
}')
proposal(c(1,2,3))


# 3) METROPOLIS

sourceCpp("BayesianMHLinReg.cpp")

nsim <- 10000
init <- rep(0,ncol(x)+1) # Take intercept into account
mh.samp <- MHBayesLinReg(nsim, init, objdens, proposal,
                         cbind(1,as.matrix(x)), y) # 1 for intercept
estims <- mh.samp$theta
mh.samp

#  SOME DIAGNOSTIC IMPORTANT STUFF
#  Exploration graph:
library(calibrate)
pts <- seq(1,nrow(estims),by=5)
plot(estims[pts, ], type="l")
#textxy(estims[pts,1], estims[pts,2], pts)
cor(estims)
### 1) REJECTION RATES
rejections <- mh.samp$rejections[-1]
trials <- rejections + 1
rej.rate <- cumsum(rejections)/cumsum(trials)
plot(rej.rate, type="l", ylim=c(0,1), main="Rejection rate")
plot(trials[-1], type="l", main="Number of trials")
### 2) AUTOCORRELATION
par(mfrow=c(2,2))
for(i in 1:4){
  acf(estims[ , i])
}
par(mfrow=c(1,1))
# burnin and subsampling
burnin <- 100
estims <- estims[-(1:burnin), ]
thinning <- 15
pts <- seq(1, nsim-burnin, by=thinning)
# OBS: thinning IS actually useful here
estims <- estims[pts, ]
par(mfrow=c(2,2))
for(i in 1:4){
  acf(estims[ , i])
}
par(mfrow=c(1,1))


# LET'S COMPARE PRIORS AND POSTERIORS AND DO INFERENCE
par(mfrow=c(2,2))
for(j in 1:4){
  hist(estims[ ,j], prob=TRUE,  breaks=20, col="lightblue",
       main="Histogram and Posterior(blue) vs Prior(red) of the Mean")
  plot(prior.beta, xlim=c(min(estims[ ,j]),max(estims[ ,j])), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
  lines(density(estims[ ,j]), col="darkblue", lwd="2")
}
par(mfrow=c(1,1))

# Pointwise estimations of coefficients
betahat <- sapply(1:(ncol(x)+1), function(i) mean(estims[,i]))
betasd <- sapply(1:(ncol(x)+1), function(i) sd(estims[,i]))

# CERTAINTY INTERVALS
alpha <- 0.05
intervals <- lapply(1:(ncol(x)+1), function(i){
  quantile(estims[ ,i], c(alpha/2, 1-alpha/2)) %>%
    t %>%
    as.data.frame
}) %>%
  rbind_all

# COMPARISON OF ALL RESULTS
Comparison <- data.frame(betahat, betasd, intervals)
colnames(Comparison) <- c('Estimate', 'sd', colnames(intervals))
Comparison
summary(mod.lm)


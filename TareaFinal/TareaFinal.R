


#setwd("D:/Dropbox/Cursos impartidos/Estad?stica-Computacional/EM")
library(foreign)
library(mvtnorm)
library(dplyr)
library(ggplot2)
library(parallel)
library(tidyr)
library(AUC)


# Leer datos --------------------------------------------------------------


table <- read.dta("datos_politicos.dta") # LEEMOS LOS DATOS DE FORMATO STATA
anio <- 1986 # POR EL MOMENTO ESCOJAMOS UN SOLO A?O PUES NO SABEMOS NADA DE DATOS PANEL
data <- table[table$year==anio, ]
labels <- paste(names(data), attributes(data)$var.labels, sep=": ") # NOMBRES DE LAS VARIABLES

y <- data$reg # INDICADORA DE SI SE ES O NO UNA DEMOCRACIA
list.depend <- c("level", "open", "g", "strikes", "govpwt", "ls", "invpwt",
                 "fertil", "lfagric", "popg", "femsec", "EnergProd") # VARIABLES ECONOMICAS EXPLICATIVAS



# Imputación --------------------------------------------------------------

imputar <- function(dat, maxiter=300, tol=1e-6, verbose=T){
  # Valores faltantes
  mis <- is.na(dat)
  
  # Estimaciones iniciales ignorando NAs
  mu <- apply(dat, 2, function(y) mean(y, na.rm=TRUE))
  S <- var(dat, na.rm=TRUE)
  
  # Primera imputación simple
  na_rows <- as.numeric(which(apply(mis, 1, any)))
  norms <- rmvnorm(nrow(dat), mu, S)
  dat[mis] <- norms[mis]
  
  
  logliks <- numeric(maxiter)
  crit <- 1
  for(i in 1:maxiter){
    if(verbose) print(i)
    for(j in 1:ncol(dat)){
      v <- names(dat)[j]
      f <- as.formula(paste(v, '~ .'))
      mod <- lm(f, data = dat)
      xjhat <- predict(mod, newdata=dat)
      dat[mis[,j], j] <- xjhat[mis[,j]]
    }
    
    mu <- apply(dat, 2, function(y) mean(y))
    S <- var(dat)
    
    logliks[i] <- sum(dmvnorm(dat, mu, S, log = TRUE))
    if(i > 1){
      crit <- (logliks[i] - logliks[i-1])/logliks[i-1]
      if(verbose) print(crit)
      if(!is.na(crit) && abs(crit) < tol){
        logliks <- logliks[1:i]
        break
      }
    }
  }
  list(x=dat, logliks=logliks)
}

# Ejemplo de uso
x <- data[list.depend]
res <- imputar(x, maxiter=100, tol=2*1e-6, verbose = T)
xi <- res$x
logliks <- res$logliks
plot(logliks, type='l')


## Ahora sí Bootstrap

# Datos originales
x <- data[list.depend]

# Parámetros
nboot <- 100
maxiter <- 300
tol <- 5*1e-6

# Bootstrap
res <- mclapply(mc.cores = 8, 1:nboot, function(k){
  idx <- sample(1:nrow(x), size = nrow(x), replace = T)
  xx <- x[idx, ]
  xi <- imputar(xx, maxiter=maxiter, tol=tol, verbose = T)$x
  mod <- glm(y ~ ., data = cbind(xi, y), family = binomial(link='logit'))
  phat <- predict(mod, newdata = xi)
  yhat <- round(phat)
  acc <- mean(y == yhat)
  list(beta=coef(mod), phat=phat, yhat=yhat, acc=acc)
})

# Juntamos las betas de todas las muestras bootstrap en un data.frame
betas <- res %>%
  lapply(function(x) as.data.frame(t(as.data.frame(x$beta)))) %>%
  rbind_all %>%
  rename(Intercept = `(Intercept)`)

# Quitamos betas degeneradas (NAs) y outliers
betas <- betas[!apply(betas, 1, function(x) any(is.na(x))),]
# betas <- betas[(betas$Intercept > quantile(betas$Intercept, 0.025)) & (betas$Intercept < quantile(betas$Intercept, 0.975)),]

# Gráficas de distribución bootstrap
long <- gather(betas, var, val, Intercept:EnergProd)
ggplot(long, aes(val)) +
  geom_bar() +
  facet_wrap(~var, scales = 'free_x')

accs <- res %>%
  sapply(function(q) q$acc)
qplot(accs)

# Resultado final
beta.mu <- apply(betas, 2, mean)
beta.sd <- apply(betas, 2, sd)
odds.ratio <- exp(beta.mu)

format(round(data.frame(Estimate=beta.mu,
                        sd=beta.sd,
                        Odds.ratio=odds.ratio), 5), scientific=F)


phat <- res %>%
  sapply(function(q) q$phat)
phat <- phat[,!apply(phat, 2, function(x) any(is.na(x)))] %>%
  apply(1, mean)

yhat <- as.numeric(round(phat))

table(yhat, y)
mean(y == yhat)
y
yhat


# PCA
xi <- imputar(x, maxiter=300, tol=2*1e-6, verbose = T)$x
cor(xi)
pc <- prcomp(xi)
summary(pc)
qplot(pc$x[,'PC1'], pc$x[,'PC2'], color=y, size=2)
# Evidentemente sobreajustamos muchísimo con 10 variables más


















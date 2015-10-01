
library(dplyr)
library(ggplot2)
library(boot)

# PARÁMETROS
nboot <- 10000
sample.size <- 100
X <-  runif(sample.size) #rnorm(sample.size)
fun <- var


# BOOTSTRAP-MC A LA ANTIGUA -----------------------------------------------

vec <- numeric(nboot)
for(i in 1:nboot){
  X.b <- sample(X, sample.size, replace = T)
  vec[i] <- fun(X.b)
}

# hist(X)
fun(X) # Estimador sin remuestreo
hist(vec) # Distribución del estimador fun
summary(vec)
1/12 # Varianza de una uniforme (ie. de X), que es el valor real
var(vec) # Varianza del estimador

## INTERVALOS DE CONFIANZA
alpha <- 0.05
# 1) PERCENTILES (EQUIVALENTE A PERCENTILE DE BOOT)
int.quant <- quantile(vec, c(alpha/2, 1 - alpha/2))
int.quant
fun(X) # Estimador original. Estos intervalos no están centrados en el estimador de la muestra original

# 2) MÉTODO CENTRADO



# AHORA USANDO EL PAQUETE BOOT --------------------------------------------

# AHORA USANDO EL PAQUETE BOOT
# Siempre hay que definir primero una función que reciba dos argumentos, los datos que pueden ser un vector, un data.frame, etc, y un vector de índices (ver abajo)
my.fun <- function(x, ind) fun(x[ind])

boot.res <- boot(X, my.fun, R = nboot)
boot.res
#boot.res$t # Es equivalente a 'vec' en nuestra implementación
plot(boot.res)

boot.ci(boot.res) 















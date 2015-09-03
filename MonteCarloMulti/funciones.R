# Funciones ---------------------------------------------------------------

fun <-  function(x){
  n <- length(x)
  (2*pi)^(-n/2)*exp(-0.5*sum(x*x))
}


int_trap <- function(fx, a, b){
  N <- length(fx)
  (b - a)/2*mean(fx[-N] + fx[-1])
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
  I <- int_trap(fx, a[1], b[1])
  return(I)
}

int_riem <- function(fx, a, b){
  N <- length(fx)
  (b - a)*mean(fx)
}

int_riem_mult <- function(f, a, b, N=20){
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
      fx[i] <- int_riem_mult(g, a[-1], b[-1], N=N)
    }
  }
  #   print(x)
  #   print(fx)
  I <- int_riem(fx, a[1], b[1])
  return(I)
}

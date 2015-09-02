library(shiny)
library(ggplot2)
library(dplyr)
library(pracma)


# Funciones ---------------------------------------------------------------

fun <-  function(x){
  n <- length(x)
  (2*pi)^(-n/2)*exp(-0.5*sum(x*x))
}

int_trap <- function(f, x){
  N <- length(x)
  (x[N] - x[1])*mean(f)
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
  #   print(x)
  #   print(fx)
  I <- int_trap(fx, x)
  return(I)
}

# int_trap_mult(fun, rep(-2,n), rep(2,n), N=1100)
# (erf(sqrt(2)))^n


shinyServer(function(input, output){
  I_trap <- reactive({
    sapply(1:input$N, function(k){
      int_trap_mult(fun, rep(input$a[1],input$n), rep(input$a[2],input$n), k)
      })
  })
  
  I_MC <- reactive({
    phi <- function(x){
      as.numeric(all(x >= input$a[1] & x <= input$a[2]))
    }
    I <- numeric(input$N)
    phix <- I
    lower <- I
    upper <- I
    s <- I
    set.seed(input$seed)
    for(i in 1:input$N){
      x <- rnorm(input$n, 0, 1)
      phix[i] <- phi(x)
      s[i] <- sd(phix[1:i])
      I[i] <- mean(phix[1:i])
      lower[i] <- I[i] - s[i]/sqrt(i)*qnorm(1 - input$alpha/2, 0, 1)
      upper[i] <- I[i] + s[i]/sqrt(i)*qnorm(1 - input$alpha/2, 0, 1)
    }
    out <- data.frame(I_MC=I, lower=lower, upper=upper, sd=s, true = erf(sqrt(2))^input$n)
    out
  })
  
  data <- reactive({
    cbind(nsim=1:length(I_trap()), I_trap=I_trap(), I_MC())
  })
  
  output$plot <- renderPlot({
    p <- ggplot(data(), aes(x=nsim)) +
      geom_line(aes(y=I_trap), color='blue', size=1) +
      geom_line(aes(y=I_MC), color='black', size=1)
    if(input$ribbon){
      p <- p + geom_ribbon(aes(ymin=lower, ymax=upper), fill='black', alpha=0.5)
    }
    if(all(input$a == c(-2,2))){
      p <- p + geom_line(aes(y=true), color='red', size=1, linetype='dashed')
    }
#     p + ylim(min(I_MC()[1:4], na.rm = T), max(I_MC()[1:4], na.rm = T))
    p + ylim(0,1) + labs(x='Número de simulaciones/puntos',
                         y='I',
                         title='Estimación del valor de la integral')
  })
  output$errplot <- renderPlot({
    if(all(input$a == c(-2,2))){
      p <- ggplot(data(), aes(x=nsim)) +
        geom_line(aes(y=abs(I_trap-true)), color='blue', size=1) +
        geom_line(aes(y=abs(I_MC-true)), color='black', size=1) +
        geom_line(aes(y=true-true), color='red', linetype='dashed')
    }else{
      p <- ggplot(data(), aes(nsim, I_MC)) +
        geom_line(alpha=0)
    }
    p + ylim(0,1)
  })
  output$data <- renderDataTable(round(data(), 3))
  output$data <- renderDataTable(round(data(), 3))
  
})











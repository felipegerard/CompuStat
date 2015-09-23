library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(parallel)

mc.intervals <- function(phi, N, rg=runif, fg=dunif, alpha=0.05, mc.cores=4){
  # N puede ser un vector
  # fg es la densidad de la que se quiere simular
  # rg genera muestras de x ~ fg
  results <- mclapply(mc.cores=mc.cores, N, function(nsim){
    x <- rg(nsim)
    gx <- sapply(x, fg)
    phix <- sapply(x, phi)
    estim <- mean(phix/gx)
    s2 <- var(phix/gx)
    quant <- qnorm(alpha/2, lower.tail = F)
    int_upper <- estim + sqrt(s2/nsim)*quant
    int_lower <- estim - sqrt(s2/nsim)*quant
    salida <- data.frame(N=nsim, Estimate=estim, LI=int_lower, UI=int_upper)
    return(salida)
  })
  return(ldply(results) %>% mutate(i = row_number()))
}

ind <- function(x) (x > -1 & x < 1)
gen_r <- function(n, f){
  out <- f(n)
  for(i in 1:n){
    while(!ind(out[i])){
      out[i] <- f(1)
    }
  }
  out
}

g0 <- function(x) dunif(x,-1,1)*ind(x)
rg0 <- function(n) gen_r(n, function(n) runif(n,-1,1))
g1 <- function(x) dnorm(x,0,1)*ind(x)
rg1 <- function(n) gen_r(n, function(n) rnorm(n,0,1))
g2 <- function(x) dnorm(x,0,4)*ind(x)
rg2 <- function(n) gen_r(n, function(n) rnorm(n,0,4))
g3 <- function(x) dnorm(x,-1,1)*ind(x)
rg3 <- function(n) gen_r(n, function(n) rnorm(n,-1,1))
g4 <- function(x) dnorm(x,0,0.3)*ind(x)
rg4 <- function(n) gen_r(n, function(n) rnorm(n,0,0.3))

gs <- list(list(g0,rg0),list(g1,rg1),list(g2,rg2),list(g3,rg3),list(g4,rg4))

shinyServer(function(input, output, session){
  
  #g <- reactive(eval(parse(text = input$g)))
  N <- reactive(rep(input$n, input$N))
  mc <- reactive({
    phi <- function(x){
      input$k / (1 + abs(x)^input$m)
    }
    i <- as.numeric(input$g1)
    g <- gs[[i]]
    mc.intervals(phi, N(), g[[2]], g[[1]], alpha = 0.05, mc.cores = 4)
  })
  output$mc_data <- renderDataTable(mc())
  output$mc_plot <- renderPlot(
    ggplot(mc(), aes(i, Estimate)) +
      geom_line() +
      geom_ribbon(aes(ymin=LI, ymax=UI))
  )
  
  is <- reactive({
    phi <- function(x){
      input$k / (1 + abs(x)^input$m)
    }
    i <- as.numeric(input$g2)
    g <- gs[[i]]
    mc.intervals(phi, N(), g[[2]], g[[1]], alpha = 0.05, mc.cores = 4)
  })
  output$is_data <- renderDataTable(is())
  output$is_plot <- renderPlot(
    ggplot(is(), aes(i, Estimate)) +
      geom_line() +
      geom_ribbon(aes(ymin=LI, ymax=UI))
  )
  
  output$hist <- renderPlot({
    dat <- rbind(cbind(method='x ~ g1',mc()), cbind(method='x ~ g2',is()))
    ggplot(dat, aes(Estimate, fill=method)) +
      geom_density(alpha=0.5)
  })
  output$func <- renderPlot({
    phi <- function(x){
      input$k / (1 + abs(x)^input$m)
    }
    gg1 <- gs[[as.numeric(input$g1)]][[1]]
    gg2 <- gs[[as.numeric(input$g2)]][[1]]
    par(mfrow=c(2,2))
    plot(gg1, xlim=c(-2,2), ylab='g1(x)')
    plot(gg2, xlim=c(-2,2), ylab='g1(x)')
    plot(phi, xlim=c(-2,2), ylab='phi(x)')
    par(mfrow=c(1,1))
  })
    
})


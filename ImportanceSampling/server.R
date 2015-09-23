library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(parallel)
library(triangle)

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

g0 <- function(x) dunif(x,-1,1)
rg0 <- function(n) runif(n,-1,1)
g1 <- function(x) dnorm(x,0,1)
rg1 <- function(n) rnorm(n,0,1)
g2 <- function(x) dnorm(x,0,4)
rg2 <- function(n) rnorm(n,0,4)
g3 <- function(x) dnorm(x,-1,1)
rg3 <- function(n) rnorm(n,-1,1)
g4 <- function(x) dnorm(x,0,0.3)
rg4 <- function(n) rnorm(n,0,0.3)
g5 <- function(x) dtriangle(x,-1,1,0)
rg5 <- function(n) rtriangle(n,-1,1,0)

gs <- list(list(g0,rg0),list(g1,rg1),list(g2,rg2),list(g3,rg3),list(g4,rg4),list(g5,rg5))

shinyServer(function(input, output, session){
  
  #g <- reactive(eval(parse(text = input$g)))
  N <- reactive(rep(input$n, input$N))
  mc <- reactive({
    phi <- function(x){
      ind(x)*input$k / (1 + abs(x)^input$m)
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
      ind(x)*input$k / (1 + abs(x)^input$m)
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
    int <- rbind(
      data.frame(LI=quantile(mc()$Estimate, 0.05),
                 mean=mean(mc()$Estimate),
                 UI=quantile(mc()$Estimate, 0.975),
                 method='x ~ g1'),
      data.frame(LI=quantile(is()$Estimate, 0.025),
                 mean=mean(is()$Estimate),
                 UI=quantile(is()$Estimate, 0.975),
                 method='x ~ g2')
    )
    ggplot(dat) +
      geom_density(aes(Estimate, fill=method), alpha=0.5) +
      geom_vline(data=int, aes(xintercept=LI, color=method),
                 linetype='dashed', size=1) +
      geom_vline(data=int, aes(xintercept=mean, color=method), size=1) +
      geom_vline(data=int, aes(xintercept=UI, color=method),
                 linetype='dashed', size=1)
  })
  output$sims <- renderPlot({
    dat_mc <- mc() %>% mutate(i = row_number(), method='x ~ g1')
    dat_is <- is() %>% mutate(i = row_number(), method='x ~ g2')
    dat <- rbind(dat_mc, dat_is)
    ggplot(dat) +
      geom_ribbon(aes(i,ymin=LI,ymax=UI, fill=method), alpha=0.5) +
      geom_line(aes(i,Estimate)) +
      facet_wrap(~method, nrow = 2)
  })
  output$func <- renderPlot({
    phi <- function(x){
      input$k / (1 + abs(x)^input$m)
    }
    phi_trunc <- function(x) phi(x)*ind(x)
    gg1 <- gs[[as.numeric(input$g1)]][[1]]
    gg2 <- gs[[as.numeric(input$g2)]][[1]]
    par(mfrow=c(2,2))
    plot(gg1, xlim=c(-3,3), ylab='g1(x)')
    plot(gg2, xlim=c(-3,3), ylab='g1(x)')
    plot(phi, xlim=c(-3,3), ylab='phi(x)')
    plot(phi_trunc, xlim=c(-3,3), ylab='phi(x)*I(0,1)(x)')
    par(mfrow=c(1,1))
  })
    
})


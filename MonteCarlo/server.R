library(shiny)
library(ggplot2)
library(dplyr)
library(pracma)
library(parallel)

source('funciones.R')

shinyServer(function(input, output, session){
  
  I_riem <- reactive({
    print(input$fun)
    f <- function(x) fun(x, fun=input$fun)
    mclapply(1:input$N, function(k){
      int_riem_mult(f, input$a, input$b, k)
    }, mc.cores=input$mc.cores) %>%
      unlist
  })
  I_trap <- reactive({
    print(input$fun)
    f <- function(x) fun(x, fun=input$fun)
    mclapply(1:input$N, function(k){
      int_trap_mult(f, input$a, input$b, k)
    }, mc.cores=input$mc.cores) %>%
      unlist
  })
  I_MC <- reactive({
    set.seed(input$seed)
    I <- numeric(input$N)
    indicadorax <- I
    lower <- I
    upper <- I
    s <- I
    x <- runif(input$N, input$a, input$b)
    fx <- (input$b - input$a)*sapply(x, function(x) fun(x, fun=input$fun))
    I <- cumsum(fx)/1:length(fx)
    s <- mclapply(1:input$N, function(i) sd(fx[1:i]), mc.cores = input$mc.cores) %>% unlist
    for(i in 1:input$N){
      lower[i] <- I[i] - s[i]/sqrt(i)*qnorm(1 - input$alpha/2, 0, 1)
      upper[i] <- I[i] + s[i]/sqrt(i)*qnorm(1 - input$alpha/2, 0, 1)
    }
    out <- data.frame(I_MC=I, lower=lower, upper=upper, sd=s)
    out
  })
  data <- reactive({
    cbind(nsim=1:length(I_trap()), I_riem=I_riem(), I_trap=I_trap(), I_MC())
  })
  
  output$estim_riem <- renderText({
    n <- nrow(data())
    paste0('Estimación Riemann: ', round(data()$I_riem[n], 5))
  })
  output$estim_trap <- renderText({
    n <- nrow(data())
    paste0('Estimación trapecio: ', round(data()$I_trap[n], 5))
  })
  output$estim_MC <- renderText({
    n <- nrow(data())
    paste0('\nEstimación MonteCarlo: ', round(data()$I_MC[n], 5))
  })
  
  output$plot <- renderPlot({
    p <- ggplot(data(), aes(x=nsim)) +
      geom_line(aes(y=I_riem), color='green', size=1) +
      geom_line(aes(y=I_trap), color='blue', size=1) +
      geom_line(aes(y=I_MC), color='black', size=1)
    if(input$ribbon){
      p <- p + geom_ribbon(aes(ymin=lower, ymax=upper), fill='black', alpha=0.5)
    }
    p +
      labs(x='Número de simulaciones/puntos',
           y='I',
           title='Estimación del valor de la integral')
  })
  
  output$data <- renderDataTable(round(data(), 3))
  output$data <- renderDataTable(round(data(), 3))
  
  observeEvent(input$reset_input, {
    updateSliderInput(session, "N", value = 50)
    updateSliderInput(session, "a", value = 0)
    updateSliderInput(session, "b", value = 2)
    updateSliderInput(session, "alpha", value = 0.05)
    updateNumericInput(session, "seed", value = 1234)
    updateCheckboxInput(session, "ribbon", value = TRUE)
  })
  
})











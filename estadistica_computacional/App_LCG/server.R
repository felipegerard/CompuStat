library(shiny)
library(ggplot2)

LCG <- function(nsim, M = 259, a = 67, c = 1, x0 = 1){
  x <- c(x0, nsim - 1)
  for(i in 1:(nsim - 1)) x[i+1] <- (a*x[i] + c) %% M
  x/M
}

shinyServer(function(input, output){
  a <- reactive(input$a)
  d <- reactive(LCG(input$nsim, input$m, input$a, input$c, input$x0))
  output$random_numbers <- renderDataTable(data.frame(rnd = d()))
  output$random_hist <- renderPlot(qplot(d(), binwidth = 1/30))
  output$random_plot <- renderPlot(
    qplot(d()[-input$nsim], d()[-1])
  )
  output$c <- renderText(input$c)
})
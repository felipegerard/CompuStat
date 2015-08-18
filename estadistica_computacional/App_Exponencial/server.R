library(shiny)
library(ggplot2)

my_rexp <- function(nsim, lambda = 1, seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  u <- runif(nsim)
  -(1/lambda)*log(1-u)
}

shinyServer(function(input, output){
  d <- reactive(my_rexp(input$nsim, input$lambda, input$seed))
  output$random_numbers <- renderDataTable(data.frame(rnd = d()))
  output$random_hist <- renderPlot(qplot(d()) + xlim(input$range[1], input$range[2]))
  output$random_plot <- renderPlot(
    qplot(d()[-input$nsim], d()[-1])
  )
  output$c <- renderText(input$c)
})
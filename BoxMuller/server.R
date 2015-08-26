library(shiny)
library(ggplot2)

my_rnorm <- function(nsim, seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  odd <- nsim %% 2
  x <- numeric(nsim + odd)
  for(i in 1:floor(nsim/2)){
    u <- runif(2)
    x[2*(i-1) + 1] <- sqrt(-2*log(u[1]))*cos(2*pi*u[2])
    x[2*(i-1) + 2] <- sqrt(-2*log(u[1]))*sin(2*pi*u[2])
  }
  x[1:(nsim - odd)]
}

shinyServer(function(input, output){
  d <- reactive(my_rnorm(input$nsim, input$seed))
  output$random_numbers <- renderDataTable(data.frame(rnd = d()))
  output$random_hist <- renderPlot(qplot(d()))
  p <- reactive({
    p <- qplot(d()[-input$nsim], d()[-1]) +
      coord_equal()
    if(input$smooth) p <- p + geom_smooth()
    if(input$dens) p <- p + geom_density2d()
    p
  })
  output$random_plot <- renderPlot(p())
  output$c <- renderText(input$c)
})

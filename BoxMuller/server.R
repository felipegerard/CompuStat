library(shiny)
library(ggplot2)

my_rnorm <- function(nsim, mu = 0, sigma = 1, seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  odd <- nsim %% 2
  x <- numeric(nsim + odd)
  for(i in 1:floor(nsim/2)){
    while(TRUE){
      u <- runif(1)
      v <- runif(1)
      s <- u^2 + v^2
      if(s > 0 && s < 1){
        signs <- sign(2*runif(2) - 1)
        x[2*(i-1) + 1] <- signs[1]*u*sqrt(-2*log(s)/s)
        x[2*(i-1) + 2] <- signs[2]*v*sqrt(-2*log(s)/s)
        break
      }
    }
  }
  x[1:(nsim - odd)]
}

shinyServer(function(input, output){
  d <- reactive(my_rnorm(input$nsim, input$mu, input$sigma, input$seed))
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

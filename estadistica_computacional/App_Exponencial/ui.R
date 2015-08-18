library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('LCG'),
    sidebarPanel(
      numericInput('nsim', label = 'Número de simulaciones', value = 100, min = 1),
      numericInput('lambda', label = 'lambda', value = 1, min = 0),
      sliderInput('seed', label = 'Semilla', value = 5, min = 1, max = 2^16 - 1),
      sliderInput('range', label = 'Límites de la gráfica', value = c(0,1), min = 0, max = 10, step = 0.01)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Hist', plotOutput('random_hist')),
        tabPanel('Plot', plotOutput('random_plot')),
        tabPanel('Table', dataTableOutput('random_numbers'))
      )
    )
  )
})
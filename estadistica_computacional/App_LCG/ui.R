library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('LCG'),
    sidebarPanel(
      numericInput('nsim', label = 'Número de simulaciones', value = 100, min = 1),
      numericInput('m', label = 'm', value = 2^16 - 1, min = 1),
      numericInput('a', label = 'a', value = 67, min = 1),
      numericInput('c', label = 'c', value = 1, min = 1),
      sliderInput('x0', label = 'x0', value = 5, min = 1, max = 2^16 - 1)
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
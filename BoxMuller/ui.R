library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('Generando exponenciales'),
    sidebarPanel(
      numericInput('nsim', label = 'Número de simulaciones', value = 1000, min = 1),
      sliderInput('seed', label = 'Semilla', value = 5, min = 1, max = 2^16 - 1),
      checkboxInput('smooth', 'Tendencia', value = 1),
      checkboxInput('dens', 'Densidad', value = 0)
      #sliderInput('range', label = 'Límites de la gráfica', value = c(-4,4), min = -10, max = 10, step = 0.01)
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

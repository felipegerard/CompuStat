library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('Integración Trapezoidal vs. Integración MonteCarlo'),
    sidebarPanel(
      sliderInput('N', 'Número de simulaciones',
                  value = 50, min = 1, max = 1000, step = 1),
      sliderInput('n', 'Número de puntos por simulación',
                  value = 100, min = 1, max = 1000, step = 1),
      sliderInput('k', 'Numerador de phi', value = 10,
                  min = 1, max = 100000, step = 1),
      sliderInput('m', 'Exponente de phi', value = 10,
                  min = 1, max = 10, step = 1),
      selectInput('g1', 'Función de muestreo g1', c('f'=1,'N(0,1)'=2,'N(0,4)'=3,'N(-1,1)'=4,'N(0,0.3)'=5), selected = 1),
      selectInput('g2', 'Función de muestreo g2', c('f'=1,'N(0,1)'=2,'N(0,4)'=3,'N(-1,1)'=4,'N(0,0.3)'=5), selected = 2)
      #actionButton('reset_input', 'Reset input')
    ),
    mainPanel(
      tabsetPanel(
        #tabPanel('Plot', plotOutput('mc_plot', width = '8in', height = '3in')),
        tabPanel('Histogram', plotOutput('hist', width = '8in', height = '6in')),
        tabPanel('Functions', plotOutput('func', width = '8in', height = '6in')),
        tabPanel('Data', dataTableOutput('mc_data'))
      )
    )
  )
})

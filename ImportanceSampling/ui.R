library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('Importance Sampling: Integrando phi(x) = k/(1 + |x|^m)'),
    sidebarPanel(
      sliderInput('N', 'Número de simulaciones',
                  value = 50, min = 1, max = 1000, step = 1),
      sliderInput('n', 'Número de puntos por simulación',
                  value = 100, min = 1, max = 1000, step = 1),
      selectInput('g1', 'Función de muestreo g1', c('U(-1,1)'=1,'N(0,1)'=2,'N(0,4)'=3,'N(-1,1)'=4,'N(0,0.3)'=5,'Triang(-1,1,0)'=6), selected = 1),
      selectInput('g2', 'Función de muestreo g2', c('U(-1,1)'=1,'N(0,1)'=2,'N(0,4)'=3,'N(-1,1)'=4,'N(0,0.3)'=5,'Triang(-1,1.0)'=6), selected = 2),
      sliderInput('k', 'Numerador de phi (k)', value = 10,
                  min = 1, max = 100000, step = 1),
      sliderInput('m', 'Exponente de phi (m)', value = 10,
                  min = 1, max = 10, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        #tabPanel('Plot', plotOutput('mc_plot', width = '8in', height = '3in')),
        tabPanel('Simulation',
                 plotOutput('sims', width = '8in', height = '6in')),
        tabPanel('Histogram', plotOutput('hist', width = '8in', height = '6in')),
        tabPanel('Functions', plotOutput('func', width = '8in', height = '6in')),
        tabPanel('Data g1', dataTableOutput('mc_data')),
        tabPanel('Data g2', dataTableOutput('is_data'))
      )
    )
  )
})

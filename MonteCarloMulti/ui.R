library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('Integración Trapezoidal vs. Integración MonteCarlo'),
    sidebarPanel(
      numericInput('n', label = 'Dimensión de la normal multivariada', value = 1, min = 1, max = 5),
      sliderInput('N', label = 'Número de puntos/simulaciones por dimensión', value = 50, min = 1, max = 200, step = 1),
      sliderInput('a', label = 'Límites de integración', min = -6, max = 6, value = c(-2,2), step = 0.01),
      sliderInput('alpha', label = 'Significancia de los intervalos', min = 0.001, max = 0.1, value = 0.05, step = 0.001),
      numericInput('seed', label = 'Semilla para generar números pseudoaleatorios', value = 1234),
      checkboxInput('ribbon', 'Mostrar bandas de confianza', value = TRUE),
      actionButton('reset_input', 'Reset input')
    ),
    mainPanel(
      tabsetPanel(
#         tabPanel('Hist', plotOutput('random_hist')),
        tabPanel('Plot',
                 h5(textOutput('estim_riem')),
                 h5(textOutput('estim_trap')),
                 h5(textOutput('estim_MC')),
                 plotOutput('plot', width = '8in', height = '3in'),
                 plotOutput('errplot', width = '8in', height = '3in')
        ),
        tabPanel('Data', dataTableOutput('data'))
      )
    )
  )
})

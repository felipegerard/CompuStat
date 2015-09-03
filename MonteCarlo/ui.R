library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('Integración Trapezoidal vs. Integración MonteCarlo'),
    sidebarPanel(
      selectInput('fun', 'Función', list('sqrt(4-x^2)'=1,
                                         '4/(1+x^2)'=2,
                                         '6/sqrt(4-x^2)'=3),
                  4, selected = 1, multiple=F),
      sliderInput('N', label = 'Número de puntos/simulaciones', value = 50, min = 1, max = 1000, step = 1),
      sliderInput('mc.cores', label = 'Número de núcleos en paralelo', value = 6, min = 1, max = 8, step = 1),
      numericInput('a', 'a', value = 0),
      numericInput('b', 'b', value = 2),
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
                 plotOutput('plot', width = '9in', height = '6in')
#                  plotOutput('errplot', width = '8in', height = '3in')
        ),
        tabPanel('Data', dataTableOutput('data'))
      )
    )
  )
})

library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('Intervalos de confianza: Asintóticos vs. Bootstrap'),
    sidebarPanel(
      h4(withMathJax('$$E[\\varphi(X)] \\approx \\frac{1}{N} \\sum_{i=1}^N \\varphi(X_i)$$')),
      numericInput('n', 'Número de muestras iniciales (N)', value = 50),
      numericInput('B', 'Número de remuestras bootstrap (B)', value = 5000),
      numericInput('alpha', 'Nivel de significancia (1 - alpha)', value = 0.05),
#       textInput('dist',
#                 'Función para generar X, como función de n (e.g. runif(n), rgamma(n,2,3), rnorm(n,0,1))',
#                 value = 'runif(n)'),
      h5('Función para generar X, como función de n (e.g. runif(n), rgamma(n,2,3), rnorm(n,0,1), rweibull(n,0.5,1))'),
      tags$textarea(id = 'dist', rows = 5, cols=30,
"a <- runif(n)
ifelse(a < 0.1,
  rnorm(n,-5,1),
    rnorm(n,3,1))"),
      textInput('phi',
                'Función phi, como función de x (e.g. x -> mean(x), x - mean(x) -> var(x), etc.',
                value = 'x')
    ),
    mainPanel(
      plotOutput('hist_phix'),
      plotOutput('plot_boot')
    )
  )
})

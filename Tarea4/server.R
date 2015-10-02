library(shiny)



shinyServer(function(input, output){
  
  phix_ <- reactive({
    nsamp <- input$n
    dist <- function(n) eval(parse(text=input$dist))
    phi <- function(x) eval(parse(text=input$phi))
    x <- dist(nsamp)
    phi(x)
  })
  
  asint <- reactive({
    nsamp <- input$n
    phix <- phix_()
    theta_hat <- mean(phix)
    s <- sd(phix)
    list(theta_hat=theta_hat, sd=s)
  })
  
  boot <- reactive({
    nsamp <- input$n
    nboot <- input$B
    phix <- phix_()
    theta_hat <- mean(phix)
    thetas <- numeric(nboot)
    for(i in 1:nboot){
      phixb <- sample(phix, nsamp, replace = TRUE)
      thetas[i] <- mean(phixb)
    }
    s <- sd(thetas)
    list(thetas=thetas, theta_hat=theta_hat, sd=s)
  })
  
  ic <- reactive({
    nsamp <- input$n
    alpha <- input$alpha
    IC_BS <- data.frame(Estimate=boot()$theta_hat,
                        sd=boot()$sd,
                        LI=as.numeric(quantile(boot()$thetas, alpha/2)),
                        UI=as.numeric(quantile(boot()$thetas, 1 - alpha/2))) %>%
      mutate(IC_length = UI - LI)
      
    IC <- data.frame(Estimate=asint()$theta_hat,
                     sd = asint()$sd,
                     LI=asint()$theta_hat + qnorm(alpha/2)*asint()$sd/sqrt(nsamp),
                     UI=asint()$theta_hat + qnorm(1 - alpha/2)*asint()$sd/sqrt(nsamp)) %>%
      mutate(IC_length = UI - LI)
    aux <- data.frame(x=seq(min(boot()$thetas),max(boot()$thetas),l=100))
    aux$y <- dnorm(aux$x, asint()$theta_hat, asint()$sd/sqrt(nsamp))
    list(IC_BS=IC_BS, IC=IC, aux=aux)
  })

  output$hist_phix <- renderPlot({
    phix <- phix_()
    qplot(phix) +
      labs(title='Histograma de phi(X)')
  })

  
  output$plot_boot <- renderPlot({
    aux <- ic()$aux
    thetas <- data.frame(theta=boot()$thetas)
    ic_asint <- ic()$IC
    ic_boot <- ic()$IC_BS
    ic <- rbind(
      cbind(id='Asintóticos', ic_asint),
      cbind(id='Bootstrap', ic_boot)
    )
    ggplot() +
      geom_histogram(data=thetas, aes(theta, ..density..),
                     alpha=0.7,
                     fill='blue') +
      geom_line(data=aux, aes(x,y)) +
      geom_vline(data=ic, aes(xintercept=LI, color=id), linetype='dashed') +
      geom_vline(data=ic, aes(xintercept=UI, color=id), linetype='dashed') +
      scale_color_manual(guide='legend',values = c('black','blue')) +
      geom_vline(data=ic_asint, aes(xintercept=Estimate)) +
      geom_vline(data=thetas, aes(xintercept=mean(theta)), color='blue') +
      labs(title='Distribución e Intervalos: Bootstrap vs. Asintóticos')
  })
})

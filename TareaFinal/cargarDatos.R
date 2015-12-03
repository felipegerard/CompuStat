setwd("D:/Dropbox/Cursos impartidos/Estadística-Computacional/EM")
library(foreign)
library(mvtnorm)

table <- read.dta("datos_politicos.dta") # LEEMOS LOS DATOS DE FORMATO STATA
año <- 1986 # POR EL MOMENTO ESCOJAMOS UN SOLO AÑO PUES NO SABEMOS NADA DE DATOS PANEL
data <- table[table$year==año, ]
labels <- paste(names(data), attributes(data)$var.labels, sep=": ") # NOMBRES DE LAS VARIABLES

Y <- data$reg # INDICADORA DE SI SE ES O NO UNA DEMOCRACIA
list.depend <- c("level", "open", "g", "strikes", "govpwt", "ls", "invpwt",
                 "fertil", "lfagric", "popg", "femsec", "EnergProd") # VARIABLES ECONÓMICAS EXPLICATIVAS

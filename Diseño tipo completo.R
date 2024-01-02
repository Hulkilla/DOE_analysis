library(readxl)
library(knitr)
library(tidyverse)
library(broom)
theme_set(theme_classic())
library(car)
library(rsm)
library(ggfortify)

################## IMPORTACIÓN DE LOS DATOS DEL MODELO #####################

DOE <- read_excel("DOE KOH 10%.xlsx") 

DOE$Temperature = as.numeric(DOE$Temperature)


# Los factores a optimizar

#-   x1 = Temperatura (ºC)
#-   x2 = Tiempo (h)
#-   x3 = Molaridad KOH (M)

DOE_coded_data = coded.data(
  DOE, 
  formulas = list(
    x1 ~ (Temperature - 45)/15,
    x2 ~ (Time - 14)/10,
    x3 ~ (KOH - 0.625)/0.375
  ))

Variables=c("Temperatura", "Tiempo", "KOH (M)")

minx1 = min(DOE_coded_data$x1)
minx2 = min(DOE_coded_data$x2)
minx3 = min(DOE_coded_data$x3)
maxx1 = max(DOE_coded_data$x1)
maxx2 = max(DOE_coded_data$x2)
maxx3 = max(DOE_coded_data$x3)


################## PREPARACIÓN DEL MODELO #####################

Model <- rsm((`TOC content (%)`) ~ SO(x1, x2, x3), data = DOE_coded_data)

summary(Model)

################## OPTIMIZACIÓN TIPO #####################

#Como viene de un diseño exportado que no ha sido creado con el propio programa de R, es mejor ignorar este ajuste y hacer una optimización con la función optim

opt_point_coded <- summary(Model)$canonical$xs

opt_point_coded[1] = minx1

op_point <- code2val(opt_point_coded,                     
                     codings = codings(DOE_coded_data)  
                     )

df11 = data.frame(round(op_point,2))

df11

opt_point_df <- data.frame(x1 = opt_point_coded[1],  # predict() needs a data frame with the points 
                           x2 = opt_point_coded[2],  # to be predicted 
                           x3 = opt_point_coded[3]
                           )

best_response <- predict(Model,          # Our model
                         opt_point_df    # Data frame with points to be predicted 
                         )

best_response #Valor en las condiciones óptimas de la variable de respuesta

################## FÓRMULA CUADRÁTICA OPTIMIZACIÓN #####################

formula_model = function(x) -(Model$coefficients[1] + 
                               Model$coefficients[2] * x[1] + 
                               Model$coefficients[3] * x[2] + 
                               Model$coefficients[4] * x[3] + 
                               Model$coefficients[5] * x[1] * x[2] + 
                               Model$coefficients[6] * x[1] * x[3] + 
                               Model$coefficients[7] * x[2] * x[3] + 
                               Model$coefficients[8] * x[1]^2 + 
                               Model$coefficients[9] * x[2]^2 + 
                               Model$coefficients[10] * x[3]^2)

optim(c(0,0,0), formula_model, method = "L-BFGS-B", lower = c(minx1,minx2, minx3), upper = c(maxx1,maxx2, maxx3)) -> Model_optimo

R1 = Model_optimo$par[1]*15+45
R2 = Model_optimo$par[2]*10+14
R3 = Model_optimo$par[3]*0.375+0.675

optimos_variables = round(c(R1,R2,R3),2) 
optimos_variables_2 = Model_optimo$par

df12 = data.frame(Variables, optimos_variables)

df12

opt_point_df_2 <- data.frame(  # predict() needs a data frame with the points 
  x1 = Model_optimo$par[1],         # to be predicted 
  x2 = Model_optimo$par[2],
  x3 = Model_optimo$par[3]
)

best_response_2 <- predict(
  Model,             # Our model
  opt_point_df_2             # Data frame with points to be predicted 
)

best_response_2 #Valor en las condiciones óptimas de la variable de respuesta

################# ANÁLISIS DE LOS RESIDUOS ####################

plot(Model, 1)
plot(Model, 2)
plot(Model, 3)
plot(Model, 4)
autoplot(Model)

#Residuos
res=Model$residuals

#Shapiro-Wilks
x.test <- shapiro.test(res)
x.test
p_value_thresh=0.05

#Definiendo la funcion que me devuelve si se cumple o no la hipotesis
sw_test_results <- function(x.test,p_value_thresh) {
  if(x.test$p.value > p_value_thresh){
    print('Assumption satisfied')
    } 
  else {
    print('Assumption not satisfied')
    print('Confidence intervals will likely be affected')
    print('Try performing nonlinear transformations on variables')
  }
}

#Probando la funcion
sw_test_results(x.test,p_value_thresh)


plotn <- function(x,main="Histograma de frecuencias \ny distribuci?n normal",
                  xlab="X",ylab="Densidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
}

plotn(res,main="Distribuci?n normal")


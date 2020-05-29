install.packages("neuralnet")
install.packages("readxl")

library(neuralnet)
library(readxl)
#datos.df=read.csv(file.choose(),T)

setwd("C:/Users/juanj/Desktop")

entrenamiento = read_excel("ejemplo.xlsx", sheet = "Sheet1")

prueba = read_excel("ejemplo.xlsx", sheet = "Sheet2")

#creacion de red neuronal
#relacion infarto por medio edad, con los triricericos
#tomamos los datos de la hoja1 de entrenamiento
#usamos 4 capas ocultas
#funcion activacion logisitca, en la clasificacion binomial
#dependencia entre variables independientes y dependientes es falso


rn = neuralnet(infarto~edad+T, data = entrenamiento, hidden = 20, act.fct= "logistic", linear.output = F) 

#ver red neuronal
plot(rn)

#Prediccion
rnprediccion = compute(rn, prueba)
a =rnprediccion$net.result
sino =ifelse(a>0.5,1,0)
sino

View(entrenamiento)
View(prueba)

install.packages("tidyverse")
library(tidyverse)
library(stats)
library(readr)
library(ggplot2)

vinos.df<-read_csv("C:/Users/juanj/Downloads/vinos.csv")
head(vinos.df, 10)


attach(vinos.df)
plot(vinos.df)

#correlacion

cor(vinos.df)

#regresionlineal

regresion=lm(vinos.df$alcohol~vinos.df$density, data=vinos.df)
regresion

par(mfrow=c(2,2))
plot(regresion)
e<-residuals(regresion)
sregresion=summary(regresion)
d<-e/sregresion$sigma

hist(d, probability=T, main="l", xlim=c(-3,3))
d.seq<-seq(-3, 3, length=50)
lines(d.seq, dnorm(d.seq, mean(d),  sd(d)))

#prueba shapiro
shapiro.test(e)

#paqueteria para pruebas de homosteacidad
install.packages("lmtest")

library(lmtest)



bptest(regresion)
#mayor a 0.05, nuestros recidos son heterogenieps

#studentized Breusch-Pagan test

#data:  regresion
#BP = 0.00096557, df = 1, p-value = 0.9752

dwtest(regresion, alternative="two.sided")

#Durbin-Watson test

#data:  regresion
#DW = 1.4039, p-value = 0.02524
#alternative hypothesis: true autocorrelation is not 0

#estan correlacionadas
#pero solo abarca el 65 por ciento, eso significa que no abarca mucho 
#nuestra hipotesis es nula

mean(e)

step(regresion)

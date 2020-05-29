

modelo<-lm(volatile.acidity ~ citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol + fixed.acidity, data = datos)
summary(modelo)

#
modelo<-lm(volatile.acidity ~ citric.acid + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + alcohol + fixed.acidity, data = datos)
summary(modelo)

#prueba de normalidad
e<-residuals(modelo)
shapiro.test(e)


par(mfrow=c(2,2))
>plot(modelo)
library(lmtest)

#prueba de heterogenialidad
bptest(modelo)

#prueba de correlacion
dwtest(modelo)


modelo2<-lm(alcohol~ citric.acid + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + fixed.acidity, data = datos2)
summary(modelo2)

#prueba de normalizacion
e<-residuals(modelo2)
shapiro.test(e)


#prueba de hetero
bptest(modelo2)

#prueba de correlacion
#menor a 0.05 significa que no pasa
dwtest(modelo2)


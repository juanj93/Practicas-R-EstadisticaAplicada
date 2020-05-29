install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot")
library(ggplot2)
install.packages("arules")
library(arules)
datos.df=read.csv(file.choose(), T)
install.packages("ISLR")
library(ISLR)
head(datos.df, 3)

datos_slit<-split(x=datos.df$item, f=datos.df$id_compra)
transacciones<-as(datos_slit, Class="transactions")

transacciones

tamanos<-size(transacciones)
summary(tamanos)


quantile(tamanos, probs=seq(0,1,0.1))

install.packages("ROCR")
library(ROCR)
install.packages("Matrix")
library(Matrix)
install.packages("caret")
library(caret)

install.packages("magrittr")
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)

#primera parte
frecuencia<-itemFrequency(x=transacciones, type="relative")

frecuencia%>%sort(decreasing = TRUE)%>%head(5)

soporte<-30/dim(transacciones)[1]
itemsets<-apriori(data=transacciones, parameter=list(support=soporte, minlen=1, maxlen=20, target="frequent itemset"))
summary(itemsets)

top_20<-sort(itemsets, by="support", decreasing = TRUE)[1:20]
inspect(top_20)
#combinaciones
inspect(sort(itemsets[size(itemsets)>1], decreasing = TRUE)[1:20])

reglas<-apriori(data=transacciones, parameter = list(support=soporte, confidence=0.70, target="rules"))
summary(reglas)
reglas
metricas<-interestMeasure(reglas, measure = c("coverage","fishersExactTest"), transactions = transacciones)
metricas
df_reglas<-as(reglas, Class="data.frame")
df_reglas
itemsets_filtro<-arules::subset(itemsets,subset=items %in% "newspapers")
itemsets_filtro
summary(itemsets_filtro)
itemsets_filtro<-arules::subset(itemsets,subset=items %ain% c("newspapers", "whole milk"))

inspect(sort(itemsets_filtro, decreasing = TRUE)[1:16])

inspect(reglas[1:10])

install.packages("tidyverse")
library(tidyverse)
library(stats)
library(readr)
library(ggplot2)

datos.df<-read_csv("C:/Users/juanj/Downloads/Creditoscard.csv")
head(datos.df, 10)

names(datos.df) = c("ID", "Age", "Experience", "Income", "ZIP.Code", "Family", "CCAvg", "Education", "Mortgage", "Personal.Loan", "Securities.Account", "CD.Account", "Online", "CreditCard")

set.seed(1)
train<-sample(x=1:5000,3000)
i_train<-sample(x=nrow(datos.df), size=0.6*(nrow(datos.df)),replace=FALSE)
datos.train<-datos.df[i_train, ]
datos.test<-datos.df[i_train, ]
library(dplyr)

modelo_rlog<-glm(Securities.Account ~ Age + Experience+ Income+ ZIP.Code+ Family +CCAvg+ Education+ Mortgage+ Personal.Loan +CD.Account+ Online+ CreditCard, data=datos.df, subset=i_train, family="binomial")

#te muestra el modelo 
summary(modelo_rlog)

#prediccion
#object modelo matematico, el type va por cada registr, newdata, la base de datos de prueba(los index)
predictTest=predict(object=modelo_rlog, type="response", newdata=datos.test)

table(datos.test$Securities.Account, predictTest>0.2)

table(datos.test$Securities.Account, predictTest>0.8)

table(datos.test$Securities.Account, predictTest>0.5)

table(datos.test$Securities.Account, predictTest>0.3)

#libreria que calcula la curva rocr(sencibilidad) 
install.packages("ROCR")

library(ROCR)

#predition parte de la libreria rorc
predictionROCR=prediction(predictTest, datos.test$Securities.Account)

#libreria rorc, para calcular todas las preducciones
ROCRPerfL = performance(predictionROCR, "tpr", "fpr")

#Crea el grafico utilizando rocrperf haciendo la curva de sencibilidad, haciendo una curva arcoiris
plot(ROCRPerfL, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

#aucurasi, para saber el area debajo de la curva rorc
auc = as.numeric(performance(predictionROCR,"auc")@y.values)

#vemos el area
auc

#OTRA REGRESION LOGISTICA

#######################################################################

dim(train)

dim(datos.test)

regresion_log<-glm(Personal.Loan~.,data = datos.train, family = "binomial")

summary(regresion_log)

predictTest_2=predict(object=regresion_log, type="response", newdata=datos.test)

table(datos.test$Personal.Loan, predictTest_2>0.2)

predictionROCR_2=prediction(predictTest_2, datos.test$Personal.Loan)

ROCRPerfL = performance(predictionROCR_2, "tpr", "fpr")

plot(ROCRPerfL, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

auc_2 = as.numeric(performance(predictionROCR_2,"auc")@y.values)

auc_2

########

datos.df=read.csv(file.choose(),T)

head(datos.df,5)

train.index<-sample(c(1:dim(datos.df)[1]), dim(datos.df)[1]*0.6)

train<-datos.df[train.index, c(1:12)]

selected.var<-c(1:12)

train<-datos.df[train.index, selected.var]

valid<-datos.df[-train.index, selected.var]

dim(train)

dim(valid)

bayes<-naiveBayes(Personal.Loan~., data=train)

bayes

summary(bayes)



pred.classt<-predict(bayes, newdata = valid)

library(ggplot2)

library(caret)

confusionMatrix(pred.classt, valid$Personal.Loan)








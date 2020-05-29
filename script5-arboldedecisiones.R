install.packages("tidyverse")
install.packages("rpart")
install.packages("rpart.plot")
library(rpart.plot)
install.packages("caret")
library(caret)
install.packages("ggplot2")
library(ggplot2)
install.packages("ISLR")
library(ISLR)

datos.df=read.csv(file.choose(),T)
head(datos.df, 5)

selected.var<-c(1:12)

train.index<-sample(c(1:dim(datos.df)[1]), dim(datos.df)[1]*0.6)
train<-datos.df[train.index, selected.var]
valid<-datos.df[-train.index, selected.var]

dim(train)
dim(valid)

library(rpart)
library(rpart.plot)
library(caret)


arbol<-rpart(formula=Personal.Loan~., data=train)
arbol

rpart.plot(arbol)
head(train, 5)

prediction<-predict(arbol, newdata = valid, type="class")

confusionMatrix(prediction, valid[["Personal.Loan"]])


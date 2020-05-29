install.packages("tidyverse")
library(tidyverse)
library(stats)
library(readr)
library(ggplot2)

#Acceso a la base de datos

vinos.df<-read_csv("C:/Users/juanj/Downloads/vinos.csv")
head(vinos.df, 10)

#Grafico de tablas
plot(vinos.df$"volatile acidity", type="l", col="purple")
plot(vinos.df$"volatile acidity", type="b", col="purple")
plot(vinos.df$"alcohol", type="o", col="blue", ylim=c(3, 15))
lines(vinos.df$"fixed acidity", type="o", col="green", pch=22, lty=2)
plot(vinos.df)
plot(vinos.df$"alcohol", vinos.df$"density")
plot(vinos.df$"alcohol", vinos.df$"density", main=diagrama, abline("juaUlises"))
plot(vinos.df$"alcohol", vinos.df$"density")
hist(vinos.df$"pH", col="red")

ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point(aes(shape=iris$Species))
ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point(aes(color=iris$Species, shape=iris$Species, size=3))
qplot(Sepal.Length, data=iris)
qplot(Sepal.Length, data=iris, bis=15, geom="histogram") + geom_vline(xintercept = mean(iris$Sepal.Length), color="yellow") + geom_vline(xintercept = median(iris$Sepal.Length), color="blue")

qplot(Sepal.Length, data=iris, geom="density", color=Species)
barplot(table(iris$Species))
barplot(table(iris$Species), col="blue")
barplot(table(iris$Species), col=c("blue","green","yellow"))
boxplot(iris)

pie(table(iris$Species), col=c("red","blue","green"))

pie(table(iris$Species), col=c("red","blue","green"))
x=(vinos.df$"ahcohol")
x=(vinos.df$"alcohol")
par(mfrow=c(2,2))
pie(x)
barplot(x)
hist(x)
boxplot(x)

x<-c(1,3,5,7,9)
y<-c(2,4,6,8)
z<-matrix(runif(20,1,10),5,4)
contour(x,y,z)
filled.countour(x,y,z)


persp(x,y,z, theta=30, phi=30, col="green")


par(mfrow=c(1,1))
persp(x,y,z, theta=30, phi=30, col="green")

#usando base de datos Creditoscard.csv
credicard.df<-read_csv("C:/Users/juanj/Downloads/Creditoscard.csv")        
head(credicard.df, 10)
barplot(table(credicard.df$Education))
barplot(table(credicard.df$Education), col=c("blue","yellow","red"))
barplot(table(credicard.df$Education), col=c("blue","yellow","red"), horiz = TRUE)
plot(credicard.df$Age)
plot(credicard.df$Education)
hist(credicard.df$Age)
hist(credicard.df$Age, col="red")
boxplot(credicard.df$Age)
pie(credicard.df$Age)
pie(credicard.df$Education)
pie(credicard.df$CreditCard)
boxplot(credicard.df)

plot(vinos.df$"alcohol", vinos.df$"density")
abline(lm(vinos.df$density~vinos.df$alcohol), col="red")

data("USArrests")
head(USArrests)


apply(X=USArrests, MARGIN = 2, FUN= mean)
apply(X=USArrests, MARGIN = 2, FUN= var)

apply(X=USArrests, MARGIN = 2, FUN= mean)
apply(X=USArrests, MARGIN = 2, FUN= var)
pca<-prcomp(USArrests, scale= TRUE)
names(pca)
pca$scale
pca$rotation
pca$x
biplot(x=pca, scale=0, cex=0.6, col=c("blue","brown"))
pca$rotation<--pca$rotation
biplot(x=pca, sclae=0, cex=0.6, col= c("blue", "brown"))

ggplot(data = data.frame(varianza, pc = 1:4),
      aes(x = pc, y = varianza)) +
  geom_col(width = 0.3) +
     scale_y_continuous(limits = c(0,1)) +
   theme_bw() +
     labs(x = "Componente principal",
      y = "Prop. de varianza explicada")


varianza_a=cumsum(varianza)

ggplot(data = data.frame(varianza_a, pc = 1:4),
       aes(x = pc, y = varianza_a, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")



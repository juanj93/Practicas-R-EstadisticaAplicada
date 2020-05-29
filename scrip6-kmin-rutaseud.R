install.packages("tidyverse")
install.packages("ggplot2")
install.packages("caret")
library(caret)
library(ggplot2)
library(ISLR)
library(tidyverse)

install.packages("factoextra")
library(factoextra)

install.packages("factoextra")
library(factoextra)

data("USArrests")
head(USArrests)

str(USArrests)
datos<- scale(USArrests)

library(cluster)
library(factoextra)

fviz_nbclust(x=datos, FUNcluster = pam, method="wss", k.max=15, diss=dist(datos, method="manhattan"))
#en este caso se elegiria entre 4 o 5 clusters por la cercania a los demas puntos

pam_cluster<-pam(x=datos, k=4, metric="manhattan")
pam_cluster

fviz_cluster(object=pam_cluster, data=datos, ellipse.type = "t", repel = TRUE) + theme_bw() + labs(title="Resultados") +theme(legend.position = "none")

mat_dist<-dist(x=datos, method="euclidean")
#hacer distancias euclideanas

hc_euclidea_completa<-hclust(d=mat_dist, method="complete")

hc_euclidea_average<-hclust(d=mat_dist,method="average")

fviz_dend(x=hc_euclidea_completa, k=2, cex=0.6) + geom_hline(yintercept = 5.5, linetype="dashed") + labs(title="dendograma")
#se dividira en 60 por ciento

#iteracion
hkmeans_cluster<-hkmeans(x=datos, hc.metric = "euclidean", hc.method = "complete", k=4)

hkmeans_cluster

fviz_cluster(object=hkmeans_cluster, pallete="jco", repel = TRUE) +theme_bw() +labs(title="Hierarchical k-means Clustering")

fviz_cluster(object=hkmeans_cluster, pallete="jco", repel = TRUE) +theme_bw() +labs(title="Hierarchical k-means Clustering")

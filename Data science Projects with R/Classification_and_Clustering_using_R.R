#Cluster analysis


#data preparation

str(iris)
set.seed(1234)
new <- sample(1:dim(iris)[1], 15)
data <- iris[new,]  
library(psych)
pairs.panels(data[-5], gap=0,
             color = c("red","yellow", "blue")[data$Species],
             pch = 21)
summary(data)

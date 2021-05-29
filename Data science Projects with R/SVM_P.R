 #Complete seperation of datapoints

set.seed(10)
#below is used to create a dummy data ignore madi
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] +3/2
data <- data.frame(x=x, y=as.factor(y))
View(data)

#below to plot the grph to se the seperation
library(ggplot2)
ggplot(data = data, aes(x = x.2, y = x.1, color = y, shape = y)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#000000", "#FF0000"))

library(e1071)  # used for SVM
m1 <- svm(y~., kernel = "linear",
          scale = TRUE, data = data)
summary(m1)


#visualise

plot(m1, data)

#prediction
p1 <- predict(m1, data)
tab <- table(predicted = p1, Actual = y)
tab

#confusion matrix
accuracy <- sum(diag(tab))/sum(tab)
accuracy

#data overlap
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
data1 <- data.frame(x=x, y=as.factor(y))

ggplot(data = data1, aes(x = x.2, y = x.1, color = y, shape = y)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#000000", "#FF0000"))


m2 <- svm(y~., kernel = "linear",
          scale = TRUE, data = data1)
summary(m2)

#visualise
plot(m2, data1)

#prediction
p2 <- predict(m2, data1)
tab <- table(predicted = p2, Actual = y)
tab

#confusion matrix
accuracy <- sum(diag(tab))/sum(tab)
accuracy


#Model tuning

m4 <- tune(svm, y~., data = data1, kernel = "linear",
           ranges = list(cost = c(0.01,0.1,1,5,10,100)))

#best model
bm <- m4$best.model
bm
summary(m4) #see for the lowest error and least cost value 
###-----------####------------##-------------###

x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] <- x[1:100,] + 2.5
x[101:150,] <- x[101:150,] - 2.5
y <- c(rep(1,150), rep(2,50))
data2 <- data.frame(x=x, y=as.factor(y))
View(data2)

ggplot(data = data2, aes(x = x.2, y = x.1, color = y, shape = y)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#000000", "#FF0000"))

#below we consider default value if gama=0.5 and cost=1 and see the change
#m5 <- svm(y~., data = data2, kernel = "radial", gamma = 0.5, cost = 1)
#m5 <- svm(y~., data = data2, kernel = "radial", gamma = 5, cost = 1)

m5 <- svm(y~., data = data2, kernel = "radial", gamma = 5, cost = 1)

#kernel=radial by default
?svm

plot(m5, data2)

#model tuning

m7 <- tune(svm, y~., data = data2,
           ranges = list(cost = c(0.1,1,10,100,1000),
                         gamma = c(0.05, 0.1, 0.5, 1, 2, 3,4)))

#best model
bm1 <- m7$best.model
bm1
summary(m7)

p11 <- predict(m5, data2) #after tuning select the best gamma and cost 
tab <- table(predicted = p11, Actual = y)
tab

#confusion matrix
accuracy <- sum(diag(tab))/sum(tab)
accuracy
#------------------####-----------------##

x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0,50))
x[y==0,2] <- x[y==0,2] + 2.5
data3 <- data.frame(x=x, y=as.factor(y))
View(data3)

ggplot(data = data3, aes(x = x.2, y = x.1, color = y, shape = y)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#000000", "#FF0000", "#00BA00"))

m8 <- svm(y~., data = data3, kernel = "radial",
          cost = 1, gamma = 1)
summary(m8, data3)

plot(m8, data3)

#model tuning

m9 <- tune(svm, y~., data = data3,
           ranges = list(cost = c(0.1,1,10,100,1000),
                         gamma = c(0.05, 0.1, 0.5, 1, 2, 3, 4)))

#best model
bm2 <- m9$best.model
bm2
summary(m9)

p22 <- predict(m8, data3) #after tuning select the best gamma and cost 
tab <- table(predicted = p22, Actual = y)
tab

#confusion matrix
accuracy <- sum(diag(tab))/sum(tab)
accuracy
#-------------------####-------------------------##


View(iris)
data("iris")
attach(iris)

dim(iris)
colnames(iris)
plot(Sepal.Width~Sepal.Length, col = Species)

plot(Petal.Length~Petal.Width, col = Species)

library(tidyverse)
#or alternate ggplot
qplot(Sepal.Length, Sepal.Width, data = iris, color = Species)

qplot(Petal.Length, Petal.Width, data = iris, color = Species)

library(e1071)
m1 <- svm(Species~ Sepal.Length + Sepal.Width, data = iris, kernel = "radial", 
          cost = 1, gamma = 0.5 )

plot(m1, iris, Sepal.Width ~Sepal.Length, 
     slice = list(Sepal.Width = 1, Sepal.Length = 1))

summary(m1)

#prediction
p1 <- predict(m1, iris)
tab <- table(predicted = p1, Actual = iris[,5])
tab

#confusion matrix
accuracy <- sum(diag(tab))/sum(tab)
accuracy   #82 %

#model tuning
set.seed(123)  #for tunning we do, else the values change
tune1 <- tune(svm, Species~., data = iris,
           ranges = list(cost = c(0.1,1,10,100,1000), 
                         gamma = c(0.5, 1, 2, 3, 4)))
summary(tune1)
#best model
tune1$best.model

#including all variable to improve accuracy
#linear or sigmoid or radial
library(e1071)
m2 <- svm(Species~ ., data = iris, kernel = "radial", 
          cost = 1, gamma = 0.5)

plot(m2, iris, Petal.Width ~Petal.Length, 
     slice = list(Sepal.Width = 2, Sepal.Length = 8))

summary(m1)

#prediction
p2 <- predict(m2, iris[,-5])
tab <- table(predicted = p2, Actual = iris[,5])
tab

#confusion matrix
accuracy <- sum(diag(tab))/sum(tab)
accuracy

#model tuning
set.seed(123)  #for tunning we do, else the values change
tune2 <- tune(svm, Species~., data = iris,
              ranges = list(cost = c(0.1,1,10,100,1000),
                            gamma = c(0.5, 1, 2, 3, 4)))
summary(tune2)
#best model
tune1$best.model

#------------------------#####---------------------------##

setwd("C:/DataScience/DSP13")
getwd()
social <- read.csv("Social_Network_Ads.csv")
View(social)
dim(social)
library(caTools)
set.seed(123)
split =sample.split(social, SplitRatio = 0.7)
train =subset(social,split==TRUE)
test = subset(social,split==FALSE)

str(train)
train$Purchased <- as.factor(train$Purchased)
colSums(is.na(test))
attach(train)
plot(Age + EstimatedSalary, col = Purchased)

library(tidyverse)
#or alternate ggplot
qplot(Age, EstimatedSalary, data = train, color = Purchased)

install.packages("e1071")
library(e1071)
m1 <- svm(Purchased~ Age + EstimatedSalary, data = social, 
          kernel = "radial", 
          cost = 5, gamma = 0.5 )

plot(m1,data = train, Age ~EstimatedSalary)

summary(m1)

#prediction
p1 <- predict(m1, train[,-4])
tab <- table(predicted = p1, Actual = train[,4])
tab

#confusion matrix
accuracy <- sum(diag(tab))/sum(tab)
accuracy

#model tuning
set.seed(123)  #for tunning we do, else the values change
tune1 <- tune(svm, Purchased~., data = train,
              ranges = list(cost = c(0.1,1,10,100,1000),
                            gamma = c(0.5, 1, 2, 3, 4)))
#best model
tune1$best.model

#for test data

test$Purchased <- as.factor(test$Purchased)

m2 <- svm(Purchased~ Age + EstimatedSalary, data = test, 
          kernel = "radial", 
          cost = 5, gamma = 0.5 )
p2 <- predict(m2, test[,-4])
tab <- table(predicted = p2, Actual = test[,4])
tab

#confusion matrix
accuracy <- sum(diag(tab))/sum(tab)
accuracy

#------------------####-------------------------------#


heart <- read.csv("F:/DSP13/Heart.csv")
View(heart)

colSums(is.na(train))
colSums(train == "")

library(caTools)

samplesize <- floor(0.75 * nrow(heart))
set.seed(123)
train_ind <-sample(seq_len(nrow(heart)), size = samplesize)

train <- heart[train_ind, ]
test <- heart[-train_ind, ]


str(train)
colSums(is.na(train))
train$num <- as.factor(train$num)
colSums(is.na(test))
attach(train)

library(e1071)
#or alternate ggplot
m1 <- svm(num~ ., data = train, 
          kernel = "radial",
          cost = 2, gamma = 0.5)

summary(m1)
dim(train)

#prediction
p1 <- predict(m1, train)
tab <- table(predicted = p1, Actual = train$num)
tab

#confusion matrix
accuracy <- sum(diag(tab))/sum(tab)
accuracy

#model tuning
set.seed(123)  #for tunning we do, else the values change
tune1 <- tune(svm, num~., data = train,
              ranges = list(cost = c(0.1,1,10,100,1000),
                            gamma = c(0.5, 1, 2, 3, 4)))
#best model
tune1$best.model

#for test data
test$num <- as.factor(test$num)

str(test)

p2 <- predict(m1, newdata = test)
tab <- table(predicted = p2, Actual = test$num)
tab

#confusion matrix
accuracy <- sum(diag(tab))/sum(tab)
accuracy

#tuning
set.seed(123)  #for tunning we do, else the values change
tune1 <- tune(svm, num~., data = test,
              ranges = list(cost = c(0.1,1,10,100,1000),
                            gamma = c(0.5, 1, 2, 3, 4)))
#best model
tune1$best.model


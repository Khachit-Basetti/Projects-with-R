data <- read.csv("F:/DSP13/CTGAll.csv") #
View(data)
colSums(is.na(data))

colSums(data == "")
dim(data)
str(data)

data$NSP <- as.factor(data$NSP)
table(data$NSP) #Normal,Suspect, Pathologic
dim(data)

set.seed(123)
sample <- sample(nrow(data), nrow(data)*0.70)
train <- data[sample,]
test <- data[-sample,]

#install.packages("randomForest")
library(randomForest)

attach(data)

set.seed(222)
m1 <- randomForest(NSP~., data = train) #No. of variables tried at each split -> mtry
m1
attributes(m1)  #ntree and mtry we will use to tune

pred <- predict(m1, train, type = "class")
tab <- table(pred, train$NSP)
tab
acc <- sum(diag(tab))/sum(tab)
acc

#for test
pred<- predict(m1, newdata = test, type = "class")
tab <- table(pred, test$NSP)
acc <- sum(diag(tab))/sum(tab)
acc

plot(m1) 

set.seed(111)
tuneRF(train[,-22], train[,22],
       stepFactor = 0.5, plot = T, ntreeTry = 300, trace = T, improve = 0.05)
set.seed(111)
m2 <- randomForest(NSP~., data = train, ntree = 300, mtry = 8,type = "class")
m2

#for test, train will not change
pred1<- predict(m2, newdata = test)
tab <- table(pred1, test$NSP)
tab
acc <- sum(diag(tab))/sum(tab)
acc


#more understanding
hist(treesize(m2), col = "red")
varImpPlot(m2) #graph where we decide the best variable
importance(m2)
varUsed(m2)

#partial dependence plot

partialPlot(m2, train, ASTV, 1) #Normal=1; Suspect=2; Pathologic=3		
partialPlot(m2, train, ASTV, 2)
partialPlot(m2, train, ASTV, 3)

getTree(m2, 1, labelVar = T) # 1 represents the first tree out of 300 according tho this model

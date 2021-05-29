#install.packages("party")
library(party)
library(rpart)
library(rpart.plot)
data("readingSkills")
View(readingSkills)
attach(readingSkills)
dim(readingSkills)
data <- readingSkills
#flowchart
tree1 <- ctree(nativeSpeaker ~., data = data)
tree1
plot(tree1)

#alternate way to represent in  flowchart format
tree2 <- rpart(nativeSpeaker ~., data = data)
rpart.plot(tree2, cex = 0.6)


predict(tree1, data)
tab <- table(predict(tree1), data$nativeSpeaker)

aucc <- sum(diag(tab))/sum(tab)
aucc

#---------------------###----------------------------##

data <- read.csv("F:/DSP13/titanic_train.csv")
View(data)
colSums(is.na(data))
colSums(data == "")
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = T)
data$Embarked[data$Embarked == ""] <- "S"
data$Survived <- as.factor(data$Survived)
attach(data)

Tree1 <- rpart(Survived ~ Sex+SibSp+Parch+Fare+Embarked, data = data)
rpart.plot(Tree1, cex = 0.8)

pred <- predict(Tree1, data, type = "class")

tab <- table(prediction = pred, Actual = data$Survived)
tab
aucc <- sum(diag(tab))/sum(tab)  ####Accuracy = 81%
aucc

#using logistic to compare accuracy

model1 <- glm(Survived ~Sex+SibSp+Parch+Fare+Embarked,
              family=binomial, data=data)
summary(model1)

##McFadden's pseudo-R2
RsqLogit<-1-(model1$deviance/model1$null.deviance)
RsqLogit

library(caret)

predicttrain<-predict(model1,type = 'response')
predicttrain
tab<- table(data$Survived, predicttrain >= 0.5) #cutoff is predicttrain
tab 

sum(diag(tab))/sum(tab)  #accuracy = 79%

#For SVM

library(e1071)
#or alternate ggplot
data$Survived <- as.factor(data$Survived)
m1 <- svm(Survived~ Sex+SibSp+Parch+Fare+Embarked, data = data, 
          kernel = "radial")

summary(m1)
dim(data)
str(data)
#prediction
p1 <- predict(m1, data)
p1
dim(p1)
tab <- table(prediction = p1, Actual = data$Survived)
tab

#confusion matrix
accuracy <- sum(diag(tab))/sum(tab)  #Accuracy = 80%, hence decision tree is better
accuracy   

#-------------------------###----------------------##

data("kyphosis")
attach(kyphosis)
View(kyphosis)
data <- kyphosis
View(data)
?kyphosis
library(rpart)
library(rpart.plot)
colSums(is.na(data))
colSums(data == "")
tree1 <- rpart(Kyphosis ~., data = data)
rpart.plot(tree1)

dim(data)


pred <- predict(tree1, data, type = "class")

tab <- table(prediction = pred, Actual = data$Kyphosis)
tab
aucc <- sum(diag(tab))/sum(tab)  ####Accuracy = 81%
aucc

#Alternate flowchart
Tree2 <- ctree(Kyphosis ~., data = data)
plot(Tree2)
#--------------------###------------------------------###------

data <- read.csv("F:/DSP13/diabetes.csv")
dim(data)
View(data)

colSums(is.na(data))
colSums(data =="")
attach(data)

data$Outcome <- as.factor(data$Outcome)

tree1 <- rpart(Outcome ~., data = data)
rpart.plot(tree1, cex = 0.7)

pred <- predict(tree1, data, type = "class")
tab <- table(prediction = pred, Actual = data$Outcome)
tab
aucc <- sum(diag(tab))/sum(tab)  ####Accuracy = 81%
aucc

#----------------------#####----------------------##


data <- read.csv("F:/DSP13/CTG.csv")
dim(data)
View(data)

colSums(is.na(data))
colSums(data =="")
attach(data)

library(caTools)
samplesize <- floor(0.70 * nrow(data))
set.seed(123)
train_ind <-sample(seq_len(nrow(data)), size = samplesize)
train <- data[train_ind, ]
test <- data[-train_ind, ]

dim(train); dim(test)

attach(train)
train$NSP <- as.factor(train$NSP)
levels(NSP)
library(party)
tree1 <- ctree(NSP~., data = train)
tree1
plot(tree1)

tree1<- ctree(NSP~., data = train,
              controls = ctree_control(mincriterion = 0.99, minsplit = 50))
#minsplit=50, means it split only if the box variables are more than 50, else it is not
plot(tree1)

str(train)
predict(tree1, test, type = "prob")
predict(tree1, test, type = "response")

##predict train data
pred <- predict(tree1, test)

tab <- table(prediction = predict(tree1), Actual = train$NSP)
tab
aucc <- sum(diag(tab))/sum(tab)  ####Accuracy = 79.64%
aucc
1-aucc   #misclassification

##predict test data
str(test)

tab <- table(prediction = pred, Actual = test$NSP)
tab
aucc <- sum(diag(tab))/sum(tab)  ####Accuracy = 84.50
aucc
1-aucc 

####--------------------####--------------------##

library(rpart)
HR <- read.csv("F:/DSP13/HR.csv")
View(HR)
colSums(is.na(HR))
colSums(HR =="")
dim(HR)

set.seed(123)
sample <- sample(nrow(HR), nrow(HR)*0.70)
train <- HR[sample,]
test <- HR[-sample,]

str(HR)
tree1 <- rpart(left~., data = train, method = "class", control = rpart.control(cp=0))

plot(tree1)

text(tree1, pretty = 0) #preety is for labelling

#alternate way for plotting flowchart
library(rattle)
library(RColorBrewer)
fancyRpartPlot(tree1)

printcp(tree1)
plotcp(tree1)
tree1$cptable[which.min(tree1$cptable[,"xerror"]), "CP"]

#Accuracy of train
pred <- predict(tree1, train, type = "class")
tab <- table(pred, train$left)
acc <- sum(diag(tab))/sum(tab)

#accuracy of test
pred <- predict(tree1, test, type = "class")
tab <- table(pred, test$left)
acc <- sum(diag(tab))/sum(tab)

tree2 <- rpart(left~ ., data = train, method = "class",
               control = rpart.control(cp = 0.002012882, maxdepth = 8, minsplit = 100))
pred <- predict(tree2, train, type = "class")
tab <- table(pred, train$left)
tab
acc <- sum(diag(tab))/sum(tab)
acc


#-----------------####-----------------------##

admission <- read.csv("F:/DSP13/Admission.csv")
View(admission)

library(rpart)
library(rpart.plot)

str(admission)
dim(admission)
admission$Admission_YN <- as.factor(admission$Admission_YN)
set.seed(123)
attach(admission)

tree1<- rpart(Admission_YN~ ., data = admission, method = "class",
              control = rpart.control(cp = 0.02362205, mindepth = 10,
                                      minsplit = 100))
?rpart.control
tree1$cptable[which.min(tree1$cptable[,"xerror"]), "CP"]

rpart.plot(tree1)
text(tree1, pretty = 0)

pred <- predict(tree1, admission, type = "class")
tab <- table(pred, admission$Admission_YN)
tab
acc <- sum(diag(tab))/sum(tab)
acc


#--------------------------------------------------------------###

data("iris")
View(iris)
attach(iris)
dim(iris)
data <- iris
head(data)
#flowchart
tree1 <- ctree(Species ~., data = data)
tree1
plot(tree1)

#alternate way to represent in  flowchart format
tree2 <- rpart(Species ~., data = data)
rpart.plot(tree2, cex = 0.6)


predict(tree1, data)
tab <- table(predict(tree1), data$Species)

aucc <- sum(diag(tab))/sum(tab)
aucc    #accuracy = 96%

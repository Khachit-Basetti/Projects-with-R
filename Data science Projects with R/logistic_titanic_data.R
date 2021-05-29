#EXTRA, remve everything in the working environment
#rm(list = ls())
#cat("\14") #clear console

###### classification problem with titanic data
getwd()
setwd("C:/DataScience/DSP13")
train <- read.csv("titanic_train.csv")
test <- read.csv("titanic_test.csv")
test_survived <- read.csv("titanic_gender_submission.csv")
View(train)

trainratio <- nrow(train)/(nrow(train)+ nrow(test))
testratio<- 1-trainratio
round(cbind(trainratio, testratio))
dim(train)
colnames(train)
colnames(test)
colSums(is.na(test))
#adding survived data to test dataset(from gender_submission file)

View(test_survived)
dim(test_survived)
dim(test)
test$Survived <- NA
test$Survived <- test_survived$Survived 
View(test)


colSums(is.na(train)) #177 NAs are present in train data
colSums(train == '')

#glm() - generalised linear model, its a group which consist many other model
hist(train$Age, col = "yellow", border = "blue")
train$Age[is.na(train$Age)] <- mean(train$Age, na.rm = T)

train$Cabin <- as.numeric(train$Cabin)
hist(train$Cabin, col = "yellow", border = "blue")

prop.table(table(train$Embarked))
train$Embarked[train$Embarked  == ""] <- "S"

?subset
train1 <- subset(train, select = -c(Cabin, PassengerId, Ticket, Name))
head(train1)
View(train1)
str(train1)

#EDA

train1$Survived <- as.factor(train1$Survived)
train1$Pclass <- as.factor(train1$Pclass)
str(train1)
par(mfrow = c(1,1))
library(ggplot2)
ggplot(train1, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() + ggtitle("Pclass Vs survived rate")

ggplot(train1, aes(x = Sex, fill = Survived)) + 
  geom_bar() + ggtitle("Sex vs survived")

ggplot(train1, aes(x = SibSp, fill = Survived)) + 
  geom_bar() + ggtitle("Sibling vs survived")

ggplot(train1, aes(x = Embarked, fill = Survived)) + 
  geom_bar() + ggtitle("Embarked vs survived")

ggplot(train1, aes(x = Age, fill = Survived)) + 
  facet_grid(.~Sex) + geom_histogram(binwidth = 2.5)

ggplot(train1, aes(x = Age, fill = Survived)) + 
  facet_grid(.~Pclass) + geom_histogram(binwidth = 2.5)


#note down the Null, Residual and AIC of whole model

attach(train1)
model1 <- glm(Survived ~., family = binomial, data = train1)
summary(model1)

#we will remove one by one the suitable variable(with less *) and compare it with the above model
model1 <- glm(Survived ~. -Embarked, family = binomial, data = train1) # Keep
summary(model1)

model1 <- glm(Survived ~. -Parch, family = binomial, data = train1) # remove
summary(model1)

model1 <- glm(Survived ~. -Fare-Parch, family = binomial, data = train1) # Keep
summary(model1)

model1 <- glm(train1$Survived ~.-Fare,
              family=binomial, data=train1)
summary(model1)

##McFadden's pseudo-R2
RsqLogit<-1-(model1$deviance/model1$null.deviance)
RsqLogit

## or use R package
#install.packages("modEvA")
library(modEvA)
modEvA::Dsquared(model1)

#predicting accuracy
library(caret)
#type = 'response - 0 to 1
#type = 'probs'- for multibinomial variable-
predicttrain<-predict(model1,type = 'response')
predicttrain
tab<-table(train1$Survived, predicttrain >= 0.5) #cutoff is predicttrain
tab 
#if tab is giving error tht mean the length for survivedtrain and predicttrain is difft
#length(train$Survived)
#length(predicttrain)
acc <- sum(diag(tab))/sum(tab)
acc
###Model Performance Evaluation--------------------------
#install.packages("ROCR")
library(ROCR)

P12<-prediction(predicttrain,train1$Survived)
MPE<-performance(P12,'acc') #model performance evaluation
?performance
plot(MPE,col="red")

panel.first = grid()

maxvalue<-which.max(slot(MPE,"y.values")[[1]])
maxy<-slot(MPE,"y.values")[[1]][maxvalue]
maxy #Accuracy     0.8215

maxx<-slot(MPE,"x.values")[[1]][maxvalue]
maxx #Cutoff       0.6064

abline(h=.822672,v=.6032054,col="blue")

Pred <- predict(model1,type='response')
ROCR1 <- prediction(Pred, train1$Survived)
ROCR2<-performance(ROCR1,measure = "tpr",x.measure = "fpr")
plot(ROCR2, colorize=T)
abline(a=0,b=1)
panel.first = grid(col = "gray", lty = "dotted")

#Area under Curve (AUC)

auc<-performance(ROCR1, measure = "auc")
auc<-auc@y.values[[1]]
auc
legend(.4,.4,auc,title = "AUC")

##Test data

test1 <- subset(test, select = -c(Cabin, PassengerId, Ticket, Name))
test1$Survived <- as.factor(test1$Survived)
test1$Pclass <- as.factor(test1$Pclass)
str(test1)

colSums(is.na(test))
colSums(test=='')

library(caret)
test1$Age[is.na(test1$Age)] <- mean(test1$Age, na.rm = T)
test1$Fare[is.na(test1$Fare)] <- mean(test$Fare, na.rm=T)

p1 <- predict(model1, newdata = test1, type = 'response') #response =0 or 1 
View(p1)
p2 <- ifelse(p1 > 0.5,1,0)
head(p1, 10)
head(test1,10)

## or use confusionmatrix
p2 <- as.factor(p2)
class(p2)
confusionMatrix(data=p2, reference=test1$Survived)

tab <- table(test1$Survived, p1>= .5)
tab
sum(diag(tab))/sum(tab)








############### is we want to predict the AGE NA values ################

age.model = lm(age ~ fare + as.factor(Title) + sibsp + parch)
for(i in 1:nrow(train)){
  if(is.na(train[i, "age"])){
    train[i, "age"] = predict(age.model, newdata= train[i,])
  }
}
#write.csv(train, "train data with estimated age.csv")


################################################################
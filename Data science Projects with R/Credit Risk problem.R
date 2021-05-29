library(tidyverse)
lgd <- read.csv("R_Module_Day_5.2_Data_Case_Study_Loss_Given_Default.csv")
View(lgd)
setwd("C:/DataScience/DSP13")
getwd()

names(lgd)
dim(lgd)
str(lgd)

summary(lgd)
hist(lgd$Losses.in.Thousands,
     main = "Histogram of Losses",
     xlab = "Loss in $",
     ylab = "Number of accounts", col = "red",
     border = "yellow",breaks = 50) #break means each bar will be of width 50
?hist
par(mfrow =c(1,2))
boxplot(lgd$Age, 
        main = "Boxplot of Age",
        ylab = "Age in years",
        col = "green")

boxplot(lgd$Years.of.Experience, 
        main = "Boxplot of years of experience",
        ylab = "years of experience",
        col = "green")
par(mfrow = c(1,1)) #sets space for 3 graph
attach(lgd)
boxplot(Losses.in.Thousands ~ Age, main = "Loss Vs Age",
        ylab = "Losses in Thousands", xlab = "Age of the person",
        col = "yellow", border = "Orange")  #always y vs x
boxplot(Losses.in.Thousands ~ Gender, col = "yellow", border = "Orange",
        ylab = "Losses in thousands", xlab = "Gender")  
boxplot(Losses.in.Thousands ~ Married, main = "Loss VS Marital status",
        xlab="Marital status", ylab="Loss in thousands",col = "yellow", border = "Orange")   
boxplot(Losses.in.Thousands ~ Number.of.Vehicles, main = "Loss Vs Car count",
        xlab="Number of vehicles", ylab='Loss in thousands', border = 'black')

cor(Age , Years.of.Experience, method = "spearman")
#method involve pearson, kendall, spearman
?cor

table(lgd$Gender) #to know the proportion of male and female
prop.table(table(lgd$Gender)) #proportion in %

#data transformation
str(lgd)
lgd$Ac_No <- as.numeric(lgd$Ac_No)
lgd$Age <- as.numeric(lgd$Age)
lgd$Years.of.Experience <- as.numeric(lgd$Years.of.Experience)
lgd$Number.of.Vehicles <- as.numeric(lgd$Number.of.Vehicles)
lgd$Gender <- as.factor(lgd$Gender)
lgd$Married <- as.factor(lgd$Married)

str(lgd)

#split Data into training and testing datasets
#install.packages("caTools")
library(caTools)
set.seed(123) #locks the value to avoid randomness when new data is added
split = sample.split(lgd, SplitRatio = 0.7) #0.7 will be set to True and else false
training_set = subset(lgd, split == TRUE)
test_set = subset(lgd, split == FALSE)

#model Building

model1 <- lm(Losses.in.Thousands ~ ., training_set)
model1
summary(model1)


#identifying Multicollinearity in data
#install.packages("car")
library(car)
vif(model1) #variation inflation factor


#removing the values which have high VIF. 
#if more value then remove one by one and check R^2 value
names(training_set)
model1 <- lm(Losses.in.Thousands ~ Age + Number.of.Vehicles + Gender + Married, data = training_set)
model1
summary(model1)
dim(lgd)
vif(model1)

#assumption checking

par(mfrow = c(2,2))
plot(model1)

par(mfrow = c(1,1))

#prediction

predictloss <- predict(model1, test_set)
tab<-table(training_set$Losses.in.Thousands, predictloss >= 0.5)
sum(diag(tab))/sum(tab)



plot(test_set$Losses.in.Thousands, type = "l", col = "red")
lines(predictloss, type = "l", col = "blue")
#simplifying to a sample to see the actual visualisation.
#where you can see whether the plot is comparable
plot(test_set$Losses.in.Thousands[1:150], type = "l", col = "red")
lines(predictloss[1:150], type = "l", col = "blue")

#is model is ready and good, next how we operationalise(to put in machine)?
#by using r_shinny, app, batch  or other related package to push

write.csv(test_set, file = "risk.csv")

#import test data, predict and export the output
actual <- test_set
output <- cbind(predict(model1, test_set), actual)
View(output)
write.csv(output, file = "prediction.csv", row.names = F)

#predicting for a news dataset called boston 

#installed.packages("MASS")
library(MASS)
View(Boston)
data <- Boston
View(data)
dim(data)
?Boston
data("Boston")
colSums(is.na(data))
str(data)

Boston$rad <- as.numeric(data$rad)
scatter.smooth(x = data$medv, y = data$lstat,
               main = "medv Vs lstat", col = "red")  #scatter plot with a line 
par(mfrow = c(1,2))
boxplot(data$medv)
boxplot(data$lstat)
boxplot(data$medv, main = "medv" )
boxplot(data$lstat, main="lstat")
boxplot(data$medv ~ data$rad,
        main = "med vs rad",
        xlab = "rad", ylab = "medv", col = "yellow", border = "red")
#install.packages("corrplot")
library(corrplot)
?cor
bcor <- cor(data)
corrplot(bcor, type = "lower") #dark blue indicates +vely corelated with each other 
#and dark red indicates -vely corelation
par(mfrow = c(1,1))

corrplot(bcor, type = "upper")
#below to display by values and with lowest color we can ignore
corrplot(bcor, type = "lower", method = "number")
plot(density(data$medv), col = "red")
par(mfrow = c(1,2))
polygon(density(data$medv), col = "blue")
hist((data$medv), col = "red")

#split data  into training and testing datasets
library(car)   #for vif and other function
library(caTools)  #for samplesplit, subset 
set.seed(123)
split = sample.split(data, SplitRatio = 0.7)
training_set = subset(data, split == TRUE)
test_set = subset(data, split== FALSE)

#model  bulding
model1 <- lm(training_set$medv~. , data = training_set)
summary(model1)
vif(model1)
model1 <- lm(training_set$medv~.-indus-age-rad-tax, data = training_set)
#after testing the data this is a final statment
summary(model1)

#assumption checking
par(mfrow = c(2,2))
plot(model1,1)
plot(model1,2)
plot(model1,3)
plot(model1,4)

#prediction

predmedv <- predict(model1, test_set)
predmedv
head(predmedv)
 
#visualisation
par(mfrow = c(1,1))
plot(test_set$medv, type = "l", col = "red")
lines(predmedv, type = "l", col ="blue" )

#calculate accuracy
actvspred <- data.frame(cbind("ACTUAL" = test_set$medv, "PREDICTED" = predmedv))
actvspred
head(actvspred)
cor(actvspred) #this will give the accuracy of the prediction
round(cor(actvspred), 2) #rounding upto to 2 decimal point


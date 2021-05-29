#dataMining
library(tidyverse)
#install.packages("Hmisc")
library(Hmisc)
data("mtcars")
View(mtcars)
?mtcars
dim(mtcars)  #to know no. of rows and column
is.na(mtcars)
any(is.na(mtcars))  #return if there are NA's at all
sum(is.na(mtcars))  #return no. of NAs
#*********************************
View(airquality)
is.na(airquality)
sum(is.na(airquality))
colSums(is.na(airquality))
sum(is.na(airquality$Ozone))  #returns no. of NAs per coulmn using $ operator
mean(airquality$Ozone, na.rm=T)
median(airquality$Ozone, na.rm = T)
hist(airquality$Ozone)    #histogram to decide whether the data is normally distrubuted
mean(airquality$Solar.R, na.rm =T)
median(airquality$Solar.R, na.rm =T)

aq <- airquality[c(1:4)]  #remove last two column of airquality which is not usefull

View(aq)
colSums(is.na(aq))
dim(aq)
hist(airquality$Ozone)
airquality$Ozone <- impute(airquality$Ozone, median)
hist(airquality$Temp)
temp <- impute(airquality$Temp, mean)
aql<-na.omit(aq)   #to remove NA, R removes entire row where ever NA is present
dim(aql)
dim(airquality)
#below funtion is one where column wise removes the NAs, column wise
#omit is not efficient
aqozone <- na.omit(aq$Ozone) 

#install.packages("Hmisc")
library(Hmisc) #to use impute 
median(airquality$Ozone, na.rm = T)
impute(airquality$Ozone, median)
impute(airquality, mean)
sum(is.na(airquality))  #actual no. of NAs
sum(!complete.cases(airquality))  #no. of rows where atleast one NA is present
?sweep

#simple linear regression with one. install.packages("psych")
library(psych)
?psych
data("airquality")
View(airquality)
names(airquality)
str(airquality)
plot(Ozone ~ Solar.R, data = airquality) #Ozone is y and Solar.R is x
plot(airquality$Ozone,airquality$Solar.R) #alternate over above stmt
#below col = changes the colour of the plot
plot(Ozone ~ Solar.R, data = airquality, col = "red")
#cex is used to increase or decrease the size of dots
plot(Ozone ~ Solar.R, data = airquality,col = "red", cex = 2)  
plot(Ozone ~ Solar.R, data = airquality,col = "red", cex = 1)
abline(Model1, col = "blue")

#Lm() is a linear model, it returns y(intercept) and 
Model1 <- lm(Ozone ~ ., data = airquality) 
Model1 
summary(Model1)
#below returns the complete plot of airquality, where we can predict the best plot
#i.e Y vs(~) X. if the R squared value tends to 1 that model will be the best model. 
#*Best model will also depends on othre below values* 
pairs.panels(airquality)

library(corrplot)
cr <- cor(airquality)
corrplot(cr, type = "lower")
#multiple linear regression
Model2 <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
Model2
summary(Model2)

#Try yourself.
Model3 <- lm(Ozone ~ Temp, data = airquality)
Model3
summary(Model3)

Model5 <- lm(Ozone ~ Wind, data = airquality)
Model5
summary(Model5)


#below function returns the descriptive statistics i.e it returns Min, Max,Mean etc
#we predict the best fit by considering the value which is in between Max and Min
summary(airquality)
predict(Model2, data.frame(Solar.R = 300, Wind= 20:15, Temp = 90:85), interval = 'confidence')

#above function the values should be inside of max and min value and
#interval= confidence returns lower bind and upper bind
#it outputs fit which should be in between Lwr and Upr value which defines the best fit
predict(Model2, data.frame(Solar.R = 295:300, Wind= 20:15, Temp = 90:85), interval = 'confidence')

plot(Model2)
plot(Model2, 1) #it displays 1st graph among the 4 graphs

args(par)
par(mfrow = c(2,2))  #returns 2 by 2 plot
plot(Model2)


model <- lm(Ozone ~ Temp, data = airquality)
model
summary(model)


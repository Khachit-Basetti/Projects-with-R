data("mtcars")
mtcars
View(mtcars)
model1 <- glm(vs ~ disp+wt, data = mtcars, family = binomial)
model1

summary(model1)

#use the model summary to understand logit function
#eulers number e = 2.71828

numerator <- (2.71828^((-0.03443)*108+(1.62635)*2.320+1.60859))
denominator <- 1+ numerator
y <- numerator/denominator
y

######credit risk problem##
getwd()
setwd("C:/DataScience/DSP13")
train <- read.csv("R_Module_Day_7.2_Credit_Risk_Train_data.csv")
View(train)
colnames(train)
dim(train)
colSums(is.na(train))
colSums(train == '')
hist(train$LoanAmount, col = "yellow", border = "blue")
hist(train$Loan_Amount_Term, col = "yellow", border = "blue")

table(train$Credit_History)
prop.table(table(train$Credit_History))

train$LoanAmount[is.na(train$LoanAmount)] <- median(train$LoanAmount, na.rm = T)
train$Loan_Amount_Term[is.na(train$Loan_Amount_Term)] <- 360
train$Credit_History[is.na(train$Credit_History)] <- 88249847

prop.table(table(train$Gender))
prop.table(table(train$Married))
table(train$Dependents)
table(train$Self_Employed)

train$Gender[train$Gender == ""] <- "Male"
train$Married[train$Married == ""] <- "Yes"
train$Dependents[train$Dependents == ""] <- "0"
train$Self_Employed[train$Self_Employed == ""] <- "No"
train$Credit_History <- as.factor(train$Credit_History)
par(mfrow = c(1,1))
library(ggplot2)
ggplot(train, aes(x = Gender, fill = factor(Loan_Status))) +
  geom_bar() + ggtitle("Loan status by gender")

ggplot(train, aes(x = Married, fill = Loan_Status)) + 
  geom_bar() + ggtitle("Loan by  Marital status")

boxplot(train$ApplicantIncome ~ train$Loan_Status, xlab = "Loan status",
        ylab = "Applicant income")
boxplot(train$CoapplicantIncome ~ train$Loan_Status)
boxplot(train$LoanAmount ~ train$Loan_Status)

ggplot(train, aes(x = Credit_History, fill = factor(Loan_Status))) +
  geom_bar() + ggtitle("Loan status by credit history")

ggplot(train, aes(x = Property_Area, fill = factor(Loan_Status))) +
  geom_bar() + ggtitle("Loan status by property area")

ggplot(train, aes(x = Property_Area, fill = factor(Loan_Status))) +
  geom_bar() + facet_grid(.~Gender)

ggplot(train, aes(x = Property_Area, fill = factor(Loan_Status))) +
  geom_bar() + facet_grid(.~Education+Gender)
train
train1 <- train[,-1]
#note down the Null, Residual and AIC of whole model
model1 <- glm(Loan_Status ~., family = binomial, data = train1)
summary(model1)

#we will remove one by one the suitable variable(with less *) and compare it with the above model
model1 <- glm(Loan_Status ~. -Gender, family = binomial, data = train1) # remove
summary(model1)

model1 <- glm(Loan_Status ~. -Self_Employed,
              family = binomial, data = train1)   #remove
summary(model1)

#we are not removing Dependencys coz our residual is getting more after removing
model1 <- glm(Loan_Status ~. -Dependents, 
              family = binomial, data = train1)   #Keep
summary(model1)

#we are not removing education coz our residual is getting more after removing
model1 <- glm(Loan_Status ~. -Education,
              family = binomial, data = train1) #Dnt remove
summary(model1)

model1 <- glm(Loan_Status ~. -ApplicantIncome,
              family = binomial, data = train1)  #remove
summary(model1)

model1 <- glm(Loan_Status ~. -Loan_Amount_Term,
              family = binomial, data = train1)  #remove
summary(model1)
#final model

model1 <- glm(Loan_Status ~.-Gender-Dependents-Self_Employed-
                ApplicantIncome-LoanAmount-Loan_Amount_Term,
              family=binomial, data=train1)
summary(model1)

##McFadden's pseudo-R2
RsqLogit<- 1-(model1$deviance/model1$null.deviance)
RsqLogit
## or use R package, alternate method to find McFadden's pseudo-R2
#install.packages("modEvA")
#library(modEvA)
#modEvA::Dsquared(Model1)

library(caret)
#type = 'response - 0 to 1
#type = 'probs'- for multibinomial variable-
predicttrain<- predict(model1,type = "response")
predicttrain
tab <- table(train1$Loan_Status, predicttrain >= 0.5)
sum(diag(tab))/sum(tab)

###Model Performance Evaluation--------------------------
install.packages(ROCR)
?pROC
library(pROC)
P11 <- predict(model1, type='response')
P12 <- prediction(P11,train1$Loan_Status)

MPE<-performance(P12,'acc')
plot(MPE,col="red")

panel.first = grid()

maxvalue <- which.max(slot(MPE,"y.values")[[1]])
maxy <- slot(MPE,"y.values")[[1]][maxvalue]
maxy #Accuracy

maxx<-slot(MPE,"x.values")[[1]][maxvalue]
maxx #Cutoff

abline(h=.8127036,v=.5733747,col="blue")

P11 <- predict(model1,type='response')
ROCR1 <- prediction(P11, train1$Loan_Status)
ROCR2<-performance(ROCR1,"tpr","fpr")
plot(ROCR2, colorize=T,
     main="ROC Curve", print.cutoffs.at=seq(0.1, by=0.05))
abline(a=0,b=1)

#Area under Curve (AUC)

auc<-performance(ROCR1,"auc")
auc<-unlist(slot(auc,"y.values"))
auc
legend(.4,.4,auc,title = "AUC")

##Test data

test$LoanAmount[is.na(test$LoanAmount)] <- median(test$LoanAmount, na.rm = T)
test$Loan_Amount_Term[is.na(test$Loan_Amount_Term)] <- 360
test$Credit_History[is.na(test$Credit_History)] <- 1

test$Gender[test$Gender==""] <- "Male"
test$Self_Employed[test$Self_Employed==""] <- "No"
test$Dependents[test$Dependents==""] <- 0

colSums(is.na(test))
colSums(test=='')

library(caret)

test1<-test[-1]
test1$Credit_History<-as.factor(test1$Credit_History)
str(test1)
levels(test1$Self_Employed)
which(test1$Self_Employed == "", arr.ind=TRUE)


P1 <- predict(Model1,newdata = test1,type='response')
P1
P2 <- ifelse(P1 > 0.5,1,0)
CM<-table(P2,test1$outcome)
CM
Acc<-sum(diag(CM))/sum(CM)
Acc
## or use confusionmatrix
class(P2)
P2 <-as.factor(P2)
class(P2)
test1$outcome <- ifelse(test1$outcome=="Y",1,0)
class(test1$outcome)
test1$outcome<-as.factor(test1$outcome)
class(test1$outcome)
levels(test1$outcome)

confusionMatrix(data=P2, reference=test1$outcome)

##Split data based on credit History = NA -------------

nt<-train[is.na(train$Credit_History),]
View(nt)
nt1<-train[!is.na(train$Credit_History),]
View(nt)



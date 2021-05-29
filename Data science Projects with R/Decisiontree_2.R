train <- read.csv("F:/DSP13/R_Module_Day_12.1_Network_Intrusion_Train_data.csv", stringsAsFactors = FALSE)
test <- read.csv("F:/DSP13/R_Module_Day_12.2_Network_Intrusion_Test_data.csv", stringsAsFactors = FALSE)
validate <- read.csv("F:/DSP13/R_Module_Day_12.3_Network_Intrusion_Validate_data.csv", stringsAsFactors = FALSE)

colSums(is.na(train))
colSums(train =="")
attach(train)
str(train)
dim(train)
dim(test)
data <- rbind(train,validate)

set.seed(123)
sample <- sample(nrow(data), nrow(data)*0.70)
train <- data[sample,]
test <- data[-sample,]

train$protocol_type <- as.factor(train$protocol_type)
train$class <- as.factor(train$class)
prop.table(table(train$class))

validate$class <- as.factor(validate$class)
library(party)
library(rpart)
library(rpart.plot)

tree2 <- rpart(train$class ~., data = train, method = "class")
rpart.plot(tree2, cex = 0.6)

pred <- predict(tree2, train, type = "class")
tab <- table(pred,train$class)
tab
acc <- sum(diag(tab))/sum(tab)
acc

#--------------------------##------------------------##

#import cars data

data <- read.csv("F:/DSP13/car.csv", header = TRUE)
View(data)
str(data)
table(data$class)

set.seed(111)
sample <- sample(nrow(data), nrow(data)*0.70)
train <- data[sample,]
test <- data[-sample,]

#ID3 method (entropy//information gain)

library(caret)
tc <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(123)
tree1 <- train(class~., data = train, method = "rpart",
              parms = list(split = "information"),
              trControl = tc, tuneLength = 10) #change split =gini or rf and check the acc
tree1$results
tree1
plot(tree1$finalModel)

library(rpart.plot)
prp(tree1$finalModel, box.palette = "green", tweak = 1.6)

#prediction
pred1 <- predict(tree1, newdata = test)
tab <- table(pred1, test$class)
acc <- sum(diag(tab))/sum(tab)
acc

confusionMatrix(pred1, test$class) #or use confusion matrix

library(randomForest)

m1 <- randomForest(class~., data = train)
m1
plot(m1)

#model tuning
set.seed(1345)
tuneRF(train[,-7], train[,7], stepFactor = 0.5,
       plot = T, ntreeTry = 50, trace = T, improve = 0.05) #ntreeTry = 30, gives 100% acc, try madi

m2 <- randomForest(class~., data = train, ntree = 50, mtry = 4)
m2

pred<- predict(m2, train, type = "class")
tab <- table(pred, train$class)
acc <- sum(diag(tab))/sum(tab)
acc

pred <- predict(m2, test, type = "class")
tab <- table(pred, test$class)
acc <- sum(diag(tab))/sum(tab)
acc

#-------------------##--------------------###

data <- read.csv("F:/DSP13/CreditRisk.csv")
View(data)

dim(data)

colSums(is.na(data))
colSums(data =="")

hist(data$Loan_Amount_Term, col = "yellow", border = "blue")
median(data$Loan_Amount_Term, na.rm =TRUE)

hist(data$Credit_History)
mean(data$Credit_History, na.rm = TRUE)

hist(data$LoanAmount)

data$LoanAmount[is.na(data$LoanAmount)] <- mean(data$LoanAmount, na.rm =TRUE)
data$Credit_History[is.na(data$Credit_History)] <-  1
data$Loan_Amount_Term[is.na(data$Loan_Amount_Term)] <- median(data$Loan_Amount_Term, na.rm =TRUE)

prop.table(table(data$Self_Employed))
data$Gender[data$Gender == ""] <- "Male" 
data$Married[data$Married == ""] <- "Yes"
data$Dependents[data$Dependents == ""] <- 0
data$Self_Employed[data$Self_Employed == ""] <- "No"

attach(data)
#install.packages("ROSE") $random over sampling
library(ROSE)
table(data$Loan_Status)
data1 <- ovun.sample(Loan_Status~., data = data,
                     method = "over", N = 994)$data
table(data1$Loan_Status)

data2 <- ovun.sample(Loan_Status~., data = data,
                     method = "under", N = 22)$data
table(data2$Loan_Status)

##rose
data3 <- ROSE(Loan_Status~., data = data, seed = 1)$data
table(data3$Loan_Status)

#code will be shared 
##-------------------###---------------------####

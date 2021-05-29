###########Binary classification with neural network


setwd("C:/DataScience/Estimates/NeuralNetwork")

credit <- read.csv("UCI_Credit_Card.csv") #predict if the tumor is 
#maligant (M) or benign (B)

head(credit)

#drop columns
drops = c("ID")

df <- credit[ ,!names(credit) %in% drops]

head(df)

names(df)[names(df)=='default.payment.next.month'] <- 'default'

str(df)

## lets convert 1 to default and 0 as not default.
df$default[df$default == 1] <- 'Yes'

df$default[df$default == 0] <- 'No'

table(df$default)

set.seed(99)

library(caret)

train <- createDataPartition(df$default, p=0.75, list= FALSE)
#splits the data in 75% - 25% ratio


training = df[train, ] #75% data for training
testing = df[-train, ] #25% testing

# implement cross validation
fitcontrol = trainControl(method = "cv", number = 10,
                          preProcOptions = list(thresh = 0.95),
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

######

modelnet = train(default ~.,
                 training,
                 method ="nnet", #neutal network method
                 metric = "ROC", #since this is a classification problem its set to ROC
                 preProcess= c('center', 'scale'), #for artificial NN centring and scaling is imp
                 trace = FALSE, 
                 tuneLength = 10,
                 trControl = fitcontrol)
modelnet

summary(modelnet)

head(testing)

plot(modelnet)

prednet <- predict(modelnet, testing)

t <- confusionMatrix(predictions = prednet, testing$default)
t

varImp(modelnet)

plot(varImp(modelnet))

###########Binary classification with neural network using PCA


setwd("C:/DataScience/Estimates/NeuralNetwork")

credit <- read.csv("UCI_Credit_Card.csv") #predict if the tumor is 
#maligant (M) or benign (B)

head(credit)

#drop columns
drops = c("ID")

df <- credit[ ,!names(credit) %in% drops]

head(df)

names(df)[names(df)=='default.payment.next.month'] <- 'default'

str(df)

## lets convert 1 to default and 0 as not default.
df$default[df$default == 1] <- 'Yes'

df$default[df$default == 0] <- 'No'

table(df$default)

set.seed(99)

library(caret)

train <- createDataPartition(df$default, p=0.75, list= FALSE)
#splits the data in 75% - 25% ratio


training = df[train, ] #75% data for training
testing = df[-train, ] #25% testing

# implement cross validation
fitcontrol = trainControl(method = "cv", number = 10,
                          preProcOptions = list(thresh = 0.95),
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

######

modelnet = train(default ~.,
                 training,
                 method ="pcaNNet", #neutal network method
                 metric = "ROC", #since this is a classification problem its set to ROC
                 preProcess= c('center', 'scale'), #for artificial NN centring and scaling is imp
                 trace = FALSE, 
                 tuneLength = 10,
                 trControl = fitcontrol)
modelnnet

summary(modelnet)

head(testing)

plot(modelnnet)

prednet <- predict(modelnnet, testing)

t <- confusionMatrix(prednet, testing$default)
t

varImp(modelnet)

plot(varImp(modelnet))


#############  Multi Layer Perceptron MLP for supervised classification
install.packages("RSNNS")
library(RSNNS)

d <- read.csv("glass.csv")

head(d)

str(d)

d$Type <- as.factor(d$Type)

df <- d[sample(1:nrow(d), length(1:nrow(d))), 1:ncol(d)]

dfvalues <- df[,1:9] #predictors

dftargets= decodeClassLabels(df[,10]) #response

df <- splitForTrainingAndTest(dfvalues, dftargets, ratio = 0.20)

## 80% training & 20% testing

df <- normTrainingAndTestSet(df)  ##normalize the data

##MLP's are fully connected, feed -forward networks

model <- mlp(df$inputsTrain,
             df$targetsTrain,
             size = 5,
             learnFuncParams = c(0,1),
             maxit = 50,
             inputsTest = df$inputsTest,
             targetsTest = df$targetsTest)

summary(model)

#function prints out a summary of the network

weightMatrix(model)

#function plots the iterative training and test error of the net of 
plotIterativeError(model)

predictions <- predict(model, df$inputsTest)

#plotRegressionError(predictions[,2], df$targetsTest[,2])

confusionMatrix(df$targetsTrain, fitted.values(model))

#confusion matrix of training and testing set

confusionMatrix(df$targetsTest, predictions)

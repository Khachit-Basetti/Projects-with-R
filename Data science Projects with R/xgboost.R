library(mlbench)  #for the data sonar
library(caret) # cross validation

data("Sonar")
df <- Sonar

help("Sonar")

dim(df)

head(df)
summary(df)

str(df)

library(caTools)

ind<- createDataPartition(df$Class, p = 0.70, list = FALSE)
traindf <- df[ind,]
tesdf <- df[-ind,]

controlparameter <- trainControl(method = "cv",
                                 number = 5,
                                 savePredictions = TRUE,
                                 classProbs = TRUE)

parameterGrid <- expand.grid(eta = c(0.1, 0.2),
                             colsample_bytree = c(0.5, 0.7),
                             max_depth = c(3,6),
                             nrounds = 100,
                             gamma =1,
                             min_child_weight = 2,
                             subsample = 1)

modelxg <- train(Class~.,
                 data = traindf,
                 method = "xgbTree",
                 trControl = controlparameter,
                 tuneGrid = parameterGrid)

plot(modelxg)


#predictions

prediction <- predict(modelxg, tesdf)

t <- table(prediction, actual = tesdf$Class)
t

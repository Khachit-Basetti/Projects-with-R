data("AirPassengers")
class(AirPassengers)
start(AirPassengers)  
end(AirPassengers)
frequency(AirPassengers)
summary(AirPassengers)
plot(AirPassengers) #plot of timeseries

abline(reg = lm(AirPassengers~ time(AirPassengers)))

cycle(AirPassengers)

#GENERAL TREND
#To aggregae the cycle and display a year on year trend
plot(aggregate(AirPassengers, FUN = mean))

#SEASONALITY
#Boxplot across months will give us a sense on seasonal effect
boxplot(AirPassengers~cycle(AirPassengers))


#To make the data stationary is by coverting Varience
#Variance, mean and co-Varience to be equal

plot(log(AirPassengers)) #Varience is equal

plot(diff(log(AirPassengers))) #Diffrentiate to make Mean constant

#AR I MA
#AutoRegression  Integration  MovingAverage
#p               d                 q

#PDQ values can be found out by auto corelation function graph

acf(AirPassengers) #Ideally the lines should be below the blue line

acf(diff(log(AirPassengers))) #determines the value of q
#the line before the inverted line is considered. oth, 1th, 2nd(inverted)
#hence 1th line, therefore q=1


#Partial Auto corelation function is used to determine the value of p
pacf(diff(log(AirPassengers))) #with same procedure as above, p=0
#because 1th line is getting inverted, therefore we consider the line before

#p=0, q=1
#Value of D, depends on how many times we diffrentiate to make the mean constant
#in this case we have diffrentiated once
#hence d=1, Therefore (p,d,q)=(0,1,1)

#lets fit an Arima Model and predict the future for 10 years
fit <- arima(log(AirPassengers), c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))

pred <- predict(fit, n.ahead = 10*12)
pred1 <- 2.718^pred$pred #since my values are in log form, we have to take 
#e(2.718) to the power value, to convert them into decimal form

#earlier the data was till 1960, now we predicted the price for next 10 years, i.e, till 1970

ts.plot(AirPassengers, pred1, log ="y", lty=c(1,3))#this is how tthe graphh looks after predition

#now lets consider test data, i.e, we will extract the data till 1959, and predict values for 1960
datawide <- ts(AirPassengers, frequency = 12, start = c(1949,1),end= c(1959,12))

fittest <- arima(log(datawide), c(0,1,1), seasonal = list(order =c(0,1,1),period= 12))

predt <- predict(fittest, n.ahead = 10*12)
pred2 <- 2.718^predt$pred
data1 <- head(pred2,12)

predicted_1960 <- round(data1,digits = 0)

original_1960 <- tail(AirPassengers,12)

ts.plot(AirPassengers, pred2, log ="y", lty=c(1,3))
?ts.plot

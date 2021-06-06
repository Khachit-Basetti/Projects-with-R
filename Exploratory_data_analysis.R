#Central Tendency


ny <- c(1,2,3,3,5,6,7,8,9,10,66)  #there is one outlier in this data set which will be a oulier
#outlier affects the mean very badly. in such condition its better to go for MEDIAN
la <- c(1,2,3,4,5,6,7,8,9,10,11)
pizza <- data.frame(ny,la)
pizza

mean(pizza$ny)
mean(pizza$la)

median(pizza$ny)

summary(pizza) #ALL the values of central tendency are displayed at once
#mode: it is the commonly repeated term, if there are no repeatative values, we can say no mode
#we dont have any function as such for mode. below is a predefined one

sort(table(pizza$ny), decreasing = T)[1] #defined function to find mode
x <- table(pizza$ny) #contingency table - shows freq distribution of the data
x
names(x)[which(x == max(x))]

#coefficient of variance
ny <- c(1,2,3,3,5,6,7,8,9,11)
la <- c(1,2,3,4,5,6,7,8,9,10)

pizza <- data.frame(ny,la)
pizza
pizza$ny.mxn <- c(18.81,37.62,56.43,56.43,94.05,112.86,131.67,150.48,169.29,206.91)
pizza

lapply(pizza, mean)
sapply(pizza, mean)
sapply(pizza, var)
sapply(pizza, sd)
coef.var <- sapply(pizza, sd)/sapply(pizza, mean)
coef.var

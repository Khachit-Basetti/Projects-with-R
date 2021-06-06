rabbitlateness <- c(40,30,20,45,60,120,35,40,50,
                    55,53,22,27,48,62,33,35,40,45)
range(rabbitlateness) #returns max and min values
#lets  create a freq table
bins <- seq(20,130, by= 10)
intervals <- cut(rabbitlateness, bins, right = F)
intervals
table(intervals)
str(rabbitlateness)
#type is "h" coz we want histogram
plot(table(intervals), type = "h", main = "Rabbits arrival times",
     xlab = "Intervals", ylab = "Frequency")
#alternate
hist(rabbitlateness, breaks = bins)


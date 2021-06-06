#we prefer{} if the function ismore than one line
say.hello <- function()
{
  print("Hello World!")
}

hey.say <- function()
{
  print("Helow")
}
hey.say()
say.hello
say.hello()

#sprintf fucntion

sprintf("Hello %s", "Khachi")
sprintf("Hello %s, today is %s", "Khachi","sunday")

hello.person <- function(name)
{
  print(sprintf("hello %s",name))
}
hello.person
hello.person("Khachi")
hello.person("Basetti")
#
hello.person <- function(first, last)
{
  print(sprintf("Hello %s %s",first,last))
}

hey.boi <- function(firstname,lastname)
{
  print("hello %s %s", firstname, lastname)
}

hey.boi("khach", "bas")
hello.person("Khachit","baseti")
hello.person(first = "basetti",last = "Khachi")
hello.person(first = "khachi","Basetti")

##
hello.person <- function(first,last = "basetti")
{
  print(sprintf("hello %s %s", first, last))
}

hello.person("khachit","Ravindra")
#if we dnt put the last name it take the default name given 
#during function decleration
hello.person("khachit") 
hello.person(last = "Khachit") #error, coz last is already defined in fuction
hello.person("khachit",extra = "goodbye") #error
hello.person("khachit","Basetti",'Goodbye')#erro

### dot dot dot operator overcomes the above errors
hello.person <- function(first,last = "Basetti",...)
{
  print(sprintf("hello %s %s", first, last))
}

hello.person("Khachit", "Ravindra","Goodbye")
#the goodbye is just ignored

hello.person("khachi",extra = "goodbye")

#---------------------##-----------------------##

double.num <- function(x)
{
  x*2
}

double.num(2)
double.num(5)

double.num <- function(x)
{
  return(x*2)
}

double.num(10)
double.num(5)

double.num <- function(x)
{
  return(x*2)
  #only the above return is excecuted and below things are ignored
  print("hello")
  return(17)
}

double.num(5)


double.num <- function(x)
{
  x*2
  #above x*2 is not excecuted when there are multiple lines of code inside function()
  print("hello")
  return(17)
}
double.num(5)
#------------------------------##-------------------##

#
hello.person <- function(first, last = Basetti,...)
{
  print(sprintf("Hello %s %s", first,last))
}
hello.person("Khachit","Ravindra")

do.call("hello.person", args = list(first = "khachit",last = "Ravindra"))
do.call(hello.person, args = list("Khachit", "Ravindra"))

##do.call function

run.this <- function(x,func = mean)
{
  do.call(func, args = list(x))
}
run.this(1:10)
run.this(1:10, mean)
run.this(1:10, sum)
run.this(1:10, sd)

#if and else statement


check.bool <- function(x)
{
  if(x==1)
  {
    print("Hello")
  } else
  {
    print("Goodbye")
  }
 }

check.bool(1)
check.bool(0)
check.bool("k")
check.bool(TRUE) #Internally it considers it as 1

##
check.bool <- function(x)
{
  if(x==1)
  {
    print("Hello")
  } else if(x==0)
  {
    print("Goodbye")
  }else
  {
    print("Confused")
  }
}

check.bool(1)
check.bool("k")
check.bool(0)
check.bool(2)

#SWTICH function

use.switch <- function(x)
{
  switch (x,
    "a" = "first",
    "b" = "Second",
    "z" = "last",
    "c" = "third",
    "Others")
}

use.switch("a")
use.switch("b")
use.switch("z")
use.switch(1) #considers the position of the swtich cases
use.switch(2)
use.switch(3)
use.switch(5)
use.switch(6) #returns blank, coz there is no such position
is.null(use.switch(6)) #there is a NULL value, hence it returns TRUE

######----------####--------------------###

ifelse(1 == 1, "Yes", "Yes")
ifelse(1==0, "Yes", "No")

totest <- c(1,1,0,1,0,1)
ifelse(totest == 1, "Yes","No")
ifelse(totest == 1, totest*3, totest)
ifelse(totest == 1, totest*3, "Zero") #since there is one character variable, everything will be char

#-------------##------------##
#compound stmt

a <- c(1,1,0,1)
b <- c(2,1,0,1)

ifelse(a == 1 & b == 1, "Yes","No") # 1 & will give output for each element in a vector
ifelse(a == 1 && b == 1, "Yes","No") # 2 & will give a final output

x <- 1
y <- 2

if(x == 1 || y == 3)
{
  print("Hello")
}

#-----------###--------------------##

#for loop

for(i in 1:10)
{
  print(i)

}

fruit <- c("apple", "banana", "pomogranate")
fruitlenght <- rep(NA, length(fruit))
fruitlenght

names(fruitlenght) <- fruit
fruitlenght

for (a in fruit) 

fruitlenght

##
for (i in 1:10) 
{
  if(i == 3)
  {
    next
  }
  print(i)
}

####_---------------------###---------------------------##

x<- 1
while (x <=5)
{
  print(x)
  x <- x+1
}


for (i in 1:10)
{
  print(i)
  if(i==4)
  {
    break
  }
}

#------------------###_---------------##

####### DATA MUNGING ###########

theMatrix <- matrix(1:9, nrow = 3)
theMatrix

apply(theMatrix, MARGIN = 2, sum) 
#apply always takes Matrix. if we feed df or vector or any other datastruc it will
#convert it to matrix and compute. Margin = 1(row operation), Margin = 2(col operation)

apply(theMatrix, MARGIN = 1, sum)

#above similar operation wih some functions
colSums(theMatrix)
rowSums(theMatrix)

theMatrix[2,1] <- NA
theMatrix

apply(theMatrix, 2, sum)
apply(theMatrix, 2, sum, na.rm=TRUE)

colSums(theMatrix, na.rm = TRUE)

#-------------------###_------------------------

thelist <- list(A =matrix(1:9, nrow = 3), B=1:5, C=matrix(1:4,2), D=2)
thelist
?lapply()
lapply(thelist, sum)
#list or vector, which returns list of same lenght as the given list/vector

?sapply()
sapply(thelist, sum)
#input=list/vector, output=vector

thenames <- c("Khachit", "Jared","Paul")
lapply(thenames, nchar)
####--------------------####----------------------##

firstlist <- list(A = matrix(1:16,4), B=matrix(1:16,2),C = 1:5)
secondlist <- list(A =matrix(1:16,4), B = matrix(1:16,8),C = 15:1)
firstlist
secondlist

mapply(identical, firstlist, secondlist)

simplefunc <- function(x,y)
{ 
  NROW(x)+NROW(y)
}

mapply(simplefunc, firstlist,secondlist)
#--------------------------####--------------------##


library(ggplot2)
data("diamonds")
head(diamonds)
?aggregate
mean(diamonds$price)
aggregate(price ~ cut, diamonds,mean)
#avrage price of each cut of diamonds, like for premium,good,ideal
#very good cut whats the avg

aggregate(price ~cut, diamonds,mean,na.rm =TRUE)

aggregate(price~cut + color,diamonds, mean)

aggregate(cbind(price,carat)~ cut,diamonds,mean)

aggregate(cbind(carat,price)~cut+color, diamonds, mean)
#---------------------------##--------------------------###

library(plyr)
tail(baseball)
?baseball
colSums(is.na(baseball))

baseball$sf[baseball$year <1954] = 0 #coz, before 1954 there were no SF so its all NA's
any(is.na(baseball$sf))

hist(baseball$hbp)
baseball$hbp[is.na(baseball$hbp)] =0
any(is.na(baseball$hbp))

#we are going to checking only for players coming for bat more then 50 times

baseball <- baseball[baseball$ab >=50, ]

baseball$OBP <- with(baseball, (h+bb+hbp)/(ab+bb+hbp+sf)) #with(data, formula)
#alternate option is we can attach(baseball) and use the column names directly

tail(baseball)

obp <- function(data)
{
  c(OBP = with(data, sum(h+bb+hbp) / sum(ab+bb+hbp+sf)))
}

careerOBP <- ddply(baseball, .variables = "id", obp)
head(careerOBP)
careerOBP <- careerOBP[order(careerOBP$OBP, decreasing = TRUE),]
head(careerOBP)

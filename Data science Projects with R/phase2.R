#if statment

x <- 12
if (x > 10) {
  print("double digit")
}else{
  print("single digit")
}

#for loop
View(mtcars)
data(mtcars)
for (vars in names(mtcars)) {
  print(vars)
}

cars <- c('Valient', 'Duster 360', 'Merc 230')
for (j in cars) {
  print(paste(j,'-All Variants'))
}


#Repeat loop

a <- 5
repeat{
  print(a)
  a <- a+1
  if(a > 10)
    break
}

x <- 1:10
for (i in x) {
  if(i==5){
    break
  }
  print(i)
}

#Next stmt
x <- 1:10
for (i in x) {
  if(i==5){
    next      #skips 5 and excecutes x
  }
  print(i)
}

#predefined error or warning messages

n <- -8
if( n < 0){stop("The argument 'n' should be positive")}

if( n < 0){warning("The argument 'n' should be positive")}

#function

addingpercent <- function(x){percent <- round(x*100, digits = 2)
output <- paste(percent, "%", sep = "")
return(output)
}
#call the function
x <- c(0.678,0.2999,0.765352)
addingpercent(x)

cars <- c("Honda", "Toyota", "Tata", "Maruti", "Ford", "Fkathy")
car.names <- function(x){
  for (name in x) {
    if(name=="Tata")
      break
    print(name)
  }
}
#call fuction
car.names(cars)

#function
x <- 5
f.while <- function(x){
  i<- 0
  while (i < x) {
    i <- i+1.5
    y <- i*2
    print(y)
  }
  return(c(i,y,y*2))
}

#call function
f.while(x)


#function: Repeat loop
x <- c('Apple','Orange','Pear','Muskmelon', 'Blueberry', 'Grape')
fruit.repeat <- function(x){
  i<-1
  repeat{
    print(x[i])
    i <- i+1
    if(x[i] == 'Blueberry')
      break
  }
}
#call function
fruit.repeat(x)

#function calculate formula

#formula (x^2 +1)
w <- function(x) return(x^2 +1)
x <- c(2,3,4)
w(x)

#function with 2 variablle
z <- function(x,m) return((x+m)^2)
z(x,10)

#calculating compount intrest

ci <- function(p,r,n) return(p*(1 + r)^n)

#call
ci(10000, 0.10, 1:20)

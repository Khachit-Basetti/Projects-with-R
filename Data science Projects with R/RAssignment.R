#working directory

getwd() #getting directory
setwd('F:/DSP13') #setting qorking directory
getwd()
install.packages('e1071') #code for installing packages with dependency

#calling the installed packages
library(e1071)
1*1
1+73

#operators


#data Structures
age=c(30,25,28,32,33,44)
length(age)
class(age)
age[3]=20
age
age[3,4]=20,35 #error
age[c(3,4)]=c(21,35)
age
age=c(30,25,28,32,33,NA)
class(age)
age[6]=0
age
is.na(age)
age=c(30,25,28,NA,33,NA)
is.na(age)
sum(is.na(age))
which(is.na(age)) #position
age=ifelse(is.na(age),mean(na.rm=T) ,age)
age
age[is.na(age)]=mean(age,na.rm  =T)
age
m=matrix(2:5,2,2)
m
#Q1elements >5
v=c(5,6,7,3,2,5,6)
v[v>5]
which(v>5) #position

#Q2to sqaure and to display the position >10
v=c(5,6,7,3,2,5,6)
v1=v^2
v1
which(v1>10)

#Q3values whose square>10
v=c(5,6,7,3,2,5,6)
v1=v^2
v1[v1>10]

#Q4 create a matrix and transpose
m=matrix(c(30,20,25,40,75,60,78,79,45,42,70,75),4,4)
m
t(m)

#Q5 create char matrix called fruits

fruits=matrix(c('apple','orange','pear','grapes'))
fruits


#Q6 create 3*4  matrix of marks obtained in each quarterly exams for 4 different subject
m=matrix(c(30,20,25,40,75,60,78,79,45,42,70,75),3,4)
m

#Q7 calculate mean for rol and column

m=matrix(c(30,20,25,40,75,60,78,79,45,42,70,75),4,4)
rowMeans(m)
colMeans(m)

#Q8 add the mean/sum to row & columns of matrix and name the added column
m=matrix(c(30,20,25,40,75,60,78,79,45,42,70,75),4,4)
Rsum=rowSums(m)
  Rsum
cbind(m,Rsum)
Csum=colSums(m)
m=rbind(m,Csum)
m
cbind(m,Rsum)

m=cbind(m,rm=rowMeans(m))
m
m=rbind(m,cm=colMeans(m))
m

#Q9 using apply find mean per subject
m=matrix(c(30,20,25,40,75,60,78,79,45,42,70,75),4,4)
s1=apply(m,1,mean)
s1
m=rbind(m,s1)
m


#Q10 create data frame

df=data.frame(students=c('khac','Deb','abhi'),subject=c('eng','hindi','kann'), Marks=c('98','94','94'))
df

#Q11 Change the name of marks to score
df=data.frame(students=c('khac','Deb','abhi'),subject=c('eng','hindi','kann'), Marks=c(98,94,94))
df
names(df)[3]='Scores'
df
#Q12 students whose marks are above 40
df=data.frame(students=c('khac','Deb','abhi'),subject=c('eng','hindi','kann'), Marks=c(98,35,94))
df
  

#Q13 create 2 vectors and find mean
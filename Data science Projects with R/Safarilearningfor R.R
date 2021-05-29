
#install.packages("ggplot2")
a=c(2L,1L,2L,3L,2L) #for integer datatype 
#as by default R considers the vector as double (6th line)
typeof(a)  #type of a is integer
class(a)
b=c(5,5,59,8,7)
typeof(b)

args(sample)

deck=c("a","b","c","d","e")
deck
sample(deck,size = 3,replace = T)

###########Vector Attibutes - names, class, dim##########################
#names
age <- c(23,26,24,26)
attributes(age)
names(age) <- c("george","John","paul","ring")
age
attributes(age) 
names(age)
names(age) <- c("George hudlson","John Smith","Paul walker","Ring stone")
age
names(age) <- c("john","peter","bubble","kevin")
age

#********Indexing and slicing******
pegs <- c(6,7,8,9,10)
person<- c("khachit", "sham","ram","bham","dps")
names(pegs) <- person
pegs
pegs[4] #indexing
pegs[-4]
pegs[c(1,3,5)]
pegs["khachit"]
pegs[c("khachit","ram","dps")]
pegs[3:5]  #slicing

lv <- seq(10,100,by=10)
lv
lv[-(4:7)]
lv[lv>50]

x <- 10:1
y<- -4:5
q <- c("Hockey","Football", "Baseball","Curling", "Rugby", "Lacrosse", "BasketBall",
       "Tennis", "Cricket", "Soccer")
theDF <- data.frame(x,y,q)
theDF
class(theDF$q)
str(theDF)
theDF <- data.frame(First= x, Second= y, Sport= q, stringsAsFactors = FALSE)
class(theDF$Sport)
theDF[ ,3] #even though its Dataframe it considered it as vector after indexing
class(theDF[,3])

theDF[,2:3] #for 2 col it considered as dataframe again

theDF[,3, drop = FALSE] #SOLUTION FOR ABOVE PROBLEM
theDF[,"Sport", drop = FALSE]

class(theDF[,3, drop = FALSE])


#dim and class
a <- seq(10,120,by=10)
a                        #before class - numeric
?dim
dim(a) <- c(3,4)
a                        #after class - matrix
typeof(a)
class(a) #we get to know the class of the variable a before and after
dim(a) <- c(4,3)
a

#*****Matrix, Rbind, Cbind, rownames,Colnames, t()***********

matx <- matrix(1:12,ncol=4)
matx
matx < matrix(1:12, nrow = 3) #bydefault byrow=F, which means numbers col wise
matx
matx <- matrix(1:12,nrow=3, byrow = T) #to define the numbers row-wise
matx

usa <- c(2.3,2.6,3.5,1.6,0.5)
fr <- c(5.2,6,4.5,5.5,1.2)
ngo <- cbind(usa,fr)
ngo
rownames(ngo) <- c(2011,2012,2013,2014,2015) #naming the row
ngo
ngo <- t(ngo)   #transpose
ngo
ind <- c(2.3,2.2,2.1,2.3,2.4)
ind
ngo <- rbind(ngo,ind)
ngo

#matrix operations like rbind, cbind, rowMeans, colMeans, rowSums, colSums

matrix.mat <- matrix(c(171.5,281.6,139.3,292.0,460.6,288.0),
                     nrow=3,ncol = 2,
                     dimnames = list(c("The Matrix","Reloaded","Revolutions"),
                                     c("US","Worldwide")))
matrix.mat

total <- colSums(matrix.mat)
rowSums(matrix.mat)

mean <- colMeans(matrix.mat)
rowMeans(matrix.mat)
rbind(matrix.mat,total,mean)

#*******************factor********************
#difining a factor function
marital.status <- c("Married", "Married","Single","Married","Divorsed","Widowed",  "Divorsed")
marital.factor <- factor(marital.status)
marital.factor
typeof(marital.factor) #factor converts the levels in integer form
str(marital.factor) #defined the order of levels in integer form
#to order according to our requirement(encoding)
new.factor <- factor(marital.status,
                     levels = c("Single","Married","Divorsed","Widowed"))
new.factor
str(new.factor)
levels(new.factor) <- c("s","m","d","w") #level function is used to rename the levels
new.factor
str(new.factor)
#to rename the levels we use label variable inside factor function
new.factor <- factor(marital.status, 
                     levels = c("Single","Married","Divorsed","Widowed"),
                     labels = c("Sin","Mar","Div","Wid"))
new.factor
str(new.factor)

#set ordered= TRUE to give relative ranking to the levels defined
ordered.factor <- factor(marital.status, ordered = T, levels = c("Single","Married","Divorsed","Widowed"))
ordered.factor

#*****************LIST function**************
my.book <- list("1984","George Orwell",1949,
                list(c(1:8), c(1:10), c(1:6),
                     "Newspeak")) #list inside a list coz its  recursive
my.book
str(my.book) #returns better understandable values of list

#Defining names to make it more simple to understand list
my.book <- list(Name = "1984",Author = "George Orwell",Pulblished = 1949,
                Contents = list(PartOne = c(1:8),
                                PartTwo = c(1:10),
                                PartThree = c(1:6), Appendix = "Newspeak"))
my.book
str(my.book)

#alternate way to define names by names function
my.book <- list("1984","George Orwell",1949,
                list(c(1:8), c(1:10), c(1:6), "Newspeak"))
names(my.book) <- c("Name", "Author", "Published", "Contents")
str(my.book)

#indexing and subsetting
my.book[1:2] #first 2 values of list [] with names
my.book[2]  
my.book[[2]] #2nd value of list [[]] without names
my.book[[4]][2] #4th list with 2nd value inside that list with names

my.book[[4]][[2]] # || without names

#Fundamentals of programming
#Relational operators
3==3
"cat"=="cat"
"cat"=="car"
TRUE==TRUE
TRUE==FALSE
"sugar"!="sat"
3!=3
17>23
"rat">"cat" #symantically cat is bigger than rat. R doest use semantics. R compared with alphabetical order
TRUE>FALSE
TRUE<FALSE #Cersion rule-> i.e, T=1, F=0
7>=7
7>=4
7<=4

#logical operators or boolean operators

w <- 9
(w < 12) & (w > 6)
(w > 12) | (w < 6)

#logical and boolean operators in VECTORS

v <- c(1,3,5,7)
w <- c(1,2,3,4)
3==v
3==w
12 > c(11,11,13,14)
"catch"== c("catch",22,"is","fantastic")
c(11,12,13)>= c(10,12,14)

(v >=3) & (v<7)
v>=3
v<7
(v>=3) | (v<7)

#for loop

title <- c("Catch","Me","If","You","Can")
for(i in title){
  print("Thank you")
}

for (word in title) {
    print(word)  
}

#since for loop doesnt save elements hence we have to store explicitly 

title <- c("Catch","Me","If","You","Can")
new.title <- vector(length = 5)
new.title

for (i in 1:5) {
  new.title[i] <- title[i] 
}
new.title

#while loop
n <- -5
while (n < 2) {
  print(paste("the value is ",n))
  n <- n+1
}

#repeat loop

n <- -5

repeat{
  
  print(paste("Your value equals",n))
  n <- n + 1
  if(n >= 5){
    print("N is now either 5 or larger")
    print("The loop will be broken")
    break 
  }
  
}

#Building a function

coup <- matrix(rep(c("Duke", "Assassin", "Captain", "Ambassador",
                     "Contessa"),3), ncol = 1)

deal <- function(deck){
  
  cards <- sample(deck, size = 3 , replace = T)
  print(cards)
}
deal(deck = coup)

cit <- matrix(c("Magistrate","Thief","Wizard","Parrician","Bishop","Queen","Witch","Blackmailer","Magician","Emperor","Tax collector",
                "Spy","Seer","Merchant","Scholar","Trader","Architect","Marshal","Abbot","Alchemist","Warlord","Diplomant","Artist"), ncol = 1)

shuffle <- function(deck){
  random <- sample(1:23, size = 23)
  deck.s <- deck[random, ,drop = FALSE]
  print(deck.s)
}
shuffle(cit)

#scopping
########
######
#######
#

#***************************Dataframe***************************************************

title <- c("Star Wars", "The Empire Strikes Back", "Return of the Jedi",
           "The Phantom Menance", "Attack of the Clones", "Revenge of the Sith",
           "The Force Awakens")
year <- c(1977,1980, 1983,1999, 2002, 2005, 2015)
length.min <- c(121, 124, 133, 133, 142, 140, 135)
box.office.mil <- c(787, 534, 572, 1027, 657, 849, 2059)
#syntax of a dataframe
my.data <- data.frame(title, year, length.min, box.office.mil)
my.data
#to rename a data frame
names(my.data) <- c("Movie Title", "Release Year", "Length in minutes", "Box Office")
my.data
#Another way to rename the dataframes
my.data <- data.frame(Title= title, Year = year, Length = length.min, Gross = box.office.mil)
my.data
#the result of str(data.frame) gives ouput as list
#as discussed dataframes are 2 dimentional form of list
str(my.data) #but its names are of type "Factors"

#if we dont want the type of colnames to be Factors then set stringsAsFactors = FALSE
my.data <- data.frame(Title= title, Year = year, Length = length.min,
                      Gross = box.office.mil, stringsAsFactors = FALSE)
str(my.data)

#Master package - Tidyverse
install.packages("tidyverse")

getwd()
setwd("F:/DSP13")
getwd()
titanic <- read.table("Titanic_Formatted.csv", sep = ",", header = TRUE, stringsAsFactors = F)
titanic
#if we use cvs file consistently then read.csv is used where sep= "," and header=T Default
titanic <- read.csv("Titanic_Formatted.csv", stringsAsFactors = FALSE)
titanic

?read.delim  #tab delimited files

#in some part of the world they use ; as seperated  (sep = ";")
read.csv2()
read.delim2()

#if we need only some observation out of huge data set then we use nrow inside read.csv
titanic <- read.csv("Titanic_Formatted.csv", stringsAsFactors = FALSE, nrows = 50)
titanic

#Exporting data
title <- c("Star Wars", "The Empire Strikes Back", "Return of the Jedi",
           "The Phantom Menance", "Attack of the Clones", "Revenge of the Sith",
           "The Force Awakens")
year <- c(1977,1980, 1983,1999, 2002, 2005, 2015)
length.min <- c(121, 124, 133, 133, 142, 140, 135)
box.office.mil <- c(787, 534, 572, 1027, 657, 849, 2059)
my.data <- data.frame(title, year, length.min, box.office.mil)

#above is a data set created and exported using below function
write.csv(my.data, file = "MyFirstDataFrame.csv",
          row.names = FALSE)
#file name should end with .csv extention
# for above, if row.names = T then it concatenates extra column to indicate SL/no.

#Six essential function
titanic

nrow(titanic)   #size of rows
ncol(titanic)   #size of columns


colnames(titanic) #returns column names of the data set
rownames(titanic) #returns row name of the data set which is useless in this dataset

str(titanic) #it gives the overall summary which includes both no of rows and columns
#also type of each column and some values involved in each

summary(titanic) #it returns descriptive statistics for each ccolumn also MIN and MAX value

#####Indexing and slicing a data frame

library(tidyverse)
starwars
my.wars <- data.frame(starwars)
my.wars[11:13]
my.wars <- my.wars[ ,-(11:13)]
my.wars
head(my.wars)
tail(my.wars)
my.wars[3,9]
my.wars[3, ]
head(my.wars[ ,1])          #returns result as dataframe
head(my.wars[["name"]])     #returns same result as vectors which is same as list. dataframe is a 2D list anyway
head(my.wars["name"])
my.wars <- my.wars[c(1:7),c("name","height","mass","hair_color")]
my.wars

###########extending a dataframe

mark <- c(37.50,34.75,35.25,0.00,0.00,0.75,0.00)
carrie <- c(13.50,22.75,21.25,0.00,0.00,0.50,5.75)

my.wars$MarkScreenTime <- mark
my.wars$CarrieScreenTime <- carrie
my.wars

my.wars$MarkScreenTime <- NULL
my.wars$CarrieScreenTime <- NULL
my.wars

my.wars[["MarkScreenTime"]] <- mark
my.wars[["CarrieScreenTime"]] <- carrie
my.wars

my.wars$MarkScreenTime <- NULL
my.wars$CarrieScreenTime <- NULL
my.wars

my.wars <- cbind(my.wars, MarkScreenTime = mark, CarrieScreenTime = carrie)
my.wars

rogueone <- data.frame("Rogue One",135, 76, "blond", 0, 0.2)
rogueone
rbind(my.wars, rogueone)     #Returns ERROR, because names are not defined.

rogueone <- data.frame(name="Rogue One",height= 135, mass = 76, hair_color = "blond", MarkScreenTime =  0, CarrieScreenTime = 0.2)
rogueone
rbind(my.wars, rogueone)

data()     #will showws the dataset

##################Dealing with missing data

head(my.wars)
is.na(my.wars)
any(is.na(my.wars))
any(is.na(my.wars$name))
is.na(my.wars[ ,c("name", "homeworld", "species")])
my.wars$species[is.na(my.wars$species)] <- "Unknown"
my.wars$species
subset(my.wars, species == "Unknown")
colnames(my.wars)
my.wars$height[is.na(my.wars$height)] <- median(my.wars$height, na.rm = T)
my.wars$height

######################dplyr Package**********************************

library(tidyr)

star <- starwars
star

View(star) #returns star data is systamatic way in a new tab

#filter function
View(filter(star, species == "Droid"))
filter(star, species == "Droid", homeworld == "Tatooine")
filter(star, eye_color == "red" | eye_color == "yellow" | eye_color == "blue",
       species == "Human")

#Select function, it displays the variable we mention
select(star, name, height, mass, hair_color, skin_color)
select(star, name : gender )      #returns all the columns between specified


select(star, ends_with("color"))  #returns the coloumns which are ending with specified name
select(star, name, birth_year, gender, everything())  #the colnames defined at the start will be displayed first and then everything is displayed

#Mutate and transmute function
star[ ,c("films","vehicles", "starships")] <- NULL #ignore this line, i performed this to make BMI visible
star
star <- mutate(star, bmi = mass/(height/100)^2)
select(star, films:bmi)

star.trans <- transmute(star, bmi2 = mass/(height/100)^2)
star.trans

#arrange, Grouping data and summerising

arrange(star, mass)
arrange(star, desc(mass))

#summerize returns collapse entire column into a single value
summarize(star, avg.height = mean(height, na.rm = T))

#hence we pair it with group_by()

star.species <- group_by(star, species)
summarize(star.species, avg.height = mean(height, na.rm=T))

#Extracting ramdon data *sample*

sample_n(star, 10)  #returns 10 samples
sample_frac(star, 0.1)  #returns 10% of samples

#*************PIPE operator in R*********
View(star)
star.species <- group_by(star,species)
star.species
star.smr <- summarise(star.species, count = n(), avg.mass = mean(mass, na.rm = T))
star.smr
str(star)
filter(star.smr,count > 1) 

filter(summarise(group_by(star, species), count = n(), avg.mass = mean(mass, na.rm = T)), count > 1)
star
star %>%
  group_by(species) %>%
  summarise(count = n(), avg.mass = mean(mass, na.rm = T)) %>%
  filter(count > 1)
  
#Tidyr in R- gather() and seperate()

library(tidyverse)
#example1
billboard <- read.csv("F:/DSP13/billboard.csv")
View(billboard)
billboard <- as.tibble(billboard)
?tibble
billboard

billboard %>% gather(x1st.week:x76th.week, key = "week", value = "rank", na.rm = T) %>% arrange(artist.inverted)

#Example with a small data set for gather and seperate
classdata <- read.csv("CaseforGather_Seperate.csv", stringsAsFactors = F)
View(classdata)
detailed.view <- spread(classdata, key = "Class", value = "Grade")
args(gather)
wide.view <- gather(detailed.view, key = "Class.Subject", value = "Gradeof.students", 2:4)
View(wide.view)

args(spread)

#example 3
tbdata <- read.csv("F:/DSP13/tb.csv", stringsAsFactors = F)
View(tbdata)
tbdata
tbdata <- as.tibble(tbdata)
tb.gathered <- tbdata %>% gather(new_sp : new_sp_fu, key = "column", value = "Cases",
                                 na.rm = T) %>%
  arrange(iso2)
View(tb.gathered)
tb.gathered
tb.seperated <- tb.gathered %>% separate(column, into = c("sex","age"))


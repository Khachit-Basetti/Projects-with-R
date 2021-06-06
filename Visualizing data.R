#VISUALIZING DATA 
#working on TITANIC DATA
library(tidyverse)
titanic <- read.csv("titanic.csv", stringsAsFactors = F)
View(titanic)
df <- as.tibble(titanic)
df
df$survived <- as.factor(df$survived)
df$pclass <- as.factor(df$pclass)
df$sex <- as.factor(df$sex)
df$embarked <- as.factor(df$embarked)
str(df)

hist <- ggplot(data = df, aes(x = age))  #layers of graph
hist
#using data visualization with ggplot2 cheat sheet defined below
hist + geom_histogram(binwidth = 5, color = "gray", fill = "darkslategray4", alpha = 0.5) +
  ggtitle("Age Distribution on the Titanic") +  #Title for the graph
  labs(y = "Number of Passengers", x = "Age") + #naming the y and x axis
  theme_minimal() #decreases the background grid shade


#using BAR chart with Titanic data

#*
bar <- ggplot(df, aes(x = survived))
bar + geom_bar()

#**
bar <- ggplot(df, aes(x= survived))
bar + geom_bar() + theme_classic() +
  labs(y = "Passenger Count", title = "Survival Rate on the Titanic")

#***
bar <- ggplot(df, aes(x= survived, fill = survived))
bar + geom_bar() + theme_classic() +
  labs(y = "Passenger Count", title = "Survival Rate on the Titanic")

#***
bar <- ggplot(df, aes(x = survived, fill = sex))
bar + geom_bar() + theme_classic() +
  labs( y = "Passenger Count", title = "Survival Rate on the Titanic")

#****
bar <- ggplot(df, aes(x = sex, fill = survived))
bar + geom_bar() + theme_classic() +
  labs(y = "Passenger count", x = "Gender", title = "Survival Rateon the Titanic")
#*****
bar <- ggplot(df, aes(x = sex, fill = survived))
bar + geom_bar() + theme_light() + 
  labs(y = "Passenger count", x = "Gender", 
       title = "Servival rate on Titanic Ship") +
  facet_wrap( ~pclass) #pclass(passenger class), ~(by)

colnames(df)
#******

bar <- ggplot(df, aes(x = sex, fill = survived))
bar + geom_bar() + theme_minimal() + 
  labs(y = "Passenger count", x = "Gender", title = "Servival rate on the Titanic ship") +
  facet_wrap(sex ~ pclass)
#Here we can see for each passenger class the people survived and dead

#*******

hist <- ggplot(df, aes(x = age, fill = survived))
hist + geom_histogram(binwidth = 5 , color = "white") + theme_solid() +
  labs( y = "Passenger count", x = "Age", title = "Survival rate on the Titanic group")

#BOX PLOT with Titanic data

#*
#a simple boxplot
my.box <- ggplot(df, aes(y = age, x = survived))
my.box + geom_boxplot() + theme_grey() + 
  labs(y = "Age", x = "Survived", title = "Survival rate on the titanic data")

#**
#A box plot with values on box plot to know the diagram in detail using geom_jitter 
#it makes it more detailed, Width is where the width with which values are spread
#aesthetics are not confined to the initial ggplot call
#when defining aestetics in iniial ggplot function it will be inherited by all the layers
my.box <- ggplot(df, aes(y = age, x = survived))
my.box + geom_boxplot() + theme_grey() + geom_jitter(width = 0.3, aes(color = sex))+
  labs(y = "Age", x = "Survived", title = "Survival rate on Titanic ship based on age and gender") 

#***
#in below, we have modified the pointer of outlier using 
#variable which is inside geom_boxplot
my.box <- ggplot(df, aes( y = age, x = survived))
my.box + geom_boxplot(outlier.color = "red", outlier.shape = 4)+
  geom_jitter(width = 0.3, aes(color = sex)) +
  labs(y = "Survived", x = "Age", title = "Survival rate on Titanic ship based on age and gender")

#****
# here we are flipping the box plot by using coord_flip() function
my.box <- ggplot(df, aes(y = age, x = survived)) 
my.box + geom_boxplot(outlier.colour = "red", outlier.shape = 4)+
  geom_jitter(width = 0.3, aes(color = sex)) + theme_classic() +
  labs(x = "age", y = "Survived", title = "Survival on titanic ship based on Age and Gender")+
  coord_flip()


#*****
#SCATTER PLOT
#here we use geom_point() to plot a scattered plot
#find the CI vs DI dataset
#corruption <- read.csv(CorruptionindexVSdevelopmentIndex.data, stringsAsFactors = F)
#View(corruption)
#ci <- ggplot(corruption, aes(x = year))
#ci + geom_point(aes(color = Jurisdiction))+ theme_classic() + 
#  labs(x = "Years of progress", y= "Corruption index", title = "Survey on Corruption index every year")


#visualisation with Jared
data("diamonds")
View(diamonds)
attach(diamonds)
ggplot(diamonds, aes(x=carat , y= price)) + geom_point()
g <- ggplot(diamonds, aes(x = carat, y = price))
g + geom_point(aes(color = color))
g + geom_point(aes(color=color, shape = clarity))
#for above code, warining message coz ggplot can label max 6 variables
g +geom_point(aes(color = color, shape = cut))

#violin plot
ggplot(diamonds, aes(y = carat)) + geom_boxplot()

#boxplot for caret and cut out of diamond

ggplot(diamonds, aes(y = carat , x = cut)) + geom_boxplot()
ggplot(diamonds, aes(y = carat, x = cut)) + geom_violin()
g <- ggplot(diamonds, aes(y = carat, x = cut))
g + geom_point() + geom_violin()
g +geom_violin() + geom_point() # whatever order we provide that way we get the output
g +geom_jitter() + geom_violin()  

#---------------###----------------###------------------##
data("economics")
nrow(economics)
head(economics)
?economics
attach(economics)
View(economics)

ggplot(economics, aes(x = date, y = pop)) + geom_line()
library(lubridate)
economics$year <- year(economics$date)
economics$month <- month(economics$date)
head(economics)

econ2000 <- economics[which(economics$year >= 2000),]

head(econ2000)

nrow(economics)
nrow(econ2000)

attach(econ2000)
econ2000$month <- month(econ2000$date, label = TRUE) #coverts the #mm format to jan/feb...etc
head(econ2000)

library(scales)  #comes under ggplot2 package
g <- ggplot(econ2000, aes(x=month, y=pop))
g <- g + geom_line(aes(color = factor(year), group = year))
g
g <- g +scale_color_discrete(name = "Year")
g <- g + scale_y_continuous(labels = comma)
g <- g+labs(title = "Population Growth", x = "Month", y= "Population")
g
g <- g+theme(axis.text.x = element_text(angle = 90))
g

#--------------------###------------------------###
View(diamonds)
g <- ggplot(diamonds, aes(x = carat, y = price))
g + geom_point(aes(color = color)) + facet_wrap(~color)
#FACET_WRAP take data and makes long strip out of it as necessary to fit in plot

g + geom_point(aes(color = color)) + facet_grid(cut~clarity)

ggplot(diamonds, aes(x= carat)) + geom_histogram() + facet_wrap(~color)


ggplot(diamonds, aes(x = carat, y = price, color = color, shape = cut, size = depth))+
  geom_point() + scale_color_discrete(name = "Color of Diamonds") +
  scale_size(name = "Depth") + scale_shape(name = "Cut of diamonds")+
  labs(title = "Price of Diamonds interms of Carat", x = "Diamonds in Carat", y = "Price")

#For themes by hadley wickham
#install.packages("ggthemes")
library(ggthemes)
g <- ggplot(diamonds, aes(x = carat, y = price, color = color))+ geom_point()
g + theme_wsj()
g + theme_economist() + scale_color_economist()
g + theme_tufte()

g + theme_excel() + scale_color_excel()


#for the use of web graphics
#when we excecute the below graphs you see output in the viewer section

#install.packages("ggvis")
library(ggvis)
data("cocaine")
head(cocaine)
#differnce btw ggplot and ggvis

#first with ggplot
library(ggplot2)
ggplot(cocaine, aes(x= weight, y = price)) + geom_point()

#with ggvis

cocaine %>% ggvis(x = ~weight, y = ~price) %>% layer_points()

#inplace of colour we use fill in ggvis
cocaine %>% ggvis(x= ~weight, y = ~price, fill = ~potency) %>% layer_points()

#Stroke is used to color the outline
cocaine %>% ggvis(x= ~weight, y= ~price, stroke= ~potency) %>% layer_points()

#if i want the points to be of my choice of color then we use := sign to fill
#   := is used to assign any particular thing inside ggvis
cocaine %>% ggvis(x= ~weight, y= ~price, fill:="green") %>% layer_points()

# layer smooth gives the best fit line in scatter plot
cocaine %>% ggvis(x= ~weight, y= ~price, 
                  fill:= ~potency) %>% layer_points() %>% layer_smooths()

#this is a size and opacity control of dots which is Rshinny, after you see the 
#variations you need to click in console and press ESC button
cocaine %>% ggvis(x =~weight, y= ~price, fill =~potency, size:= input_slider(10,100),
                  opacity:= input_slider(0,1)) %>% layer_points()

#histogram diffrences in ggvis and ggplot
ggplot(cocaine, aes(x= weight)) + geom_histogram()

cocaine %>% ggvis(x= ~weight) %>% layer_histograms()

#--------------------###------------------------------##

#new package installing from GITHUB for #D3 lib
install.packages("devtools")
library(devtools)

devtools::install_github('ramnathv/rCharts')

library(rCharts)
attach(iris)
head(iris)
#global substitution gsub
names(iris) <- gsub("\\.", "", names(iris))
head(iris)

#scatter plot in rCharts

rPlot(SepalLength ~ SepalWidth | Species, color = "Species",
      type = "point", data = iris) #point for scatter pot


#--------------------###-------------------------------#

hairEye <- as.data.frame(HairEyeColor)
head(hairEye)
attach(hairEye)
rPlot(Freq ~ Hair | Eye, color = "Eye", type = "bar", data = hairEye)

#D3 lib
data(economics, package = "ggplot2")
head(economics)
tail(economics)
attach(economics)
economics$date <- as.character(economics$date) #as its easy to plot
str(economics)
head(economics)
?mPlot() #no documentation available
###
m1 <- mPlot(x="date", y= c("psavert","uempmed"), type = "Line", data = economics)
m1$set(pointSize = 0, linewidth=1)
m1
###

##WORKing WITH MAPS
map1 <- Leaflet$new()
map1
map1$setView(c(12.9716, 77.5946),zoom = 13)
map1

##########-----------3##-----------########

#pizza data and using the php format data and build a chart
#install.packages("rjson")
library(rjson)
library(plyr)
#fromJSON(temp) %>% as.data.frame
library(dplyr)
pizzaJson <- fromJSON(file = "http://www.jaredlander.com/data/PizzaPollData.php")
head(pizzaJson)

pizza <- data.frame(Reduce(rbind, pizzaJson))


pizza <- ldply(pizzaJson, as.data.frame)
head(pizza)
str(pizza)
library(rCharts) #for nPlot
pizzaPlot <- nPlot(Percent ~ Place, data = pizza, type ="multiBarChart", 
                   group = "Answer")
pizzaPlot$xAxis(axisLabel = "Pizza Place", rotateLabels = -45)
pizzaPlot$yAxis(axisLabel = "Percent")
pizzaPlot$chart(reduceXTicks = FALSE)
pizzaPlot #its blank in Viewer but it appears after you click the web output


pizzaJson <- lapply(pizzaJson, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

pizza<- data.frame(do.call("rbind", pizzaJson))
head(pizza)
str(pizza)

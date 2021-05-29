####to read excell files #############

install.packages("readxl")
library("readxl")
library(readxl)
highlow <- read_excel("EPS New High or Low and away from Majority.xlsx")
View(highlow)

## 2nd method- in the 1st quadrant >> import dataset >> browse the file >> import


######### to read XML files ###############

library(XML)
library(RCurl)
library(httr)
url = "https://en.wikipedia.org/wiki/2016_Summer_Olympics_medal_table"
#webpage we are intrested in

urldata <- getURL(url)#get data from the above URL


data <- readHTMLTable(urldata, stringsAsFactors = FALSE)


#medal tally
names(data)
head(data)
x = data$ '2016 Summer Olympics medal table'

head(x)


####### Read in data from wikipedia HTML tables   -        THIS WORKS
library(rvest)

url <- "https://en.wikipedia.org/wiki/2016_Summer_Olympics_medal_table"
#//*[@id="mw-content-text"]/div/table[2]

medal_tally <- url %>% read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>% html_table(fill= TRUE)

medal_tally <- medal_tally[[1]]
head(medal_tally)


#WHS sites in the UK

url2 = "https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_the_United_Kingdom"

whsuk <- url2 %>% read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>% html_table(fill = TRUE)

whsuk <- whsuk[[1]]
head(whsuk)





######## %operator

library(dplyr)
library(magrittr)

head(iris)

iris %>% head()


#select columns
iris %>% select(Species, Petal.Width) -> col2

col2 %>% head()

#drop the column
iris %>% select(-Species) %>% head()


#select columns

iris %>% select(Sepal.Length : Petal.Length) %>% head()


iris %>% select_if(is.numeric) %>% head()


iris %>% select(starts_with("S")) %>% head()

iris %>% select(ends_with("s")) %>% head()

iris %>% select(contains("Width")) %>% head()

iris %>% select(contains("Length")) %>% head()


#####################################


getwd()
setwd("C:/DataScience/Estimates")
df <- read.csv("world-happiness-report-2019.csv")

library(tidyverse)

head(df)

df %>% glimpse()

#select
df %>% select("SD.of.Ladder") %>% head()

df %>% select(one_of("Social.support", "Freedom"))

#Drop columns by name

df %>% select(-one_of("Social.support", "Freedom")) %>% head()

df %>% select(starts_with("Fr")) %>% head()

df %>% select(ends_with("m")) %>% head()

############################# More select 
#### when we dont have the exact column name

df %>% select(matches("Free"))

df %>% select(matches("Lad")) %>% head()

x <- c("Ladder", "Freedom")

df %>% select(one_of(x)) 

#select in the basis of data type

df %>% select_if(is.numeric) %>% glimpse()

#selecting column which are not numeric

df %>% select_if(~!is.numeric(.))%>% glimpse()

df %>% select_if(~!n_distinct(.) <5) %>% glimpse()  #unique values

#remining
df %>% select(Country = Country_region) %>% glimpse()


##### row filtering subset rows

library(tidyverse)

data(msleep)
head(msleep)
m= msleep

m %>% glimpse()

m %>% filter(bodywt >= 5) %>% glimpse()



table(m$conservation)
m %>% filter(conservation == "en") %>% glimpse()

#filter on basus of two condition

m %>% filter(bodywt >= 2, conservation == "en") %>% glimpse() # , is like an AND operator

m %>% filter(order == "Carnivora"| conservation == "en") %>% glimpse() # | represents OR


#######################################################################
# if it do not meet a condition then use !

m %>% filter(order != "Carnivora") %>% glimpse()

### multiple row conditions

m %>% filter( conservation %in% c("en", "vu"))


#########################################################

##chaining pipe operator

m %>% select(matches("vore")) %>% 
  filter(vore == "omni") %>% glimpse()


############################# Mutate() ##################
##    Create a new column/variable

m %>% glimpse()

m %>% mutate(S = sleep_total + sleep_cycle) %>% glimpse()

m %>% mutate(S = sleep_total + sleep_cycle) %>% filter(!is.na(sleep_rem)) %>% glimpse()

m %>% mutate(S = sleep_total + sleep_cycle) %>% filter(!is.na(sleep_rem),!is.na(sleep_cycle) ) %>% glimpse()
 

##  Create a var and remove the older one

m %>% transmute(S= sleep_total + sleep_rem + sleep_cycle) %>% glimpse()

###########################################################

m %>% glimpse()

m %>% group_by(conservation) %>% summarise(n())

m %>% group_by(conservation) %>% summarise(mean_a = mean(awake, na.rm= TRUE))

m %>% group_by(conservation, vore) %>% summarise(mean_a = mean(awake, na.rm= TRUE))


########

df <- read.csv("athlete_events.csv")

head(df)

df %>% glimpse()

s2016 <- df %>% filter(Year == 2016, Season == "Summer")

s2016 %>% head()

s2016 %>% group_by(Sport) %>% 
  summarise(mean_age(Age)) %>% head()



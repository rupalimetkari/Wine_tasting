#Final PRoject

#Load libraries
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(hexbin)
library(corrplot)


#upload data
wine <- read.csv("D:/Assignmentas/Wine_tasting.csv")

#basic details of data
str(wine)
summary(wine)
head(wine)

#clean data - remove special character, fill NA values 
wine$price[is.na(wine$price)]<-mean(wine$price,na.rm=TRUE)
wine$price <- ceiling(wine$price)
wine$price
table(wine$price)

#Old country wines 
oldCountries <- c("Italy","Portugal","Spain","France","Germany","Austria","Greece","Romania","Hungary","Israel")
old_wines <- filter(wine, country %in% oldCountries)
nrow(old_wines)

#New country wines
newCountries <- c("US","Chile","Australia","South Africa","Argentina","Mexico","Canada","New Zealand")
new_wines <- filter(wine, country %in% newCountries)
nrow(new_wines)

#Checking for null values in country,points column
sum(is.na(old_wines$country))
sum(is.na(old_wines$points))
sum(is.na(old_wines$price))
sum(is.na(new_wines$country))
sum(is.na(new_wines$points))
sum(is.na(new_wines$price))


#replacing null values in price by median for both subsets
old_wines$price[is.na(old_wines$price)]<-median(old_wines$price, na.rm=TRUE)
sum(is.na(old_wines$price))

new_wines$price[is.na(new_wines$price)]<-median(new_wines$price, na.rm=TRUE)
sum(is.na(new_wines$price))


#mean of points and price
mean_old_wine_points = mean(old_wines$points)
mean(old_wines$points)
mean_new_wine_points = mean(new_wines$points)
mean(new_wines$points)

mean_old_wine_price = mean(old_wines$price)
mean(old_wines$price)
mean_new_wine_price = mean(new_wines$price)
mean(new_wines$price)


#1. Two sample t.test- based on points
#Null Hypothesis- old countries wines are tastey Accept null
t.test(old_wines$points,new_wines$points)


#2. One t.test based on price
#Null Hypothesis- old countries wines are costly reject null 
t.test(old_wines$price, mu = mean_new_wine_price)

#number of wines made in each country
wine_country <- group_by(wine, country) %>%
  dplyr:: summarize(Count= n()) %>%
  filter(Count> 30) %>%
  arrange(desc(Count))
head(wine_country,20)

#Country count
ggplot(wine_country, aes(reorder(country, Count), Count, fill=country)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Count), hjust=-.1, vjust=.25) +
  ggtitle("Frequncy of wines made in each country")+
  xlab("Country") + 
  ylab("# of Wines Reviewed") +
  ylim(0, 500) +
  guides(fill=FALSE) +
  coord_flip()

#comapre country and provience  wines ANOVA test
CA_France_Italy <- filter(wine, country==c("France", "Italy") | province == "California")

ggplot(CA_France_Italy, mapping= aes(x=country, y=points, fill=country)) + geom_boxplot()+
  stat_summary(fun.y="mean", geom="point", shape=21, fill="red") +
  scale_x_discrete(labels=c("France", "Italy", "California")) +
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank())

#histogram to ensure that each of these groups of data are normally distributed.
ggplot(CA_France_Italy, aes(x=points, fill=country)) + geom_histogram()+ labs(title="Points for the wines in each region")

#test points and country
n <- aov(CA_France_Italy$points ~ CA_France_Italy$country)
summary(n)
TukeyHSD(n)

#price and country
m <- aov(CA_France_Italy$price ~ CA_France_Italy$country)
summary(m)
TukeyHSD(m)

#correlation between points and price
ggplot(wine,aes(x=points, y=price)) + geom_point() + geom_hex(bins = 50) + labs(title="Scatter plot Points VS Price")

#liner regression
lr <- lm(wine$price ~ wine$points)
summary(lr)

# To quadratic function would better fit the data.
wine$points2 <- wine$points^2 
n <- lm(wine$price ~ wine$points2 + wine$points)
summary(n)

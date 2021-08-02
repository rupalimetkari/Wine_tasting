#Milestone_2 = wine_tasting data

library(plyr)
library(dplyr)

#upload data
wine <- read.csv("D:/Assignmentas/Wine_tasting.csv")
wine

#basic details of data
str(wine)
summary(wine)
head(wine)

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

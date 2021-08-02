library(ggplot2) 
library(plyr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(FSA)
library(plotrix)
library(moments)
library(modeest) 
library(plotly)
library(gmodels)
library(stringr)
library(descr)
#upload data 
wine <- read.csv("D:/Assignmentas/Wine_tasting.csv", na.strings=c(""," ","NA"))
wine
#structure of data, list of varibles
str(wine)
summary(wine)
head(wine , n=10)
tail(wine , n=5)
ls(wine)

#clean data - remove special character, fill NA values 
wine$price[is.na(wine$price)]<-mean(wine$price,na.rm=TRUE)
wine$price <- ceiling(wine$price)
wine$price
table(wine$price)

Data1 <- gsub("[^0-9A-Za-z///' ]","'" , wine$province ,ignore.case = TRUE)
wine$province <- gsub("'","" , Data1 ,ignore.case = TRUE)
table(wine$province)

region_1 <-data.frame(wine %>% fill(region_1, .direction = "down"))
region_1
Data2 <- gsub("[^0-9A-Za-z///' ]","'" , wine$region_1 ,ignore.case = TRUE)
wine$region_1 <- gsub("'","" , Data2 ,ignore.case = TRUE)
table(wine$region_1)

region_2 <- data.frame(wine %>% fill(region_2, .direction="updown"))
region_2
Data3 <- gsub("[^0-9A-Za-z///' ]","'" , wine$region_2 ,ignore.case = TRUE)
wine$region_2 <- gsub("'","" , Data3 ,ignore.case = TRUE)
table(wine$region_2)

taster_name <- data.frame(wine %>% fill(taster_name, .direction = "downup"))
taster_name
Data4 <- gsub("[^0-9A-Za-z///' ]","'" , wine$taster_name ,ignore.case = TRUE)
wine$taster_name <- gsub("'","" , Data4 ,ignore.case = TRUE)
table(wine$taster_name)

taster_twitter_handle <- data.frame(wine %>% fill(taster_twitter_handle, .direction = "up"))
taster_twitter_handle
Data5 <- gsub("[^0-9A-Za-z///' ]","'" , wine$taster_twitter_handle ,ignore.case = TRUE)
wine$taster_twitter_handle <- gsub("'","" , Data5 ,ignore.case = TRUE)
table(wine$taster_twitter_handle)

Data6 <- gsub("[^0-9A-Za-z///' ]","'" , wine$title ,ignore.case = TRUE)
wine$title <- gsub("'","" , Data6 ,ignore.case = TRUE)
table(wine$title)

Data  <- gsub("[^0-9A-Za-z///' ]","'" , wine$variety ,ignore.case = TRUE)
wine$variety <- gsub("'","" , Data ,ignore.case = TRUE)
table(wine$variety)


#finding subsets of data and Descriptive Statistics
subset.data <- subset(wine, select = c("country", "price", "points", "variety"))
str(subset.data)
min(subset.data$points); max(subset.data$points)
descr(subset.data)

quantile(wine$price, prob = c(.25, .50, .75, .95))
quantile(wine$points, prob = c(.25, .50, .75, .95))

#In the sample, 90 price is at what percentile rank?
length(wine$price[wine$price <= 90])/length(wine$price) * 100

#In the sample, 100 is at what percentile rank?
length(wine$price[wine$price <= 85])/length(wine$price) * 100

#giveing rating based on points
wfact = cut(wine$points, 3)
wfact
wfact = cut(wine$points, 3, labels=c('bad', 'good', 'better')) 
table(wfact) 
ggplot(wine, aes(x = wfact , fill=wfact )) + geom_bar(stat = "count") + scale_fill_hue(c=40) + labs(title="Given rating based on points",x="Taste of wine", y = "Count")

#histogram
ggplot(wine, aes(x = points , fill=points )) + geom_bar(stat = "count") + scale_fill_hue(c=40) + labs(title="frequncy of  wine highest points",x="wine points", y = "Count")

#correlation between points and price 
cor(wine$points, wine$price)
plot( x=wine$points, y=wine$price, main = "Scatter plot Points VS Price",
      xlab = "points of wine", ylab = "price of wine", pch=19,col=c("red","blue"))
abline(lm( wine$price ~  wine$points, data = survey), col = "green")
legend("topleft", legend=c("points", "price"),col=c("red", "blue"), pch = 19, cex=0.8)

#frequncy pf highest country names 
ggplot(wine, aes(x =wine$country ,fill = wine$country), xlab="Undegraduate Majors") + geom_histogram(stat = "count")+  labs(title="Frequency of highest country names for wine",x="Country Names", y = "Count")      


#find outliers values using plot boxplot
OutVals = boxplot(wine$price,
                  main = "Boxplot for price",
                  ylab = "Price of wine",
                  col = "orange",
                  border = "brown"
                  )$out
OutVals



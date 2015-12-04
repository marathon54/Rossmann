# Author: Eduardo Kamioka - ekamioka

# Testing some simple linear regression (initially achieved the same score as using random forest )
library(dplyr)
train <- read.csv("~/_data/Rossmann/train.csv", header = T)
test <- read.csv("~/_data/Rossmann/test.csv", header = T)
store <- read.csv("~/_data/Rossmann/store.csv")

train <- merge(train,store)
test <- merge(test,store)

ajeita_pa_mim <- function(dt){
        # replacing NA's by the mean value  
        dt$CompetitionDistance[is.na(dt$CompetitionDistance)] = round(mean(dt$CompetitionDistance, na.rm = T))
        dt$CompetitionOpenSinceMonth[is.na(dt$CompetitionOpenSinceMonth)] = round(mean(dt$CompetitionOpenSinceMonth, na.rm = T))
        dt$CompetitionOpenSinceYear[is.na(dt$CompetitionOpenSinceYear)] = round(mean(dt$CompetitionOpenSinceYear, na.rm = T))
        dt$Promo2SinceWeek[is.na(dt$Promo2SinceWeek)] = round(mean(dt$Promo2SinceWeek, na.rm = T))
        dt$Promo2SinceYear[is.na(dt$Promo2SinceYear)] = round(mean(dt$Promo2SinceYear, na.rm = T))
        dt$Open[is.na(dt$Open)] = round(mean(dt$Open, na.rm = T))
        
        # converting to numeric
        dt$StateHoliday = as.numeric(dt$StateHoliday)
        dt$StoreType = as.numeric(dt$StoreType)
        dt$Assortment = as.numeric(dt$Assortment)
        dt$PromoInterval = as.numeric(dt$PromoInterval)
        
        # seperating out the elements of the date column for the dt set
        dt$Date = as.Date(dt$Date, format = "%Y-%m-%d")
        dt$month <- as.integer(format(dt$Date, "%m"))
        dt$year <- as.integer(format(dt$Date, "%y"))
        dt$day <- as.integer(format(dt$Date, "%d"))
        
        # removing the date columns (Date not used) (Customers affect simetry) (CompetitionOpenSinceYear not relevant/correlated)
        #dt$Date = NULL
        dt$Customers = NULL
        dt$CompetitionOpenSinceYear = NULL
        
        return(dt)
}


train1 = ajeita_pa_mim(train)
test1 = ajeita_pa_mim(test)

test<-filter(train1, Date>='2015-6-15')
train<-filter(train1, Date<'2015-6-15')
cor(train)

mod <- lm(Sales ~ DayOfWeek+Open+Promo+StoreType+Assortment+CompetitionDistance+Promo2+PromoInterval, data = train)

summary(mod)


y <- predict(mod, newdata = test)
x1<-merge()

predictions<-predict(mod,newdata=test)
error<-sqrt((sum((test$Sales-predictions)^2))/nrow(test))
error
---
        
        
library(e1071)
svm_fit<-svm(Sales~DayOfWeek+Open+Promo+StoreType+Assortment+CompetitionDistance+Promo2+PromoInterval,data=train)
svm_predictions<-predict(svm_fit,newdata=test)
error<-sqrt((sum((test$Sales-svm_predictions)^2))/nrow(testing))
error

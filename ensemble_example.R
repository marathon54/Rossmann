#Ensemble example

#First model...R2 pf .6658
set.seed(10)
y<-c(1:1000)
x1<-c(1:1000)*runif(1000,min=0,max=2)
x2<-(c(1:1000)*runif(1000,min=0,max=2))^2
x3<-log(c(1:1000)*runif(1000,min=0,max=2))
lm_fit<-lm(y~x1+x2+x3)
summary(lm_fit)

#second model...
set.seed(10)
all_data<-data.frame(y,x1,x2,x3)
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))
training<- all_data[positions,]
testing<- all_data[-positions,]

lm_fit<-lm(y~x1+x2+x3,data=training)
predictions<-predict(lm_fit,newdata=testing)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

#third model...
library(foreach)
length_divisor<-6
iterations<-5000
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
        training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
        train_pos<-1:nrow(training) %in% training_positions
        lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])
        predict(lm_fit,newdata=testing)
}
predictions<-rowMeans(predictions)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error2<-sqrt((sum(((testing$y-predictions)/testing$y)^2))/nrow(testing))

error
error2

#first ensemble...
library(randomForest)
rf_fit<-randomForest(y~x1+x2+x3,data=training,ntree=500)
predictions<-predict(rf_fit,newdata=testing)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error2<-sqrt((sum(((testing$y-predictions)/testing$y)^2))/nrow(testing))

error
error2
summary(rf_fit)



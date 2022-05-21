##210701
#################################
install.packages("dplyr")
library(dplyr)
###################
getwd()
setwd("C:/dacon")
test<-read.csv("test.csv", sep=',', header=T)
str(test)
str(test[,14])

train<-read.csv("train.csv", sep=',', header=T)
names(train)[c(12,13)] <-c("지하철역수", "버스정류장수")
str(train)
train.kn<-subset(train,지역="경상남도" )


colnames(train.kn)
group_by(train.kn, 버스정류장수) %>% mutate(mean(임대료))



   

################################################
rm(list=ls())
train<-read.csv("train.csv", sep=',', header=T)
names(train)[c(12,13)] <-c("지하철역수", "버스정류장수")

str(train)
summart(train)

train$임대료<-as.numeric(train$임대료)
train$임대보증금<-as.numeric(train$임대보증금)
class(train$임대료) ; class(train$임대보증금)
summary(train)

sum( is.na(train$임대료) ) 

train$임대료<-ifelse(  is.na(train$임대료), 0, train$임대료   )
train$임대보증금<-ifelse(  is.na(train$임대보증금), 0, train$임대보증금   )

summary(train$임대료)  ; summary(train$임대보증금)

#######################
summary(train)
train.kn<-subset(train,지역=="경상남도")
train.kn  %>% group_by(버스정류장수) %>% mutate(round( mean(임대료),-2   ) ) %>% summarise(mean(임대료))
train.kb<-subset(train,지역=="경상북도")
train.kb  %>% group_by(버스정류장수) %>% mutate(mean(임대료) ) %>% summarise(mean(임대료))
######################
test<-read.csv("test.csv", sep=',', header=T)
names(test)[c(12,13)] <-c("지하철역수", "버스정류장수")
summary(test)
sum( is.na(test$지하철역수) ) 


sum( is.na(test$임대료) ) 
sum (is.na(   test$임대보증금) ) 


test$임대료<-as.numeric(test$임대료)
test$임대보증금<-as.numeric(test$임대보증금)



sum( is.na(test$임대료) ) 
summary(test$임대료)
sum (is.na(   test$임대보증금) ) 
summary(test$임대보증금)

sum( is.na(test$임대료) ) 
sum (is.na(   test$임대보증금) ) 



test$임대료<-ifelse(  is.na(test$임대료), 0, test$임대료   )
test$임대보증금<-ifelse(  is.na(test$임대보증금), 0, test$임대보증금   )



test.kn<- subset(test,지역=="경상남도")
summary(test.kn)
test.kn  %>% group_by(버스정류장수) %>% mutate( mean(임대료) ) %>% summarise(mean(임대료))



sum ( is.na(test.kn$임대료) ) 

################################
#아웃라이어 처리

boxplot(train$버스정류장수)$stats

train$버스정류장수<-ifelse(  is.na(train$버스정류장수), 6, train$버스정류장수   )
summary(train$버스정류장수)

boxplot(train$버스정류장수)$stats

?qt

is.na

quantile(train$버스정류장수, probs=seq(0.1, by=0.01))



quantile(test$버스정류장수, probs=seq(0.1, by=0.01))



table(test$버스정류장수)

train$버스정류장수<-ifelse(  train$버스정류장수>=15, 15, train$버스정류장수   )
summary(train$버스정류장수)

test$버스정류장수<-ifelse(  test$버스정류장수>=19, 19, test$버스정류장수   )
summary(test$버스정류장수)

getwd()

str()
write.csv(train, "train1.csv", row.names=F )
write.csv(test, "test3.csv", row.names=F )


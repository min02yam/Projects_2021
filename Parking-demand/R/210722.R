##210722
###########################
getwd() 
setwd("C:/dacon/re")
library(ggplot2)
library(corrplot)
library(dplyr)
###########################
m=15 #앞서, 20번 시행 함
test.result<-mice( test   , m=m, method = "sample", seed=0049, print=FALSE)
densityplot(test.result) 
test.completedData<-complete(test.result, 1 )


table(test.completedData$bus)
table(test$bus)

write.csv(test.completedData, "test0722.csv", row.names=F)

##cor확인

train.cor<-cor(train[,-1]) 
corrplot(train.cor, method="number",  type = "lower" ,addCoef.col = "black" ,  tl.col = "black", tl.srt = 45, diag = F) 
	#target num_park 0.86 이외 높은 상관계수 확인 안됨



################################
write.csv(train, "train0722(1).csv", row.names=F)
table(train$type_as)
##train$type_as2<-tapply(  train$type_as   ,train$code, mean )

##tapply(  train$area_pop   ,train$code, sum )
##Mfee_deposit<-




train$type_as2<-ifelse(train$type_as0==1 | train$type_as1==1, 2, 0)
#1이고0 -> 0
#0이고1-> 1


train$type_as0<-ifelse(train$type_as==0,1,0)
train$type_as1<-ifelse(train$type_as==1,1,0)
table( train$type_as2 ) 




train<-read.table("train0722(3).csv", sep=',', header=T)

train %>% filter(type_as0==1   type_as1==1) %>%head()

train2<-tapply(  train$type_as   ,train$code, mean )

train2<-train %>% group_by(code) %>%summarise(mean=mean(type_as))
head(train2,5)

write.csv(train2, "train0722(3)_1.csv", row.names=F)

train.join<-full_join(train,train2,by='code')
left


##210729
############################################
#경로 설정 및 라이브러리 로드
getwd()
setwd("C:/dacon/re")
install.packages("mice", dependencies=T)
library(mice)
#데이터 로드
train<-read.table("train.org0729.csv", sep=",", header=T)

summary(train)
str(train)

train$ fee_deposit<- as.numeric(train$ fee_deposit)
train$ fee_rent<- as.numeric( train$ fee_rent ) 

sum(is.na(train$ fee_deposit))




m=10
result<-mice( train  , m=m, method = "sample", seed=42, print=FALSE)
par(mfrow=c(2,2))
densityplot(result) 

completedData<-complete(result, 1 )
write.csv(completedData, "train0729.mice.csv", row.names=F)
###############################################################
test<-read.table("test.org0729.csv", sep=",", header=T)

summary(test)
test$ fee_deposit<- as.numeric(test$ fee_deposit)
test$ fee_rent<- as.numeric( test$ fee_rent ) 

table(test$type_qual)

test$type_qual<-ifelse(test$type_qual=="A",1,(
		ifelse(test$type_qual=="B",2, (
		ifelse(test$type_qual=="C",3, (
		ifelse(test$type_qual=="D",4, (
		ifelse(test$type_qual=="E",5, (
ifelse(test$type_qual=="F",6,(
ifelse(test$type_qual=="G",7, (
ifelse(test$type_qual=="H",8, (
ifelse(test$type_qual=="I",9, 
(ifelse(test$type_qual=="J",10, (
ifelse(test$type_qual=="K",11, (
ifelse(test$type_qual=="L",12,(
ifelse(test$type_qual=="M" |  test$type_qual=="O"  ,13,(
ifelse(test$type_qual=="N", 14, NA)
)  )    )))) )  )))))) )  )))    )))))))))

m=5
result<-mice( test  , m=m, method = "sample", seed=42, print=FALSE)
par(mfrow=c(2,2))
densityplot(result) 

completedData<-complete(result, 1 )
write.csv(completedData, "test0729.mice3.csv", row.names=F)


test<-read.table("test0729.mice3.csv", sep=",", header=T)



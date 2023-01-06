##210711
###############################################
install.packages("mice", dependencies=T)
install.packages(c("dplyr","corrplot"))
library(corrplot); library(dplyr); library(mice)
###################

getwd()
setwd("C:/dacon")
train<-read.csv("train1.csv", sep=",", header=T)

str(train)

table(train$지하철역수)
table(train$버스정류장수)

#1. 대중교통 factor로 변경
train$지하철역수<-as.factor(train$지하철역수)
train$버스정류장수<-as.factor(train$버스정류장수)
str(train)
##-> 굳이 factor변견 안해도 될 것 같음. 변수 많을 수록 교호작용까지 고려해야 함



train<-read.csv("trainm0710(1).csv", sep=",", header=T)   
str(train)


test<-read.csv("testm0710(1).csv", sep=",", header=T)   

####5. test 데이터 자격유형 결측치 대치 ( 그룹화 / 국민임대 - 자격유형별 - 임대료  )
sum( table(test$자격유형) ) 
nrow(test)
sum(table(train$자격유형))
nrow(train)


rm(list=ls())
####6.행복주택 지역별/임대료별로 보고 임퓨테이션
#강원도의 행복주택 C1786단지에서 임대료 임대보증금에 결측이 존재함.
#강원도의 바이올린차트를 봤을 때, 모양과 분포가 경상남도와 매우 유사함.
#임대료와 임대보증금은  1.자격유형 2.전용면적 과 가장 영향이 있을것이라 추론.
#왜냐하면 상관계수는 결측치 때문에 근거가 될 수 없기 때문.
#자격유형=="K"   * ( 전용면적==16.91 | 전용면적==26.~~ )  
str(train)
colnames(train[10] )  # 임대보증금
colnames(train[11] )  # 임대료 


library(dplyr)
train.happy<-subset(train, 공급유형=="행복주택")
train.happy[train.happy$지역=="강원도",]

a16 <-train.happy[train.happy$지역=="경상남도" & train.happy$자격유형=="K" & train.happy$전용면적>=16 & train.happy$전용면적<=26 ,  ]  %>% summarise(mean(임대료))
#17524
b16 <-train.happy[train.happy$지역=="경상남도" & train.happy$자격유형=="K" & train.happy$전용면적>=16 & train.happy$전용면적<=26 ,  ]  %>% summarise(mean(임대보증금))
#33649800
a26 <-train.happy[train.happy$지역=="경상남도" & train.happy$자격유형=="K" & train.happy$전용면적>=26 & train.happy$전용면적<=36 ,  ]  %>% summarise(mean(임대료))
#26720
b26<-train.happy[train.happy$지역=="경상남도" & train.happy$자격유형=="K" & train.happy$전용면적>=26 & train.happy$전용면적<=36 ,  ]  %>% summarise(mean(임대보증금))
#51309000


train$임대료<-ifelse( train.happy$지역=="강원도" & train.happy$자격유형=="K" & train.happy$전용면적>=16 & train.happy$전용면적<=26  , a16, train$임대료   )
train$임대보증금<-ifelse(  train[2609,]$임대보증금==0 , b16, train$임대보증금   )


train$임대보증금[2613]


train$임대료<-ifelse( train[c(2610:2612),]$임대료==0 , a26, train$임대료   )
train$임대보증금<-ifelse(  train[c(2610:2612),]$임대보증금==0 , b26, train$임대보증금   )





test$임대보증금<-ifelse(  is.na(test$임대보증금), 0, test$임대보증금   )


train[2609,]







#############################
getwd()
setwd("C:/dacon")
train<-read.csv("train0711.csv", sep=",", header=T) 




####공공분양, 공공임대, 잔기전세  / 등록차량수


table(train[,4])

ggplot(train,
       aes(x=train[,5],
           y=등록차량수, fill=train[,5]))+
  geom_violin(scale='width')
####

library(ggplot2)



ggplot(train,
       aes(x=train[,9],
           y=등록차량수, fill=train[,9]))+
  geom_violin(scale='width')


ggplot(train,
       aes(x=train[,5],
           y=등록차량수, fill=train[,5]))+
  geom_violin(scale='width')

ggplot(train,
       aes(x=train[,4],
           y=등록차량수, fill=train[,4]))+
  geom_violin(scale='width')





table(train[,5])

str(train)




?geom_violin

area, count

width
boxplot(train[,14])$stats

mean(train[,14])


 ( (416*2) +(359*3) ) /5

boxplot(train[,14])

##F,M두개밖에없음 

table(train[,9])





##################
install.packages("mice", dependencies=T)
library(mice)

md.pairs(tr.mt)


tr.mt<-as.matrix(train[,c(10:11,14)])

train[,c(10:11,14)]

nrow(train)

as.matrix(1:2, )

getwd()
micepattern<-md.pattern(train[,c(10:11,14)])
write.csv(micepattern,"micepattern.csv", row.names=T)


train$임대료<-ifelse(  train$임대료==0, NA, train$임대료   )
train$임대보증금<-ifelse(  train$임대보증금==0, NA, train$임대보증금   )




m=20
miceresult<-mice( train   , m=m, method = "cart", seed=1, print=FALSE)

str(miceresult)


?mice

densityplot(imp) 
methods(mice)

methods(mice)

?mice



install.packages("VIM")
library(VIM)


miceresult$imp$Account.Balance


head(imp)


stripplot(imp, pch=10, cex=1.2)
####################
apply(train,1,pMiss)

###########################마이스 처음부터 다시
setwd("C:/dacon")
train<-read.csv("train0711_2.csv", sep=",", header=T)

train$임대료<-ifelse(  train$임대료==0, NA, train$임대료 )
train$임대보증금<-ifelse(  train$임대보증금==0, NA, train$임대보증금)


install.packages("mice", dependencies=T)
library(mice)


m=20

result<-mice( train   , m=m, method = "cart", seed=1, print=FALSE)
densityplot(result) 

str(result)

result$imp$Account.Balance


completedData<-complete(result, 1 )


str(completedData)
is.na()

write.csv(completedData, "train.imp.csv", row.names=F)




train<-read.csv("train.imp.csv", sep="," , header=T) 
str(train)
boxplot(train[10])

str( train


#임대보증금(10), 임대료(11)
par(mfcol=c(2,2))
for (j in 10:11) { 
boxplot(train[,j] , main=names(train)[j],  xlab=names(train)[j] )  
hist(train[,j] , main=names(train)[j] , xlab=names(train)[j] )
}


################test imputation#################
test<-read.csv("test0701.csv", sep="," , header=T) 

test$임대료<-ifelse(  test$임대료==0, NA, test$임대료 )
test$임대보증금<-ifelse(  test$임대보증금==0, NA, test$임대보증금)

m=20

result.test<-mice( test   , m=m, method = "cart", seed=1, print=FALSE)
densityplot(result.test) 


completedData.test<-complete(result.test, 1 )

write.csv(completedData.test, "test.imp2.csv", row.names=F)

#########################train 성별,에이지 #######
train.age<-read.csv("trainm0710(1).csv", sep="," , header=T) 
str(train.age)
trainage<-train.age[,c(16:37, 15)] 
trainage0<-train.age[,c(16:27, 15)] 
trainage1<-train.age[,c(28:37, 15)] 

trainage5080<-train.age[,c(26:33, 15)] 

cor(trainage)

install.packages("corrplot")
library(corrplot)
trainage.cor<-cor(trainage) 
trainage0.cor<-cor(trainage0) 
trainage1.cor<-cor(trainage1) 
trainage5080.cor<-cor(trainage5080) 


par(mfrow=c(1,2))
corrplot(trainage.cor, method="number",  type = "lower" ,addCoef.col = "black" ,  tl.col = "black", tl.srt = 45, diag = F) 
corrplot(trainage0.cor, method="number",  type = "lower" ,addCoef.col = "black" ,  tl.col = "black", tl.srt = 45, diag = F) 
corrplot(trainage1.cor, method="number",  type = "lower" ,addCoef.col = "black" ,  tl.col = "black", tl.srt = 45, diag = F)

corrplot(trainage5080.cor, method="number",  type = "lower" ,addCoef.col = "black" ,  tl.col = "black", tl.srt = 45, diag = F)  

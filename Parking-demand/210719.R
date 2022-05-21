##210719
#################################################
install.packages("mice", dependencies=T)
install.packages("corrplot")
library(dplyr);library(mice);library(corrplot)
#################################################
getwd()
setwd("C:/dacon/re")

train<-read.table("train.csv", sep=",", header=T)
str(train)

### fee_deposit , fee_rent 정수로 변경 
train$fee_deposit<-as.numeric(train$fee_deposit)
train$fee_rent<-as.numeric(train$fee_rent)
str(train)

###bus 이상치정리
table(train$bus)
quantile(train$bus, probs=0.99)   #99퀀타일값 15
train$bus<-ifelse(train$bus>=16, 15, train$bus)
table(train$bus)
summary(train)

###fee_deposit, fee_rent, sub NA관련
m=20
result<-mice( train   , m=m, method = "cart", seed=0049, print=FALSE)
densityplot(result) 
str(result)
result$imp$Account.Balance
completedData<-complete(result, 1 )
str(completedData)
sum(is.na(completedData))
sum(is.na(train))
write.csv(completedData, "train2.csv", row.names=F)

###imputation한 train으로 다시 시작
train<-read.csv("train2.csv", sep=",", header=T)
str(train)

###연속형 변수만 correlation
cont<-train[,c(2,6:8,10:15)] 


cont.cor<-cor(cont) 
corrplot(cont.cor, method="number",  type = "lower" ,addCoef.col = "black" ,  tl.col = "black", tl.srt = 45, diag = F) 
	#target num_park 0.86 이외 높은 상관계수 확인 안됨

###범주형 변수 분포 확인
#  $ region     : chr
#  $ type_im    : chr
#  $ type_qual  : chr
#  $ type_as    : chr
table(train$ type_as)  #type_as 변환 후에 type_new다시 만듦(아파트/상가/복합)
table(train$region)    #지역->그대로
table(train$type_im)   #그대로
table(train$type_qual) #1.FM만 합침 2.HI도 합침
	##STEP1. type_qual ====>MO만 합침
boxplot(train$target)$stats
quantile(train$target, probs=0.45) 
quantile(train$target, probs=0.39)   #0.6 퀀타일 차이 -> 합치지 않음(FM)
quantile(train$target, probs=0.18)    #148 220       H
quantile(train$target, probs=0.25)    #0.7퀀타일차이   I  --> 안합침



#$ region     : chr
table(train$region)

train$region<-ifelse(train$region=="강원도",1,(
		ifelse(train$region=="경기도",2, (
		ifelse(train$region=="경상남도",3, (
		ifelse(train$region=="경상북도",4, (
		ifelse(train$region=="광주광역시",5, (
ifelse(train$region=="대구광역시",6,(
ifelse(train$region=="대전광역시",7, (
ifelse(train$region=="부산광역시",8, (
ifelse(train$region=="서울특별시",9, 
(ifelse(train$region=="세종특별자치시",10, (
ifelse(train$region=="울산광역시",11,
ifelse(train$region=="전라남도",12,(
ifelse(train$region=="전라북도",13,
ifelse(train$region=="제주특별자치도",14,
ifelse(train$region=="충청남도",15, 
16   )  )    )))) )  )))))) )

table(train$region)

##  $ type_im    : chr
table(train$type_im)
공공분양 공공임대(10년) 공공임대(50년)  공공임대(5년) 공공임대(분납) 
국민임대 영구임대       임대상가       장기전세       행복주택 
   
train$type_im<-ifelse(train$type_im=="공공분양",1,(
		ifelse(train$type_im=="공공임대(10년)",2, (
		ifelse(train$type_im=="공공임대(50년)",3, (
		ifelse(train$type_im=="공공임대(5년)",4, (
		ifelse(train$type_im=="공공임대(분납)",5, (
ifelse(train$type_im=="국민임대",6,(
ifelse(train$type_im=="영구임대",7, (
ifelse(train$type_im=="임대상가",8, (
ifelse(train$type_im=="장기전세",9, 10) )))))))))
table( train$type_im ) 

## $type_qual    15개->14개로 줄임
class(table(train$type_qual)
  A    B    C    D    E    F    G    H    I    J    K    L    "MO"    N    O 

train$type_qual<-ifelse(train$type_qual=="A",1,(
		ifelse(train$type_qual=="B",2, (
		ifelse(train$type_qual=="C",3, (
		ifelse(train$type_qual=="D",4, (
		ifelse(train$type_qual=="E",5, (
ifelse(train$type_qual=="F",6,(
ifelse(train$type_qual=="G",7, (
ifelse(train$type_qual=="H",8, (
ifelse(train$type_qual=="I",9, 
(ifelse(train$type_qual=="J",10, (
ifelse(train$type_qual=="K",11, (
ifelse(train$type_qual=="L",12,(
ifelse(train$type_qual=="M" &  train$type_qual=="O"  ,13,
14   )    )))) )  )))))) )  )))    )))))))))

table( train$type_qual ) 

## $type_as
table(train$type_as)
train$type_as<-ifelse(train$type_as=="아파트",  0  , 1 )
##############################################################
##########단지 코드별 그룹화 
train<-groupby(by="code")
?groupby
##########
train.group<-train %>% group_by(code)

str(train.group)
str(train)
write.csv(train.group, "train.group2.csv", row.names=F)




 
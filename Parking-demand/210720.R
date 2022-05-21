###210720
################################
library(dplyr); library(mice)

getwd()
setwd("C:/dacon/re")

#################################
#범주형 라벨링한 TRAIN 업로드
train<-read.table("train.label2.csv", sep=',', header=T)
#################################
#단지 코드별 그룹화 

train %>% group_by(code) %>% summarise(tot_area_pop=sum(area_pop))
##################################################################
##test data  (**C2411 /C2253 자격유형 결측)
test<-read.table("test.csv", sep=',', header=T)
summary(test)   
str(test)
#1# fee_deposit , fee_rent 정수로 변경 
test$fee_deposit<-as.numeric(test$fee_deposit)
test$fee_rent<-as.numeric(test$fee_rent)
summary(test)

#2# bus potential outlier
table(test$bus)
quantile(test$bus, probs=0.99)   #99퀀타일값 19
test$bus<-ifelse(test$bus>=20, 19, test$bus)
table(test$bus)
summary(test)

#3#(*test만) type_qual NA처리 위하여 먼저 labeling
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
write.csv(test, "test0720.csv", row.names=F)  #NA포함




#3#fee_deposit, fee_rent, sub ,type_qual NA관련

m=20
test.result<-mice( test   , m=m, method = "sample", seed=0049, print=FALSE)
densityplot(test.result) 
test.completedData<-complete(test.result, 1 )

sum(is.na(test.completedData))
sum(is.na(test.completedData$type_qual))
summary(test.completedData$type_qual)




#############    
str(result)
result$imp$Account.Balance
completedData<-complete(result, 1 )
str(completedData)
sum(is.na(completedData))
sum(is.na(train))


###############5# categorical variables 
#(1)# test$type_as<-ifelse(test$type_as=="아파트",  0  , 1 )
table(test$type_as)
#(2)#
table(test$region)
test$region<-ifelse(test$region=="강원도",1,(
		ifelse(test$region=="경기도",2, (
		ifelse(test$region=="경상남도",3, (
		ifelse(test$region=="경상북도",4, (
		ifelse(test$region=="광주광역시",5, (
ifelse(test$region=="대구광역시",6,(
ifelse(test$region=="대전광역시",7, (
ifelse(test$region=="부산광역시",8, (
ifelse(test$region=="서울특별시",9, 
(ifelse(test$region=="세종특별자치시",10, (
ifelse(test$region=="울산광역시",11,
ifelse(test$region=="전라남도",12,(
ifelse(test$region=="전라북도",13,
ifelse(test$region=="제주특별자치도",14,
ifelse(test$region=="충청남도",15, 
16   )  )    )))) )  )))))) )
 )))))
 )))
 )))
 )
#(3)#$type_im  
table(test$type_im)
test$type_im<-ifelse(test$type_im=="공공분양",1,(
		ifelse(test$type_im=="공공임대(10년)",2, (
		ifelse(test$type_im=="공공임대(50년)",3, (
		ifelse(test$type_im=="공공임대(5년)",4, (
		ifelse(test$type_im=="공공임대(분납)",5, (
ifelse(test$type_im=="국민임대",6,(
ifelse(test$type_im=="영구임대",7, (
ifelse(test$type_im=="임대상가",8, (
ifelse(test$type_im=="장기전세",9, 10) )))))))))
 )))
 )))
)
##########################################################
#####MODELING
setwd("C:/dacon/re")
train<-read.table("train.label2.csv", sep=',', header=T)
train.lm<-lm(target~tot_pop+type_as+region+type_im+area+area_pop+num_empty+type_qual+fee_deposit+fee_rent+sub+bus+num_park, train)

par( mfrow=c(2,2) ) 
plot(train.lm)
#2498, 2497행 potential outlier 추정 (C1363)
train[2498,]
#(1)안없애고, AIC
step.backward = step(train.lm, direction='backward') # AIC=30665.16

step.stepwise = step(train.lm, direction='both')
summary(step.stepwise)
train.lm0<-lm(target ~ tot_pop + region + type_im + num_empty + type_qual + fee_deposit + sub + bus + num_park, train)


summary(train.lm0)
getwd()
setwd("C:/dacon/re")
test<-read.table("test.comp.csv", sep=',', header=T)
predict0720<-predict(train.lm0, newdata=test )
write.csv(predict0720, "predict0720.csv", row.names=F)




######################
##########단지 코드별 그룹화 
train %>% group_by(code) %>% summarise(tot_area_pop=sum(area_pop))

predict<-read.table("predict0720_ing.csv", sep=',', header=T")
predict.Med<-predict%>% group_by(code) %>% summarise(Median=median(target))
write.csv(predict.Med, "predict.Med.csv", row.names=F)
predict.Mean<-predict%>% group_by(code) %>% summarise(maen=mean(target))
write.csv(predict.Mean, "predict.mean.csv", row.names=F)
#mean보다는 Median

#########################################################
##age변수 
age<-read.table("age_gender_info.csv", sep=',', header=T)
table(age$region)
age$region<-ifelse(age$region=="강원도",1,(
		ifelse(age$region=="경기도",2, (
		ifelse(age$region=="경상남도",3, (
		ifelse(age$region=="경상북도",4, (
		ifelse(age$region=="광주광역시",5, (
ifelse(age$region=="대구광역시",6,(
ifelse(age$region=="대전광역시",7, (
ifelse(age$region=="부산광역시",8, (
ifelse(age$region=="서울특별시",9, 
(ifelse(age$region=="세종특별자치시",10, (
ifelse(age$region=="울산광역시",11,
ifelse(age$region=="전라남도",12,(
ifelse(age$region=="전라북도",13,
ifelse(age$region=="제주특별자치도",14,
ifelse(age$region=="충청남도",15, 
16   )  )    )))) )  )))))) )
 )))))
 )))
 )))
 )

##train age left join

train.join<-left_join(train,age,by='region')
write.csv(train.join, "train.join.csv", row.names=F)
test.join<-left_join(test,age,by='region')
write.csv(test.join, "test.join.csv", row.names=F)

str(train.join)
age.cor<-cor(train.join[,c(15:37)])
corrplot(age.cor, method="number",  type = "lower" ,addCoef.col = "black" ,  tl.col = "black", tl.srt = 45, diag = F) 
#상관계수 높은 것들로 다시 새로운 변수 만듦 
#L1040	X10	X20	X30	F50 M50	6080	X90	F100 M100


#train set 
attach(train.join)
train.join$L1040<-L10M+L10F+F40+M40
train.join$X10<-M10+F10
train.join$X20<-M20+F20
train.join$X30<-M30+F30
train.join$F50<-F50
train.join$M50<-M50
train.join$X6080<-M60+F60+M70+F70+M80+F80
train.join$X90<-M90+F90
train.join$F100<-F100
train.join$M100<-M100
write.csv(train.join, "train.join4.csv", row.names=F)

detach(train.join)

#test set
attach(test.join)
test.join$L1040<-L10M+L10F+F40+M40
test.join$X10<-M10+F10
test.join$X20<-M20+F20
test.join$X30<-M30+F30
test.join$F50<-F50
test.join$M50<-M50
test.join$X6080<-M60+F60+M70+F70+M80+F80
test.join$X90<-M90+F90
test.join$F100<-F100
test.join$M100<-M100
write.csv(test.join, "test.join.fi.csv", row.names=F)

#####################################
###MODELING2
train<-read.table("train.join.fi.csv", sep=',', header=T)
train.lm<-lm(target~tot_pop+type_as+region+type_im+area+area_pop+num_empty
+type_qual+fee_deposit+fee_rent+sub+bus+num_park+F50+M50+F100+M100+L1040+X10+X20+X30+X6080+X90, train)
par(mfrow=c(2,2))
plot(train.lm)

##AIC
step.backward = step(train.lm, direction='backward') # AIC=30574.75
step.stepwise = step(train.lm, direction='both')
summary(step.stepwise)

train.lm1<-lm(target ~ tot_pop + region + type_im + num_empty + type_qual
    +fee_deposit + sub + bus + num_park + F50 + M50 + F100 + M100
    +L1040 + X10 + X20 + X30 + X6080 + X90, train)

summary(train.lm1)
par(mfrow=c(2,2))
plot(train.lm1)

##BIC
train.step.forward.BIC = step(lm(target ~ 1, train),
list(upper = ~ tot_pop + region + type_im + num_empty + type_qual
    +fee_deposit + sub + bus + num_park + F50 + M50 + F100 + M100
    +L1040 + X10 + X20 + X30 + X6080 + X90, train), direction='both', k=log(2925))   #BIC값 위보다 큼.




##PREDICT
test<-read.table("test.join.fi.csv", sep=',', header=T)
predict0720.JOIN<-predict(train.lm1, newdata=test )
write.csv(predict0720.JOIN, "predict0720.JOIN.csv", row.names=F)

train %>% group_by(code) %>% summarise(tot_area_pop=sum(area_pop))

predict<-read.table("predict0720.JOIN.csv", sep=',', header=T)

predict.Med<-predict %>% group_by(code) %>% summarise(Median=median(target))


write.csv(predict.Med, "predict.JOIN.Med.csv", row.names=F)


predict.Mean<-predict%>% group_by(code) %>% summarise(maen=mean(target))
write.csv(predict.Mean, "predict.mean.csv", row.names=F)

##################################순서 바껴서 AGGREGATE로 바꿔서 다시 함
predict.agg<-aggregate(target~code,predict, median)
write.csv(predict.agg, "predict.agg.csv", row.names=F)


summary(election.step.forward.BIC)

predict.tap<-with(predict, tapply(target, code, median) ) 

head(predict.tap,5)
head(predict)


####submission 기준으로 병함
sub<-read.table("sample_submission.csv",sep=',', header=T )
pred<-read.table("predict.agg.csv",sep=',', header=T )
pred0720.fi<-left_join(sub,pred,by='code')
write.csv(pred0720.fi, "pred0720.fi.csv", row.names=F)









##210724
###################################################
library(dplyr)
getwd()
train<-read.table("train0722(7).csv", sep=',', header=T)


train<-read.table("train0723(3).1.csv", sep=',', header=T)


train2<-train%>% group_by(code)

train2<-train[!duplicated(train),] 
write.csv(train2, "train0723(5).csv", row.names=F)
##################test 시작
table(test$type_im)

test$type_im1<-ifelse(test$type_im==1,1,0)
test$type_im2<-ifelse(test$type_im==2,1,0)
test$type_im3<-ifelse(test$type_im==3,1,0)
test$type_im4<-ifelse(test$type_im==4,1,0)
test$type_im5<-ifelse(test$type_im==5,1,0)
test$type_im6<-ifelse(test$type_im==6,1,0)
test$type_im7<-ifelse(test$type_im==7,1,0)
test$type_im8<-ifelse(test$type_im==8,1,0)
test$type_im9<-ifelse(test$type_im==9,1,0)
test$type_im10<-ifelse(test$type_im==10,1,0)



test2<-test %>% group_by(code) %>%summarise(prop_type_im1=mean(type_im1) 
                                           , prop_type_im2=mean(type_im2)
                                           , prop_type_im3=mean(type_im3)
                                           , prop_type_im4=mean(type_im4)
                                           , prop_type_im5=mean(type_im5)
                                            , prop_type_im6=mean(type_im6)
                                              , prop_type_im7=mean(type_im7)
                                              , prop_type_im8=mean(type_im8)
                                              , prop_type_im9=mean(type_im9)
                                              , prop_type_im10=mean(type_im10)
                                              )

test<-left_join(test, test2, by='code') 

## area  공급면적 보고 나누기
test<-read.table("test0722.csv", sep=',', header=T)
table(test$area)
test$area<-ifelse(test$area>84.99, 84.99, test$area)
table(test$area)

test$areaL21<-ifelse(test$area<=24.98, 1, 0)
test$areaL29<-ifelse( test$area>24.98   &  test$area<=29.99, 1, 0)
test$areaL36<-ifelse(test$area>29.99& test$area<35.91, 1, 0)
test$areaL39<-ifelse(test$area>=35.91 & test$area<=39.99, 1, 0)
test$areaL46<-ifelse(test$area>39.99 & test$area<=49.99, 1, 0)
test$areaL59<-ifelse(test$area>49.99 & test$area<=59.99, 1, 0)
test$areaU59<-ifelse(test$area>59.99, 1, 0)


test2<-test %>% group_by(code) %>%summarise(prop_areaL21=mean(areaL21) 
                                           , prop_areaL29=mean(areaL29)
                                           , prop_areaL36=mean(areaL36)
                                           , prop_areaL39=mean(areaL39)
                                           , prop_areaL46=mean(areaL46)
                                            , prop_areaL59=mean(areaL59)
                                              , prop_areaU59=mean(areaU59)  )


### area_pop,area_pop  :area그룹별로 더해서 tot_pop과 cor비교 
==============>area_pop 삭제 (cor 높음)
## type_qual 원핫후 prop_type_qual 만들기 
test$type_qual1<-ifelse(test$type_qual==1,1,0)
test$type_qual2<-ifelse(test$type_qual==2,1,0)
test$type_qual3<-ifelse(test$type_qual==3,1,0)
testn$type_qual4<-ifelse(test$type_qual==4,1,0)
test$type_qual5<-ifelse(test$type_qual==5,1,0)
test$type_qual6<-ifelse(test$type_qual==6,1,0)
test$type_qual7<-ifelse(test$type_qual==7,1,0)
test$type_qual8<-ifelse(test$type_qual==8,1,0)
test$type_qual9<-ifelse(test$type_qual==9,1,0)
test$type_qual10<-ifelse(test$type_qual==10,1,0)
test$type_qual11<-ifelse(test$type_qual==11,1,0)
test$type_qual12<-ifelse(test$type_qual==12,1,0)
test$type_qual13<-ifelse(test$type_qual==13,1,0)
test$type_qual14<-ifelse(test$type_qual==14,1,0)

test2<-test %>% group_by(code) %>%summarise(prop_type_qual1=mean(type_qual1) 
                                           , prop_type_qual2=mean(type_qual2)
                                           , prop_type_qual3=mean(type_qual3)
                                           , prop_type_qual4=mean(type_qual4)
                                           , prop_type_qual5=mean(type_qual5)
                                            , prop_type_qual6=mean(type_qual6)
                                              , prop_type_qual7=mean(type_qual7)
                                              , prop_type_qual8=mean(type_qual8)
                                              , prop_type_qual9=mean(type_qual9)
                                              , prop_type_qual10=mean(type_qual10)
                                              , prop_type_qual11=mean(type_qual11)
                                              , prop_type_qual12=mean(type_qual12)
                                              , prop_type_qual13=mean(type_qual13)
                                              , prop_type_qual14=mean(type_qual14)
                                              )

test<-left_join(test, test2, by='code') 

## fee 관련 두개 CODE별 median  // (mean 나중에 함)
test2<-test %>% group_by(code) %>%summarise(M_fee_deposit=median(fee_deposit), M_fee_rent=median(fee_rent))
test<-left_join(test, test2, by='code') 

## CODE 그룹화 duplicated처리
test[!duplicated(train),]


test[!duplicated(train),]

train<-train[!duplicated(train),]
which(table(train$code)!=1)






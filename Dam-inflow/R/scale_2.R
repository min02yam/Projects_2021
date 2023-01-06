library(psych)
getwd()
setwd("C:/bigcon")

data<-read.csv("EDA.csv")
##eda에서 스케일이 다른걸 봤음
par(mfcol=c(2,3))
for (j in 1:3) { 
  boxplot(data[,j] , main=names(data)[j]  )  
  hist(data[,j] ,  main=names(data)[j] )
} 

pairs(data[,1:10])
###############################

set.seed(1)
idx<-sample(1:nrow(data), nrow(data)*0.7, replace=FALSE)
train<-data[idx,]
vali<-scores[-idx,]
train.scale<-as.data.frame(scale(train))
str(train.scale)
summary(train.scale)
#################################
#######################################
#########시나리오1 아웃라이어 보정하지 않음############
##1.상관계수
install.packages("corrplot")
library(corrplot)
cor<-cor(train.scale)
corrplot(cor, method="number",  type = "lower" ,addCoef.col = "black" ,  tl.col = "black", tl.srt = 45, diag = F)


##2. 주성분 분석
install.packages("psych")
 library(psych)
#(1) 성분 추출
pca<-principal(train.scale[,-1], rotate="none")   ;  pca
#(2) 남길 성분 수 결정  -> 3개
plot(pca$values, type="b", ylab="Eigenvalues", xlabs="Component") 
#(3)남은 성분 회전
pca.rotate<-principal(train.scale[,-1], nfactors=3, rotate="varimax" ) ; pca.rotate 
#각 성분 기여도 높은 변수 확인 스크리 플랏에서 3로 정한 이유
#(변화율이 줄어드는 지점, 이 성분을 추가함으로서 분산의 증가에 큰 차이를 보이지 않음), 
#nfacror2,3,4 비교 했을때 4가 ((rmsr값이 가장 작으면서)), 동시에 카이스퀘어 값의 피벨루가 가장 작았음.
#각 성분 기여도 높은 변수 확인
#(3)회전된 결과 해석
#각5개의 성분에 관한 변수의 기여도, 성분1은 - 와 - 변수의 성분에 관한 기여도가 높은 양의값
#(4)요인점수생성: 관찰값의 변수의 값*그 변수의 성분에 관한 기여도: 각 관찰값이 회전된 성분과 관련 있는 정도
scores<-data.frame(pca.rotate$scores)
head(scores)

scores$target<-train.scale$유입량
#head(scores)

##3. 회귀분석
#(1)회귀 적합
colnames(train)
lm<-lm(target~., scores)
summary(lm)    #Adjusted R-squared:  0.6849 
lm2<-lm(target~RC3+RC2, scores)#   RC3+RC2
summary(lm2)   #  Adjusted R-squared:  0.6848 -> 비슷하니까 두개만 쓰겠다
################################## #####################################
par(mfrow=c(2,2))
plot(lm) 
# 1. 분산이 점점 커지고 있음 (독립 변수 값의 크기가 커지면 종속 변수 값의 분산도 커지는 이분산성)
# 2. 정규성 오른쪽 끝 馨煮 대체로 만족하는 것 같음(-3~2는 만족)
# 3.  1.5보다 커서 약간 위험함 로에스핏과 다른 모습을 보임
# 4. cooks 햇밸루, 레버리지 전부 작음
#sol1. 150, 151, 155
scores[which(scores)==150,]
#보수적인 아웃라이어 검정 
library(car)
str(outlierTest(lm) )# 10개정도

outlierTest(lm)$rstudent
keep.subset = (scores != scores[-c(151,150,155,149,156,157,158),])

lm.noOUT = lm(target~., subset=keep.subset, data= scores) 
summary(lm.noOUT)
plot(lm.noOUT)






#(1). 아웃라이어 테스트 후 보수적인 
#(2). 타겟 로그변환


install.packages("gvlma")
library(gvlma)
summary(gvlma(lm2))
summary(gvlma(lm))

scores$sqrt<-sqrt(scores$target)

summary(scores$target)
summary(scores$ln)
names(train)
str(train)
summary(train)

#colnames( train[,-c(5:6)] )
lm2<-lm(sqrt~., train[,-c(5:6)])
par(mfrow=c(2,2))

summary( lm2  )     # adjusted r스퀘어값도 0.794에서  0.9095 로 오름 
plot(lm)
plot(lm2)
#1. 분산 많이 안정됨, 오른쪽 끝으로 갈 수록 값이 많이 없어서 그렇게 보이는 것 같음. 대체로 로에스 값과 일치함
#2. 정규성도 많이 좋아짐, 그러나 가운데 부분 많 비어서 조정이 필요해 보임
#3. 1.5 안넘음
#4. 너무 좋음
#혹시 모르니 검정
#독립성
?durbinWatsonTest
durbinWatsonTest(lm2)
#등분산성
ncvTest(lm2)  #위배
#정규성
par(mfrow=c(1,1))
qqPlot(lm2)  # 아웃라이어가 존재하는 것 같다.  후보:  285 286  202 203 
install.packages("car"); library(car)
vif(lm2)  # 다중공선성큼 --> 릿지나 엘라스틱 적합 필요.
residualPlots(lm2)
#보수적인 아웃라이어 검정 
outlierTest(lm2)  # 10개정도
influence.measures(lm2)  # 훨씬 많음

n = nrow(train)
cutoff = qt(1 - 0.05 / (2*n), (n-5))
train[which(abs(rstudent(lm2)) > cutoff),]
which(abs(rstudent(lm2)) > cutoff)   #284  285  286  287  288  474  475  476 2211 2212 2214 2215 
#201  202  203  204  205  325  326  327 1537 1538 1539 1540

summary(train$sqrt)

par(mfrow=c(1,2))
plot(resid(lm2), rstudent(lm2), pch=23, bg='blue', cex=1)
plot(rstandard(lm2), rstudent(lm2), pch=23, bg='blue', cex=1)
abline(0,1, col='red')  #obs의 아웃라이어 가능성이 작다.

nrow(train)
plot(1:100, abs(rstudent(lm2)))
abline(h=cutoff, col='red', lwd=2)
sum(abs(rstudent(lm(Y~X))) > cutoff ) #CUTOFF보다 높은거 몇개

n=nrow(train[,-c(5,6)])
cutoff = qt(0.99, (n-5))       #0.95->0.975
plot(1:2027, abs(rstudent(   lm(sqrt~., train[,-c(5,6)]))))
abline(h=cutoff, col='red', lwd=2)
sum(abs(rstudent(lm(sqrt~., train[,-5]))) > cutoff)
##############포텐셜 아웃라이어 제거함 
str(new.train)

new.train<-train[-which(abs(rstudent(lm(sqrt~., train[,-c(5,6)]))) > cutoff),]
lm3<-lm(sqrt~., new.train[,-c(5,6)])
summary(lm3)   # 알스퀘어값 또 올라감 0.90  -->  0.9438 
par(mfrow=c(2,2))
par(mfrow=c(1,1))
plot(lm3)
plot(lm2)
outlierTest(lm3)

#cutoff = qt(0.99, (n-5))       
#plot(1: 1943, abs(rstudent(  lm3 )))
#abline(h=cutoff, col='red', lwd=2)
#sum(abs(rstudent(lm3 )) > cutoff)
#qqPlot(lm2)  # 아웃라이어가 존재하는 것 같다.  후보:  285 286  202 203 
#qqPlot(lm3)
#library(car)
#vif(lm2) ; vif(lm3)  -> 줄어듦
#ncvTest(lm2); ncvTest(lm3)

#colnames(train)
#par(mfrow=c(1,2))
#avPlots(lm2, 'RC1')
#avPlots(lm2, 'RC3')

#crPlots(lm2, 'RC1')
#crPlots(lm2, 'RC3')
##################################0.95로 제거함
#n=nrow(train[,-c(5:6)])
#colnames(train)
cutoff2 = qt(0.95, (n-5))       #0.9438 ->0.95
#colnames(train)
#train<-train[,-6]
plot(1:2027, abs(rstudent(   lm(sqrt~., train[,-c(5,6)]))))
abline(h=cutoff2, col='red', lwd=2)
sum(abs(rstudent(lm(sqrt~., train[,-c(5,6)]))) > cutoff2)

new.train2<-train[-which(abs(rstudent(lm(sqrt~., train[,-c(5,6)]))) > cutoff2),]
lm4<-lm(sqrt~., new.train2[,-c(5,6)])
par(mfrow=c(2,2))
plot(lm4)
plot(lm3)
summary(lm4)           #0.9438 ->0.9581
summary(lm3)
 
   
#plot(lm3)
###############################################################진단완료
summary(lm.fi1)   # 0.90
lm.fi2<-lm(sqrt~RC1 + RC3 + RC4,     new.train[,-c(5,6)])
.sub2summary(lm.fi2)     # 0.942
lm.fi3<-lm(sqrt~RC1 + RC3 + RC4,     new.train2[,-c(5,6)])
summary(lm.fi3)      # 0.9582 
lm.fi4<-lm(sqrt~RC1 + RC3 + RC4,     new.train2[,-c(5,6)])
summary(lm.fi4)  #0.958 
################################################################
install.packages("leaps")
library(leaps) #최량
#scores<-scores[,-5]
#colnames(scores)


fit.sub2<-regsubsets(target~.,scores  )
names( summary(fit.sub2) )  
summary(fit.sub2)$cp
plot(fit.sub2, scale="Cp")   # RC1 RC3 RC4 
plot(fit.sub2, scale="bic")  # RC1 RC3 RC4 
which.min( summary(fit.sub2)$bic ) 

colnames(train)
step.forward.BIC = step(lm(sqrt~., new.train2[,-c(5,6)]),
                        list(upper =~RC1+RC3+RC2+RC4), direction='forward',   k=log(nrow(train)))
summary(step.forward.BIC)    # RC1 + RC3 + RC2 + RC4

step.both.BIC = step(lm(sqrt~., new.train2[,-c(5,6)]),
                     list(upper =~RC1+RC3+RC2+RC4), direction='both',   k=log(nrow(train)))
summary(step.both.BIC)  #RC1 + RC3 + RC4   

step.forward.AIC<-step(lm(sqrt~., new.train2[,-c(5,6)]) ,   direction = "forward",  k = 2) # RC1 + RC3 + RC2 + RC4
step.bacward.AIC<-step(lm(sqrt~., new.train2[,-c(5,6)]) ,   direction = "backward",  k = 2)  # RC1 + RC3 + RC4
step.both.AIC<-step(lm(sqrt~., new.train2[,-c(5,6)]) ,   direction = "both",  k = 2) # RC1 + RC3 + RC4

##===> RC1 + RC3 + RC4 변수 셀렉션 완료
lm.fi1<-lm(sqrt~RC1 + RC3 + RC4,    new.train2[,-c(5,6)])
summary(lm.fi1)   #################### Adjusted R-squared:    0.958 변수 하나 빼도 설명럭은 그대로임 

summary(lm.fi1$fitted.values)

summary(  new.train2[,-c(5,6)]$sqrt ) 
new.train2$fitted<-lm.fi1$fitted.values
plot(lm.fi1$fitted.values, new.train2[,-c(5,6)]$sqrt, main="Predicted versus Actual", xlab="Predicted", ylab="Actual")
abline(0,1)
###결과해석: 세가지 주성분이 타겟을 잘 예측한다/ 주성분과 타겟이 강한 선형 관계다. 학습데이터에 선형으로 적합함.


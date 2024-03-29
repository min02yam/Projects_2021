getwd()
setwd("C:/bigcon")

data<-read.csv("EDA.csv")
str(data)
summary(data)

which==3(data$유입량==3)

########정규화##############
normalize <- function(x)   {
  
  return((x - min(x)) / (max(x) - min(x)))         }

# 전체 데이터프레임에 정규화 함수 적용 

data.norm <- as.data.frame(lapply(data, normalize))



###############################

par(mfcol=c(2,3))
for (j in 1:2) { 
  boxplot(data[,j] , main=names(data)[j]  )  
  hist(data[,j] ,  main=names(data)[j] )
} 

#######################################
#########시나리오1 아웃라이어 제거하지 않음############
##1.상관계수
install.packages("corrplot"); library(corrplot)
cor<-cor(data.norm)
corrplot(cor, method="number",  type = "lower" ,addCoef.col = "black" ,  tl.col = "black", tl.srt = 45, diag = F)
##2. 주성분 분석
install.packages("psych")
 library(psych)
#(1) 성분 추출
pca<-principal(data.norm[,-1], rotate="none")   ;  pca
#(2) 남길 성분 수 결정  -> 4개
plot(pca$values, type="b", ylab="Eigenvalues", xlabs="Component") 
#(3)남은 성분 회전
pca.rotate<-principal(data.norm[,-1], nfactors=4, rotate="varimax" ) ; pca.rotate 
#각 성분 기여도 높은 변수 확인 스크리 플랏에서 4로 정한 이유
#(변화율이 줄어드는 지점, 이 성분을 추가함으로서 분산의 증가에 큰 차이를 보이지 않음), 
#nfacror3,4,5 비교 했을때 4가 ((rmsr값이 가장 작으면서)), 동시에 카이스퀘어 값의 피벨루가 가장 작았음.
#각 성분 기여도 높은 변수 확인
#(3)회전된 결과 해석
#각5개의 성분에 관한 변수의 기여도, 성분1은 - 와 - 변수의 성분에 관한 기여도가 높은 양의값
#(4)요인점수생성: 관찰값의 변수의 값*그 변수의 성분에 관한 기여도: 각 관찰값이 회전된 성분과 관련 있는 정도
scores<-data.frame(pca.rotate$scores)
head(scores)

scores$target<-data.norm$유입량
scores$origin.target<-data$유입량

#head(scores$origin.target)

###샘플링#########################################
#test<-read.csv("test.csv")
####train, validation set 생성
install.packages("caret")
library(caret)
set.seed(1)
part<-createDataPartition(scores$target, times=1, p=0.7)
parts<-as.vector(part$Resample1)
train<-scores[parts,]
vali<-scores[-parts,]
###########################################
#(1)회귀 적합
colnames(train)
lm<-lm(target~., train[,-6])
summary(lm)    #Adjusted R-squared:  0.7956

#######################################################################
par(mfrow=c(2,2))
plot(lm) 
# 1. 등분산성이 의심되 2차가 보임 로그변환?
# 2. 정규성 양쪽 끝 뺴고는 만족하는 것 같음(-2~2는 만족)
# 3.  1.5보다 커서 약간 위험함 로에스핏과 다른 모습을 보임
# 4. cooks 햇밸루, 레버리지 전부 작음

#(1). 아웃라이어 테스트 후 보수적인 
#(2). 타겟 로그변환
train$sqrt<-sqrt(train$target)
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
#train.fi<-new.train2[,-c(5,6)]
#colnames(train.fi)
#par("mar")
#par(mar=c(1,1,1,1))
#par(mar=c(1,1,1,1))

fit.sub2<-regsubsets( sqrt~., new.train2[,-c(5,6)]  )
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
###########################################################################
resid<-new.train2[,-c(5,6)]$sqrt -  lm.fi1$fitted.values
train.mse<-mean(resid^2)   #0.0007562889
###########################################################################
#
(1)퀘어
#new.train2$sq<-lm.fi1$fitted.values^2
#summary(new.train2$sq)
#summary(lm.fi1$fitted.values)

#(2)정규화 풀기
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}
#colnames(new.train2)
new.train2.origin<- new.train2[1:nrow(new.train2),"origin.target"]
#head(new.train2.origin)
#head(new.train2[,"origin.target"])
###########다시
new.train2)
new.train2$pred.origin <- unnormalize( lm.fi1$fitted.values ,min(data $ 유입량) , max(data $ 유입량))


#min(data $ 유입량)
#max(data $ 유입량)
#max(data $ 유입량)-min(data $ 유입량)
#new.train2$pred.origin<-new.train2$pred.origin^2
####
#new<-read.csv("lm_train_output4(2).csv")
#res<-new$origin.target-new$pred.origin
#rmse<-sqrt(mean(resid2^2) )  # 1.660225e+15

#head(pred.origin)
#(3)mse확인
resid2<-with(new.train2, origin.target-pred.origin )
train.mse<-mean(resid2^2)   # 1.660225e+15
#(3)아무것도 안하고 확인
resid0<-new.train2$sqrt- lm.fi1$fitted.values
train.mse0<-mean(resid0^2)    
write.csv(new.train2, "lm_train_output4.csv", row.names=F)
#############################################
##검정 데이터 적합
vali$sqrt<-sqrt(vali$target)
pred<-predict(lm.fi1, vali)
plot(pred, vali$sqrt, main="Predicted versus Actual", xlab="Predicted", ylab="Actual")
abline(0,1)    #선형성 보임
vali.resid<-vali$sqrt -  pred
vali.mse<-mean(vali.resid^2)  # 0.001768321 로버스트 하다고 할 수 있음.
#(2)정규화 풀기
#colnames(vali)
vali$pred.origin <- unnormalize(pred , min(vali$origin.target), max(vali$origin.target))
vali$pred.origin<-vali$pred.origin^2
vali.resid<-with(vali, origin.target-pred.origin )
vali.mse<-mean(vali.resid^2)   # mse  값 2.739119e+15  -> 대체로 로버스트하다고 할 수 있음
########################################################################
#릿지#
install.packages("glmnet")
library(glmnet)
colnames(new.train2)
x<-as.matrix(new.train2[,1:4])
y<-as.matrix(new.train2[,7])
ridge<-glmnet(x,y, family="gaussian", alpha=0)
print(ridge)
plot(ridge, label=TRUE)

cv1<-cv.glmnet(x,y, family="gaussian", alpha=0)
cv1$lambda.min
coef(cv1, s="lambda.min")
plot(ridge, xvar="lambda")
plot(cv1)

ridge.fit<-glmnet(x,y, family="gaussian",  alpha=0, lambda=cv1$lambda.min )
coef(ridge.fit)
str(ridge.fit)
str(lm.fi1)
?predict
newx=as.matrix(vali[,1:4])
pred.rid<-predict(ridge.fit,newx, type="response" , s= cv1$lambda.min  )
plot(pred.rid, vali$sqrt)       # 선형으로 잘 적합
abline(0,1)   
#####정규화 풀어서 mse값 비교
#colnames(vali)
vali$pred.origin <- unnormalize(pred.rid , min(vali$origin.target), max(vali$origin.target))
vali$pred.origin<-vali$pred.origin^2
vali.resid<-with(vali, origin.target-pred.origin )
vali.mse<-mean(vali.resid^2)   # mse  2.284857e+15    -> 규제 후 mse값 더 줄었음

########################################################
##elastic net##
library(caret) 
#그리드 만들기 
grid<-expand.grid(.alpha=seq(0, 0.3, by=0.01),  .lambda=seq(0.00, 0.2, by=0.001))
table(grid)
control<-trainControl(method="LOOCV")
enet.train<-train(sqrt~ ., data=new.train2 , method="glmnet", trControl=control, tuneGrid=grid)
head(enet.train)

colnames( new.train2 ) 
######################################
step(lm(ppg~., pca.scores))
```
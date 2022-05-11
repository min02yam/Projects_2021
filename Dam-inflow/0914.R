
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
library(corrplot)
cor<-cor(data.norm)
corrplot(cor, method="number",  type = "lower" ,addCoef.col = "black" ,  tl.col = "black", tl.srt = 45, diag = F)
##2. 주성분 분석
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
###샘플링#########################################
####train, validation set 생성
library(caret)
part<-createDataPartition(scores$target, times=1, p=0.7)
parts<-as.vector(part$Resample1)
train<-scores[parts,]
vali<-scores[-parts,]
###########################################
#(1)회귀 적합


library(leaps) #최량
sub.bic<-regsubsets(lm, method="bic")

summary(lm)   #두개만 유의하게 나옴
par(mfrow=c(2,2))
plot(lm)


#########다른 방법














#2. 모형화와 평가 (1) 성분 추출, 남길 성분 수 결정 (2)남은 성분 회전 (3)회전된 결과 해석 (4) 요인 점수 생성 (5) 요인점수를 입력변수로 회귀 분석-> 테스트 데이터에 평가
#(1).1 성분 추출
pca<-principal(train.scale, rotate="none")   ;  pca
#(1).2 남길 성분 수 결정 - 어떤 성분이 데이터를 가장 많이 설명하는지? 
plot(pca$values, type="b", ylab="Eigenvalues", xlabs="Component")  #스크리 플랏에서 변화율이 줄어드는 지점, 즉 그래프가 평평해지기 시작하는 구간
				#+ 선택한 성분들이 70% 이상의 분산 차지해야함]
#(2)남은 성분 회전
pca.rotate<-principal(train.scale, nfactors=5, rotate="varimax" ) ; pca.rotate 
#각 성분 기여도 높은 변수 확인
#(3)회전된 결과 해석
#각5개의 성분에 관한 변수의 기여도, 성분1은 - 와 - 변수의 성분에 관한 기여도가 높은 양의값
#sslodings: 각 성분의 고유값 --정규화-->Proportion Explained:각 성분이 설명하는 분산의 비율
#cumlative var : -개의 성분은 총 -%의 분산을 나타냄
#(4)요인점수생성: 관찰값의 변수의 값*그 변수의 성분에 관한 기여도: 각 관찰값이 회전된 성분과 관련 있는 정도
pca.scores<-data.frame(pca.rotate$scores)
head(pca.scores)
pca.scores$ppg<-train$ppg
#(5)회귀분석
lm<-lm(ppg~., pca.scores)
summary(lm)   #두개만 유의하게 나옴
par(mfrow=c(2,2))
plot(lm)
lm2<-lm(ppg~RC1+RC2, pca.scores ) 
summary(lm2) # 전과 설명력이 거의 일치함
plot(lm2$fitted.values, train$ppg, main="Predicted versus Actual", xlab="Predicted", ylab="Actual") # 실제값과 예측값 산포도
#결과해석: 두 성분이 타겟을 잘 예측한다/ 주성분과 타겟이 강한 성형 관계다.
pred<-predict()
plot(pred, test$ppg) 
#(6)시각화
train$pred<-round(lm2$fitted.values, 2)
p<-ggplot(train, aes(x=pred, y=ppg, label=Team) ) 
p+geom_point()+geom_text(size=3.5, hjust=0.1, vjust=-0.5, angle=0)+ xlim(0.8,1.4)+ylim(0.8,1.5)+stat_smooth(method="lm", se=FALSE)
#최적선추가 해석: 아래-낮은성취

#쌍렬도(요인점수-팀())
pca.scores$Team<-train$Team
p2<-ggplot(pca.scores, aes(x=RC1, y=RC2, label=Team))
p2+geom_point()+geom_text(size=2.75, hjust=0.2, vjust=-0.75, angle=0)+ xlim(-2.5, 2.5) + ylim(-3.0, 2.5)
#해석: RC1과 음의 기여도 와 양의 기여도 찾아서 해석 

#(7)모형평가 
sqrt(mean(lm2$residual^2))   #오차확인
test.scores<-data.frame( predict(pca.rotate,test[,-c(1:2)]) )
test.scores$pred<-predict(lm2, test.scores)
#시각화
test.scores$ppg<-test$ppg; test.scores$Team<-test$Team
p<-ggplot(test.scores, aes(x=pred, y=ppg, label=Team) ) 
p+geom_point()+geom_text(size=3.5, hjust=0.4, vjust=-0.9, angle=35)+ xlim(0.75,1.5)+ylim(0.5,1.6)+stat_smooth(method="lm", se=FALSE)

resid<-test.scores$ppg-test.scores$pred
sqrt(mean(resid^2)) #해석: 학습 데이터의 오차와 비교 ->타당한지





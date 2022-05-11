
getwd()
setwd("C:/bigcon")

data<-read.csv("EDA.csv")
str(data)
summary(data)

which==3(data$���Է�==3)

########����ȭ##############
normalize <- function(x)   {

return((x - min(x)) / (max(x) - min(x)))         }

# ��ü �����������ӿ� ����ȭ �Լ� ���� 

data.norm <- as.data.frame(lapply(data, normalize))
###############################

par(mfcol=c(2,3))
for (j in 1:2) { 
boxplot(data[,j] , main=names(data)[j]  )  
hist(data[,j] ,  main=names(data)[j] )
} 


#######################################
#########�ó�����1 �ƿ����̾� �������� ����############
##1.������
library(corrplot)
cor<-cor(data.norm)
corrplot(cor, method="number",  type = "lower" ,addCoef.col = "black" ,  tl.col = "black", tl.srt = 45, diag = F)
##2. �ּ��� �м�
library(psych)
#(1) ���� ����
pca<-principal(data.norm[,-1], rotate="none")   ;  pca
#(2) ���� ���� �� ����  -> 4��
plot(pca$values, type="b", ylab="Eigenvalues", xlabs="Component") 
#(3)���� ���� ȸ��
pca.rotate<-principal(data.norm[,-1], nfactors=4, rotate="varimax" ) ; pca.rotate 
#�� ���� �⿩�� ���� ���� Ȯ�� ��ũ�� �ö����� 4�� ���� ����
#(��ȭ���� �پ��� ����, �� ������ �߰������μ� �л��� ������ ū ���̸� ������ ����), 
#nfacror3,4,5 �� ������ 4�� ((rmsr���� ���� �����鼭)), ���ÿ� ī�̽����� ���� �Ǻ��簡 ���� �۾���.
#�� ���� �⿩�� ���� ���� Ȯ��
#(3)ȸ���� ��� �ؼ�
#��5���� ���п� ���� ������ �⿩��, ����1�� - �� - ������ ���п� ���� �⿩���� ���� ���ǰ�
#(4)������������: �������� ������ ��*�� ������ ���п� ���� �⿩��: �� �������� ȸ���� ���а� ���� �ִ� ����
scores<-data.frame(pca.rotate$scores)
head(scores)
scores$target<-data.norm$���Է�
###���ø�#########################################
####train, validation set ����
library(caret)
part<-createDataPartition(scores$target, times=1, p=0.7)
parts<-as.vector(part$Resample1)
train<-scores[parts,]
vali<-scores[-parts,]
###########################################
#(1)ȸ�� ����


library(leaps) #�ַ�
sub.bic<-regsubsets(lm, method="bic")

summary(lm)   #�ΰ��� �����ϰ� ����
par(mfrow=c(2,2))
plot(lm)


#########�ٸ� ���














#2. ����ȭ�� �� (1) ���� ����, ���� ���� �� ���� (2)���� ���� ȸ�� (3)ȸ���� ��� �ؼ� (4) ���� ���� ���� (5) ���������� �Էº����� ȸ�� �м�-> �׽�Ʈ �����Ϳ� ��
#(1).1 ���� ����
pca<-principal(train.scale, rotate="none")   ;  pca
#(1).2 ���� ���� �� ���� - � ������ �����͸� ���� ���� �����ϴ���? 
plot(pca$values, type="b", ylab="Eigenvalues", xlabs="Component")  #��ũ�� �ö����� ��ȭ���� �پ��� ����, �� �׷����� ���������� �����ϴ� ����
				#+ ������ ���е��� 70% �̻��� �л� �����ؾ���]
#(2)���� ���� ȸ��
pca.rotate<-principal(train.scale, nfactors=5, rotate="varimax" ) ; pca.rotate 
#�� ���� �⿩�� ���� ���� Ȯ��
#(3)ȸ���� ��� �ؼ�
#��5���� ���п� ���� ������ �⿩��, ����1�� - �� - ������ ���п� ���� �⿩���� ���� ���ǰ�
#sslodings: �� ������ ������ --����ȭ-->Proportion Explained:�� ������ �����ϴ� �л��� ����
#cumlative var : -���� ������ �� -%�� �л��� ��Ÿ��
#(4)������������: �������� ������ ��*�� ������ ���п� ���� �⿩��: �� �������� ȸ���� ���а� ���� �ִ� ����
pca.scores<-data.frame(pca.rotate$scores)
head(pca.scores)
pca.scores$ppg<-train$ppg
#(5)ȸ�ͺм�
lm<-lm(ppg~., pca.scores)
summary(lm)   #�ΰ��� �����ϰ� ����
par(mfrow=c(2,2))
plot(lm)
lm2<-lm(ppg~RC1+RC2, pca.scores ) 
summary(lm2) # ���� �������� ���� ��ġ��
plot(lm2$fitted.values, train$ppg, main="Predicted versus Actual", xlab="Predicted", ylab="Actual") # �������� ������ ������
#����ؼ�: �� ������ Ÿ���� �� �����Ѵ�/ �ּ��а� Ÿ���� ���� ���� �����.
pred<-predict()
plot(pred, test$ppg) 
#(6)�ð�ȭ
train$pred<-round(lm2$fitted.values, 2)
p<-ggplot(train, aes(x=pred, y=ppg, label=Team) ) 
p+geom_point()+geom_text(size=3.5, hjust=0.1, vjust=-0.5, angle=0)+ xlim(0.8,1.4)+ylim(0.8,1.5)+stat_smooth(method="lm", se=FALSE)
#�������߰� �ؼ�: �Ʒ�-��������

#�ַĵ�(��������-��())
pca.scores$Team<-train$Team
p2<-ggplot(pca.scores, aes(x=RC1, y=RC2, label=Team))
p2+geom_point()+geom_text(size=2.75, hjust=0.2, vjust=-0.75, angle=0)+ xlim(-2.5, 2.5) + ylim(-3.0, 2.5)
#�ؼ�: RC1�� ���� �⿩�� �� ���� �⿩�� ã�Ƽ� �ؼ� 

#(7)������ 
sqrt(mean(lm2$residual^2))   #����Ȯ��
test.scores<-data.frame( predict(pca.rotate,test[,-c(1:2)]) )
test.scores$pred<-predict(lm2, test.scores)
#�ð�ȭ
test.scores$ppg<-test$ppg; test.scores$Team<-test$Team
p<-ggplot(test.scores, aes(x=pred, y=ppg, label=Team) ) 
p+geom_point()+geom_text(size=3.5, hjust=0.4, vjust=-0.9, angle=35)+ xlim(0.75,1.5)+ylim(0.5,1.6)+stat_smooth(method="lm", se=FALSE)

resid<-test.scores$ppg-test.scores$pred
sqrt(mean(resid^2)) #�ؼ�: �н� �������� ������ �� ->Ÿ������




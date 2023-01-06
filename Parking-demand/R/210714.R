##210714
#################################
#경로 설정 및 라이브러리 로드
getwd()
setwd("C:/dacon")
library(corrplot)
#데이터 로드
train<-read.table("train0714(1).csv", sep=',' , header=T)
sum(is.na(train) ) 

train.lm<-lm(target~totpop+region+proplive+sub+bus+numpark+totrent+a15+a20+a30+a40+a50+a60+a70+a80+a100+proprent+numpark_fam+type+pub10+pub50+pub99+gukmi
n+forever+sanga+happy+A+C+D+E+FMO+G+HI+J+K+L+N+L10F+L10M+F10+M10+F20+M20+F30+M30+F40+M40+F50+M50+F60+M60+F70+M70+F80+M80+F90+M90+F100+M100
+fee_deposit+fee_rent, train)


par( mfrow=c(2,2) ) 

plot(train.lm)


train[335,]

train[191,]


train[395,]


nrow(train)
train1<-train[-395,]
nrow(train1)


train1<-train[-c(335, 395, 45, 191,265,289,320,305),]
train1.lm<-lm(target~totpop+region+proplive+sub+bus+numpark+totrent+a15+a20+a30+a40+a50+a60+a70+a80+a100+proprent+numpark_fam+pub10+pub50+pub99+gukmin+forever+sanga+happy+A+C+D+E+FMO+G+HI+J+K+L+N+L10F+L10M+F10+M10+F20+M20+F30+M30+F40+M40+F50+M50+F60+M60+F70+M70+F80+M80+F90+M90+F100+M100+fee_deposit+fee_rent, train1)

par(mfrow=c(2,2))
plot(train1.lm)



summary(train1.lm)




cor.train<-cor(train1[,-1])


cont.cor<-cor(cont) 
corrplot(train1_1.cor, method="number",  type = "lower" ,addCoef.col = "black" ,  tl.col = "black", tl.srt = 45, diag = F) 


str(train1)

train1_1<- train1[,-c(1,29:61)]
train1_1.cor<-cor(train1_1) 

age<-train1[,c(41:60)] 


cor(train1[,c()] )


colnames(train1[,-c(1,29:61)])
     colnames(train1[,c(21,27)])

cor( train1[,c(21,27)] ) 
cor( train1[,c(21,26)] ) 






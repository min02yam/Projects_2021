210713
#############
setwd("C:/dacon")
train<-read.csv("train0712.csv", sep="," , header=T) 
names(train)
str(train)

lm0=lm(차량수~ 단지코드+총세대수+지역+주거율+지하철+버스+단지내주차면수+총임대가구수+a15+a20+a30+a40+a50+a60+a70+a80+a100+임대비율+가구당주차면수+건물구분+공공임대10+공공임대50+공공임대분+국민임대+영구임대+임대상가+행복주택+A+C+D+E+FMO+G+HI+J+K+L+N+L10F+L10M+10F+10M+20F+20M+30F+30M+40F+40M+50F+50M+60F+60M+70F+70M+80F+80M+90F+90M+100F+100M+임대보증금+임대료, train)                      

par(mfrow=c(2,2))
plot(lm0)
summary(lm0)



train<-read.csv("train0712(2).csv", sep="," , header=T) 
lm0=lm(차량수~ 단지코드+총세대수+지역+주거율+지하철+버스+단지내주차면수+총임대가구수+a15+a20+a30+a40+a50+a60+a70+a80+a100+임대비율+가구당주차면수+건물구분+공공임대10+공공임대50+공공임대분+국민임대+영구임대+임대상가+행복주택+A+C+D+E+FMO+G+HI+J+K+L+N+L10F+L10M+10F+10M+20F+20M+30F+30M+40F+40M+50F+50M+60F+60M+70F+70M+80F+80M+90F+90M+100F+100M+임대보증금+임대료, train)                      










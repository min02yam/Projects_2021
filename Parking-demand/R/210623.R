##210623
#################################
install.packages(c("dplyr","ROSE","ggplot2"))
library(dplyr)
library(ROSE)
library(ggplot2)
#help(package="ROSE")
#################################

#데이터 로드
setwd("C:/dacon")
train<-read.csv("train.csv", sep=',', header=T)

#구조 확인
str(train)
search()


rename(train, 지하철역수=train.도보.10분거리.내.지하철역.수.환승노선.수.반영.)


ggplot(train,
       aes   (x=임대건물구분,y=등록차량수, fill=임대건물구분)      )
+
  geom_violin(trim=TRUE,
              scale='count')


with(train, boxplot(등록차량수~임대건물구분))


par(mfrow=c(1,2))
ggplot(train,
       aes(x=임대건물구분,
           y=등록차량수, fill=임대건물구분))+
  geom_violin(scale='width')

ggplot(train,
       aes(x=등록차량수,
           y=임대건물구분, fill=임대건물구분))+
  geom_violin()



ggplot(mpg, aes(x=factor(class),
                y=displ))+
  geom_violin()



ggplot(train,
       aes(x=지역,
           y=임대건물구분, fill=지역))+
  geom_violin(scale='width')





head(table(train[,1]))
df<-  data.frame(    table(train[,1])     )  
o<-order(df$Freq, decreasing=T)
head(df[o,],5)
tail(df[o,],5)


pairs(train[,6],train[,7] )
?pairs

tb<-with(train,xtabs(등록차량수~train[,9]))
as.dataframe(_)





df2<-data.frame(prop.table(tb))  

o<-order(df2$Freq, decreasing=T)
df2[o,]




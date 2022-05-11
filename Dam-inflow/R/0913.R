getwd()
setwd("C:/bigcon")

data<-read.csv("EDA.csv")
str(data)
summary(data)

which==3(data$유입량==3)

########정규화##############
normalize <- function(x)   **{**

return((x - min(x)) / (max(x) - min(x)))         **}**

# 전체 데이터프레임에 정규화 함수 적용 

concrete_norm <- as.data.frame(lapply(concrete, normalize))
###############################

par(mfcol=c(2,3))
for (j in 1:2) { 
boxplot(data[,j] , main=names(data)[j]  )  
hist(data[,j] ,  main=names(data)[j] )
} 
getwd()
setwd("C:/bigcon")

data<-read.csv("EDA.csv")
str(data)
summary(data)

which==3(data$���Է�==3)

########����ȭ##############
normalize <- function(x)   **{**

return((x - min(x)) / (max(x) - min(x)))         **}**

# ��ü �����������ӿ� ����ȭ �Լ� ���� 

concrete_norm <- as.data.frame(lapply(concrete, normalize))
###############################

par(mfcol=c(2,3))
for (j in 1:2) { 
boxplot(data[,j] , main=names(data)[j]  )  
hist(data[,j] ,  main=names(data)[j] )
} 
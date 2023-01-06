##210712
####################
library(caret)
library(corrplot)
####################

findCorrelation(trainage.cor ) 

17 15 13 18 14 16  1  3  7
trainage.cor
as.matrix( train.age[,c(16:37, 15)] )

##»ó°ü°è¼ö°¡ ³ôÀº 

agepca<-prcomp(trainage)  
summary(agepca)
 # pc1°ú pc2»ç¿ë
agepca
agepca$x[,1:2] 
biplot(agepca, main="Biplot")
cor.test(train.age[,16] , train.age[, 15] , method = "pearson")


##########################
fitrain1<-read.csv("fitrain1.csv", sep="," , header=T) 
fitest1<-read.csv("fitest1.csv", sep="," , header=T) 



str(fitrain1)
fitrain1<-lm(








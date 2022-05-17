<다변량회귀스플라인>
set.seed(123)
idx<-sample(1:nrow(data), nrow(data)*0.7, replace=FALSE)
train<-data[idx,]
vali<-data[-idx,]
####스케일#####
train<-scale(train)
scaleList.t <- list(scale = attr(train, "scaled:scale"),center = attr(train, "scaled:center"))
train<-as.data.frame(train)



vali<-scale(vali)
scaleList <- list(scale = attr(vali, "scaled:scale"),center = attr(vali, "scaled:center"))
vali<-as.data.frame(vali)




install.packages("earth")
library(earth)
set.seed(123)
fit.earth<-earth( 유입량~., data=train, pmethod="cv", nfold=5, ncross=3,degree=1, minspan=-1)

summary(fit.earth)
plotmo(fit.earth)
evimp(fit.earth)  #변수중요도

pred<-predict(fit.earth,vali )

prediction <- pred * scaleList$scale["유입량"] + scaleList$center["유입량"]





res<-prediction-vali$유입량
mean(res^2)
prediction<-ifelse(prediction<0,0,prediction)






vali<-scale(vali)
attributes(vali)
scaleList <- list(scale = attr(vali, "scaled:scale"),center = attr(vali, "scaled:center"))
vali<-as.data.frame(vali)

vali.scores<-data.frame( predict(pca.rotate, vali[,-1]) ) 
pred<-predict(lm.cutoff3, vali.scores)


print(pred)
prediction <- pred * scaleList$scale["유입량"] + scaleList$center["유입량"]
summary(prediction)
vali$pred<-prediction
resid<-vali$pred-vali$유입량
mean(resid^2)

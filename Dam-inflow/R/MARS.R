<�ٺ���ȸ�ͽ��ö���>
set.seed(123)
idx<-sample(1:nrow(data), nrow(data)*0.7, replace=FALSE)
train<-data[idx,]
vali<-data[-idx,]
####������#####
train<-scale(train)
scaleList.t <- list(scale = attr(train, "scaled:scale"),center = attr(train, "scaled:center"))
train<-as.data.frame(train)



vali<-scale(vali)
scaleList <- list(scale = attr(vali, "scaled:scale"),center = attr(vali, "scaled:center"))
vali<-as.data.frame(vali)




install.packages("earth")
library(earth)
set.seed(123)
fit.earth<-earth( ���Է�~., data=train, pmethod="cv", nfold=5, ncross=3,degree=1, minspan=-1)

summary(fit.earth)
plotmo(fit.earth)
evimp(fit.earth)  #�����߿䵵

pred<-predict(fit.earth,vali )

prediction <- pred * scaleList$scale["���Է�"] + scaleList$center["���Է�"]





res<-prediction-vali$���Է�
mean(res^2)
prediction<-ifelse(prediction<0,0,prediction)






vali<-scale(vali)
attributes(vali)
scaleList <- list(scale = attr(vali, "scaled:scale"),center = attr(vali, "scaled:center"))
vali<-as.data.frame(vali)

vali.scores<-data.frame( predict(pca.rotate, vali[,-1]) ) 
pred<-predict(lm.cutoff3, vali.scores)


print(pred)
prediction <- pred * scaleList$scale["���Է�"] + scaleList$center["���Է�"]
summary(prediction)
vali$pred<-prediction
resid<-vali$pred-vali$���Է�
mean(resid^2)
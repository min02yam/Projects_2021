##210624

setwd("C:/dacon")
train<-read.csv("train.csv", sep=',', header=T)
str(train)


#################################

table( subset( train[,5], train$임대건물구분=="상가") )
colnames(train[5])
table(train$임대건물구분)

prob.table()
prop.table( xtabs(~train$임대건물구분) )

table(train[,5])
prop.table(table(train[,5]))

prop.table(table(train[,4]))


prop.table(table(train[,4]))


##################################
table(train[,9])
tb<-with(train,xtabs(~train[,9]))
prop.table(tb)
pie(prop.table(tb))




df2<-data.frame(prop.table(tb))  
o<-order(df2$Freq, decreasing=T)
df2[o,]

table(train[,4])





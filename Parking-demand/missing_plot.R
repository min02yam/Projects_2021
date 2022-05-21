getwd()
setwd("C:/Users/alsdu/dacon")
library(ggplot2)


options(encoding="utf-8")
train<-read.csv("train.csv", sep=',', header=T)
str(train)
summary(train)
summary(train$임대료)


train$임대료<- as.numeric(train$임대료)
train$임대보증금<-as.numeric(train$임대보증금)


summary(train$임대보증금)
summary(train$임대료)


head( table(train$임대보증금) )   
head( table(train$임대료) )

train$임대료<- as.numeric(train$임대료)
train$임대보증금<-as.numeric(train$임대보증금)
sum(is.na(train$임대료)); sum(is.na(train$임대보증금))
write.csv(train, "train_N.csv", row.names = FALSE)




train<-read.csv("train_n.csv", sep=',', header=T)# 반영와료 

#######################

plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

plot_Missing(train[,colSums(is.na(train))>0])

plot_Missing(train[,colSums(is.na(train)) > 0, with = FALSE])



train[,colSums(is.na(train))]


train[,colSums(is.na(train))>0]









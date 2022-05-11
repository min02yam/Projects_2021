education.table = read.table('http://stats191.stanford.edu/data/education1975.table', header=T)
education.table$Region = factor(education.table$Region)
education.lm = lm(Y ~ X1 + X2 + X3, data=education.table)
summary(education.lm)
par(mrfow=c(2,2))
plot(education.lm )
boxplot(rstandard(education.lm) ~ education.table$Region, 
        col=c('red', 'green', 'blue', 'yellow'))


keep.subset = (education.table$STATE != 'AK')

education.noAK.lm = lm(Y ~ X1 + X2 + X3, subset=keep.subset, 
                       data=education.table)
summary(education.noAK.lm)
plot(education.noAK.lm)


scores$STATE

education.table[which(education.table$STATE == 'AK'),]

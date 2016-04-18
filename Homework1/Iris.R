#install.packages("randomForest")
library(randomForest)
setwd('~/Desktop/Homework1/')
iris
print(summary(iris))
#table(iris$Species)

X = iris[, 1:4]
Y = iris$Species

rf = list(list(randomForest(x=X, y=Y, ntree=200, do.tract=T, sampsize = 10)),
          list(randomForest(x=X, y=Y, ntree=200, do.tract=T, sampsize = 50)),
          list(randomForest(x=X, y=Y, ntree=200, do.tract=T, sampsize = 100)))
rf1 = randomForest(x=X, y=Y, ntree=200, do.tract=T, sampsize = 10)
rf2 = randomForest(x=X, y=Y, ntree=200, do.tract=T, sampsize = 50)
rf3 = randomForest(x=X, y=Y, ntree=200, do.tract=T, sampsize = 100)

par(mfrow=c(2, 3))
plot(rf1)
legend("topright", colnames(rf1$err.rate), cex=1, text.width = 60, fill=1:4)
plot(rf2)
legend("topright", colnames(rf1$err.rate), cex=1, text.width = 60, fill=1:4)
plot(rf3)
legend("topright", colnames(rf1$err.rate), cex=1, text.width = 60, fill=1:4)

#barplot(t(importance(rf1)), col=4)
#barplot(t(importance(rf2)), col=4)
#barplot(t(importance(rf3)), col=4)
varImpPlot(rf1)
varImpPlot(rf2)
varImpPlot(rf3)



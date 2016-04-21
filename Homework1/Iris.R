#install.packages("randomForest")
library(randomForest)
setwd('~/Github/BiomedicalMachineLearning/Homework1/')

# Get info of Iris, set categories and feature matrix
summary(iris)
X = iris[, 1:4]
Y = iris$Species

# Try different parameter for randomForest
# rf = list(list(randomForest(x=X, y=Y, ntree=200, do.tract=T, sampsize = 10)),
#           list(randomForest(x=X, y=Y, ntree=200, do.tract=T, sampsize = 50)),
#           list(randomForest(x=X, y=Y, ntree=200, do.tract=T, sampsize = 100)))
rf1 = randomForest(x=X, y=Y, ntree=200, sampsize = 10)
rf2 = randomForest(x=X, y=Y, ntree=200, sampsize = 50)
rf3 = randomForest(x=X, y=Y, ntree=200, sampsize = 100)
rf4 = randomForest(x=X, y=Y, ntree=40, sampsize = 100)
rf5 = randomForest(x=X, y=Y, ntree=200, sampsize = 100)
rf6 = randomForest(x=X, y=Y, ntree=1000, sampsize = 100)

# Plot results
png(filename="Iris-simulation.png", width = 960, height = 1920, units = "px", pointsize = 24)

par(mfrow=c(4, 3))

plot(rf1, sub="Tree=200,SampleSize=10", ylim=c(0, 0.3), lwd=2)
legend("topright", colnames(rf1$err.rate), cex=1, inset=0.03, fill=1:4)
plot(rf2, sub="Tree=200,SampleSize=50", ylim=c(0, 0.3), lwd=2)
legend("topright", colnames(rf2$err.rate), cex=1, inset=0.03, fill=1:4)
plot(rf3, sub="Tree=200,SampleSize=100", ylim=c(0, 0.3), lwd=2)
legend("topright", colnames(rf3$err.rate), cex=1, inset=0.03, fill=1:4)
varImpPlot(rf1)
varImpPlot(rf2)
varImpPlot(rf3)

plot(rf4, sub="Tree=40,SampleSize=100", ylim=c(0, 0.2), lwd=2)
legend("topright", colnames(rf4$err.rate), cex=1, inset=0.03, fill=1:4)
plot(rf5, sub="Tree=200,SampleSize=100", ylim=c(0, 0.2), lwd=2)
legend("topright", colnames(rf5$err.rate), cex=1, inset=0.03, fill=1:4)
plot(rf6, sub="Tree=1000,SampleSize=100", ylim=c(0, 0.2), lwd=2)
legend("topright", colnames(rf6$err.rate), cex=1, inset=0.03, fill=1:4)
varImpPlot(rf4)
varImpPlot(rf5)
varImpPlot(rf6)

dev.off()

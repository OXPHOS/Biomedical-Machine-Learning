#install.packages("randomForest")
#install.packages("pROC")
library(randomForest)
library(pROC)
png(filename="Nuclei-simulation.png", width = 1200, height = 960, units = "px", pointsize = 24)
setwd('~/Github/BiomedicalMachineLearning/Homework1/')
spot1 = read.csv("spot1.csv", header = T, sep = ';')
par(mfrow = c(2, 3))
image(matrix(as.numeric(spot1[1, 6:ncol(spot1)]), nrow = 60),
      main = "Visulization of one sample")


#######################################################################
###########          Extract categories and features         ##########
###########        generate preliminary feature matrix       ##########
#######################################################################
Y = spot1[, "Labeler1"]
Y = as.factor(Y)
X = c()
for (i in 1:nrow(spot1)) {
  X = rbind(X, spot1[i, 6:ncol(spot1)])
}


#######################################################################
#######    Explore different tree heights and sample sizes      #######
#######################################################################
repeats = 12
height.list = c(150, 80, 40, 20)
nodenum.list = c(10, 40, 100)
err.rate.summary = c()
for (i in 1:repeats) {
  rf.raw = randomForest(x = X, y = Y, ntree = 400, sampsize = height.list[i %% 4 + 1], 
                        maxnode = nodenum.list[i %% 3 + 1])
  err.rate.summary = rbind(err.rate.summary, colMeans(
                     rf.raw$err.rate[(nrow(rf.raw$err.rate)-51) : (nrow(rf.raw$err.rate)-1), ]))
}
err.rate.summary
### Pick condition 5: sample size = 80, maxNode = default max.


#######################################################################
#######             Repeat randomForest simulation              #######
#######               Enrich significant features               #######
#######################################################################

repeats = 50
importance = array(0, dim = c(ncol(X), 1))

# Run simulation
for (i in 1:repeats) {
  set.seed(i)
  rf.raw = randomForest(x = X, y = Y, ntree = 300, sampsize = 80)
  importance = rf.raw$importance + importance
  plot(rf.raw, ylim = c(0, 1), lwd = 1, main = "randomForest error rate with all features")
  par(new = TRUE)
}
legend("topright", colnames(rf.raw$err.rate), cex = 1, text.font = 10, inset = 0.03, fill = 1:3)
par(new = FALSE)

# Sort and plot ranked feature importance 
importance = t(importance) / repeats
importance.sort = order(importance, decreasing = T)[1:300]
plot(importance[importance.sort], type='h', main="Importance barplot of the \n300 features with highest values")

# Generate trimmed feature matrix
X.trim = X[, importance.sort[1:100]]
rf.trim = randomForest(x = X.trim, y = Y, ntree = 300)
plot(rf.trim, ylim = c(0, 1), lwd = 2, main = "randomForest model error rate with\nfirst 100 features with highest values")
legend("topright", colnames(rf$err.rate), cex = 1, text.font = 10, inset = 0.03, fill = 1:3)

#######################################################################
########     Train randomForest only with quantile features     #######
#######################################################################

# Run randomForest using only 0%, 25%, 75%, 100% quantile features
X.quantile = t(apply(X, 1, quantile, probs = seq(0, 1, 0.25)))
rf.quantile = randomForest(x = X.quantile, y = Y, ntree = 300)
#plot(rf.quantile, ylim = c(0, 1), lwd = 2)
#legend("topright", colnames(rf$err.rate), cex = 1, text.font = 10, inset = 0.03, fill = 1:3)
barplot(t(rf.quantile$importance), main="Importance barplot of quantile features")
X.quantile = cbind(X, X.quantile[, 2:4])


#######################################################################
#######            Include quantile as new features             #######
#######    Explore different tree heights and sample sizes      #######
#######################################################################
repeats = 12
height.list = c(150, 80, 40, 20)
nodenum.list = c(10, 40, 100)
err.rate.summary.quantile = c()
for (i in 1:repeats) {
  rf.raw.quantile = randomForest(x = X.quantile, y = Y, ntree = 400, sampsize = height.list[i %% 4 + 1], 
                    maxnode = nodenum.list[i %% 3 + 1])
  err.rate.summary.quantile = rbind(err.rate.summary.quantile, colMeans(
         rf.raw.quantile$err.rate[(nrow(rf.raw.quantile$err.rate)-51) : (nrow(rf.raw.quantile$err.rate)-1), ]))
}
err.rate.summary.quantile
### Pick condition 5: sample size = 80, maxNode = default max.

#######################################################################
#######             Repeat randomForest simulation              #######
#######               Enrich significant features               #######
#######################################################################

repeats = 50
importance.quantile = array(0, dim=c(ncol(X.quantile), 1))

# Run simulation
for (i in 1:repeats) {
  set.seed(i)
  rf.raw.quantile = randomForest(x=X.quantile, y=Y, ntree=300, sampsize=80)
  importance.quantile = rf.raw.quantile$importance + importance.quantile
  #plot(rf.raw.quantile, ylim=c(0, 1), lwd=1)
  #par(new = TRUE)
}
#legend("topright", colnames(rf.raw.quantile$err.rate), cex=1, text.font=10, inset=0.03, fill=1:3)
#par(new = FALSE)

# Sort and plot ranked feature importance 
importance.quantile = t(importance.quantile) / repeats
importance.sort.quantile = order(importance, decreasing = T)

# Generate trimmed feature matrix with quantiles
X.trim.quantile = X.quantile[, importance.sort.quantile[1:100]]
rf.trim.quantile = randomForest(x = X.trim.quantile, y = Y, ntree = 300)
plot(rf.trim.quantile, ylim = c(0, 1), lwd = 2, 
     main = "randomForest model error rate with\nthe 100 features with highest values")
legend("topright", colnames(rf$err.rate), cex = 1, text.font = 10, inset = 0.03, fill = 1:3)

dev.off()

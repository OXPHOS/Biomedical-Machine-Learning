setwd("~/Desktop/Homework1")
spot1 = read.csv("spot1.csv", header=T, sep=';')
#image(matrix(as.numeric(spot1[1, 6:ncol(spot1)]), nrow=60))

### Extract categories, features and generate preliminary feature matrix
Y = spot1[, "Labeler1"][1:100]
Y = as.factor(Y)
X = c()
for (i in 1:100) { #nrow(spot1)) {
  X = rbind(X, spot1[i, 6:ncol(spot1)])
}

### Repeat randomForest simulation
### Enrich significant features
importance = array(0, dim=c(ncol(spot1) - 5, 1))
for (i in 1:100) {
  rf_raw = randomForest(x=X, y=Y, ntree=200, do.tract=T)
  importance = rf_raw$importance + importance
}
plot(rf_raw)
legend("topright", colnames(rf_raw$err.rate), cex=1, text.width = 40, fill=1:3)
importance = t(importance)
 
importance_sort = order(importance, decreasing=T)
barplot(importance[importance_sort])

# Generate trimmed feature matrix
X_trim = X[, top[1:200]]
rf = randomForest(x=X_trim, y=Y, ntree=200, do.tract=T)
plot(rf)
legend("topright", colnames(rf$err.rate), cex=1, text.width = 40, fill=1:3)

barplot(t(importance(rf)))


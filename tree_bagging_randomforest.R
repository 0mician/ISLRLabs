library(randomForest) # bagging is a special case of random forest (m=p)
library(ISLR)
library(MASS)

attach(Boston)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
boston.test <- Boston[-train,"medv"]

## bagging (13 predictors)
bag.boston <- randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE)
bag.boston

yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2) # improvement over pruned tree

## random forest (default mtry=p/3)
rf.boston <- randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf <- predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2) # improvement over bagging

importance(rf.boston)
png("tree_randomforest.png")
varImpPlot(rf.boston)
dev.off()

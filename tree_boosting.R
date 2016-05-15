library(gbm)
library(ISLR)
library(MASS)

attach(Boston)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
boston.test <- Boston[-train,"medv"]

boost.boston <- gbm(medv~., data=Boston[train,],
                    distribution="gaussian", n.trees=5000, interaction.depth=4)

png("tree_boosting_influence.png")
summary(boost.boston)
dev.off()

png("tree_boosting.png")
par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")
dev.off()

yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2) # similar to rf, and better than bagging

## shrinkage default= 0.0001, let's try 0.2
boost.boston <- gbm(medv~., data=Boston[train,], shrinkage=0.2,
                    distribution="gaussian", n.trees=5000, interaction.depth=4)
summary(boost.boston)
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2) # slightly better test MSE

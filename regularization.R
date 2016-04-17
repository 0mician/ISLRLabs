## Prediction of salary of hitters
##install.packages("ISLR")
library(ISLR)
library(reshape2)
library(ggplot2)
library(glmnet) # regularization (lasso, ridge reg)
options(digits = 2)

## Info about the dataset
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters.omit.na <- na.omit(Hitters)

#########################
### Ridge regression  ###
#########################

# model matrix takes care of creating dummy var, creates matrix with 19 predictors
x <- model.matrix(Salary~., Hitters.omit.na)[,-1]
y <- Hitters.omit.na$Salary

# fit rr (automatic scaling (standardization) of vars with glmnet)
grid <- 10^seq(10,-2,length=100)
ridge.mod <- glmnet(x,y,alpha=0,lambda=grid)

dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
predict(ridge.mod,s=50,type="coefficients")[1:20,]

## selection of model based on validation set
set.seed(1)
train <- sample(1:nrow(x),nrow(x)/2)
test <- (-train)
y.test <- y[test]
ridge.mod <- glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred <- predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred <- predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

#############
### Lasso ###
#############

## same glmnet function, with param alpha=1
lasso.mod <- glmnet(x[train,],y[train],alpha=1,lambda=grid)
png("regularization_lasso.png",1280,800)
plot(lasso.mod)
dev.off()

## Cross-Validation
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
png("regularization_lasso_tmse.png", 1280,800)
plot(cv.out)
dev.off()
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out <- glmnet(x,y,alpha=1,lambda=grid)
lasso.coef <- predict(out, type="coefficients",s=bestlam)[1:20,]
lasso.coef[lasso.coef!=0]

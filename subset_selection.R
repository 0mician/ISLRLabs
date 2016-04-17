## Prediction of salary of hitters
##install.packages("ISLR")
library(ISLR)
library(reshape2)
library(ggplot2)
library(leaps) # for best subset selection
options(digits = 2)

## Info about the dataset
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
  
##############################
### Best subset selection  ###
##############################

Hitters.omit.na <- na.omit(Hitters)
regfit.full <- regsubsets(Salary~., Hitters)
summary(regfit.full) # by default it will only report the 8 best models (rather than 19)

regfit.full <- regsubsets(Salary~., Hitters, nvmax=19)
reg.summary <- summary(regfit.full)

names(reg.summary) # summary function returns many params (R^2, BIC, etc)
reg.summary$rsq # inspect R^2 values, increases monotonically here

# plotting results
png('subset_best_selection.png',width=1280,height=800)
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="p", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="p", ylab="adj. rsquared", type="l")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20)
plot(reg.summary$cp, xlab="p", ylab="Cp", type="l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col="red", cex=2, pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab="p", ylab="BIC", type="l")
points(6, reg.summary$bic[6], col="red", cex=2, pch=20)
dev.off()

# or use built-in function in leaps
par(mfrow=c(1,1))
plot(regfit.full, scale="r2")
plot(regfit.full, scale="bic")

# coef estimates for a given model
coef(regfit.full, 19)

##################################
### Stepwise subset selection  ###
##################################

# using the same function (regsubsets), but with an arg for the method:
regfit.fwd <- regsubsets(Salary~., data=Hitters.omit.na, nvmax=19, method="forward")
summary(regfit.fwd)

regfit.bw <- regsubsets(Salary~., data=Hitters.omit.na, nvmax=19, method="backward")
summary(regfit.bw)

# results will be a bit different of course
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bw, 7)

############################################
### Choice of best model using VS and CV ###
############################################

set.seed(1)
train <- sample(c(TRUE,FALSE), nrow(Hitters.omit.na), rep=TRUE)
test <- (!train)

# have to rebuild model not including test example
regfit.best <- regsubsets(Salary~., data=Hitters.omit.na[train,],nvmax=19)
test.mat <- model.matrix(Salary~., data=Hitters.omit.na[test,])

# no predict function in regsubsets()
val.errors <- rep(NA, 19)
for(i in 1:19){
    coefi <- coef(regfit.best, id=i)
    pred <- test.mat[,names(coefi)] %*% coefi
    val.errors[i] <- mean((Hitters.omit.na$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best, 10)

# let's capture that in a predict function
predict.regsubsets <- function(object, newdata, id, ...){
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id=id)
    xvars <- names(coefi)
    mat[,xvars]%*%coefi
}

regfit.best <- regsubsets(Salary~., data=Hitters.omit.na, nvmax=19)
coef(regfit.best, 10)

# CV per se, have to train only in training set (hence a bit involved)
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters.omit.na), replace=TRUE)
cv.errors <- matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))

for(j in 1:k){
    best.fit <- regsubsets(Salary~.,data=Hitters.omit.na[folds!=j,], nvmax=19)
    for(i in 1:19){
        pred <- predict.regsubsets(best.fit,Hitters.omit.na[folds==j,],id=i)
        cv.errors[j,i] <- mean((Hitters.omit.na$Salary[folds==j]-pred)^2)
    }
}

mean.cv.errors <- apply(cv.errors,2,mean)
mean.cv.errors

# plot results
png('subset_best_selection_cv.png',width=1280,height=800)
par(mfrow=c(1,1))
plot(mean.cv.errors, type="b")
dev.off()
## minimum located at 11
reg.best <- regsubsets(Salary~.,data=Hitters.omit.na, nvmax = 19)
coef(reg.best, 11)

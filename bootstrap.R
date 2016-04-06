#install.packages("ISLR")
library(ISLR)
library(reshape2)
library(ggplot2)
library(boot) #bootstrap

# note: 2-steps process
# 1: creation of function that computes the statistic of interest
# 2: use boot() to perform sampling with replacement of data

set.seed(1)

############################################
# Estimation of accuracy of a statistics ###
############################################

alpha.fn <- function(data, index){
    X <- data$X[index]
    Y <- data$Y[index]
    return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)

alpha.fn(Portfolio, sample(100,100,replace=TRUE))
boot(Portfolio, alpha.fn, R=1000)

####################################
# Estimation of accuracy of a lm ###
####################################

# polynomial degree 1
boot.fn <- function(data, index){
    return(coef(lm(mpg~horsepower,data=data,subset=index)))
}

boot.fn(Auto, 1:392)

boot.fn(Auto, sample(392,392,replace=TRUE))
boot.fn(Auto, sample(392,392,replace=TRUE)) # different results

# compare both accuracy estimation techniques
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower, data=Auto))$coef

# polynomial degree 2
boot.fn2 <- function(data, index){
    return(coefficients(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index)))
}

boot(Auto, boot.fn2, 1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef

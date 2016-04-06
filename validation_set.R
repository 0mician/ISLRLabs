#install.packages("ISLR")
library(ISLR)
library(reshape2)
library(ggplot2)

###############################
### Validation set approach ###
###############################

# first validation set & error test set estimation
##################################################

set.seed(1)
train <- sample(392, 196) # split the set of observations in half

# fitting 3 models with training set
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)
with(Auto, mean((mpg-predict(lm.fit, Auto))[-train]^2))
lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto, subset=train)
with(Auto,mean((mpg-predict(lm.fit2, Auto))[-train]^2))
lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto, subset=train)
with(Auto, mean((mpg-predict(lm.fit3, Auto))[-train]^2))

# Second validation set (different split) & error test set estimation
#####################################################################

set.seed(2) # different random seed will result in different split
train2 <- sample(397, 196)

# fitting 3 models
lm.fit <- lm(mpg~horsepower,data=Auto, subset=train)
with(Auto, mean((mpg-predict(lm.fit,Auto))[-train2]^2))
lm.fit <- lm(mpg~poly(horsepower,2),data=Auto, subset=train)
with(Auto, mean((mpg-predict(lm.fit,Auto))[-train2]^2))
lm.fit <- lm(mpg~horsepower,data=Auto, subset=train)
with(Auto, mean((mpg-predict(lm.fit,Auto))[-train2]^2))

# Multiple validation sets for fun & plots
##########################################

vs.mse <- data.frame(matrix(0, 10, 15))
colnames(vs.mse) <- sprintf("Split%d", 1:15)

attach(Auto)
for (i in 1:15) {
    set.seed(i)
    train <- sample(392,196)
    split <- c(rep(0,10))
    for (j in 1:10){
        lm.fit <- lm(mpg~poly(horsepower, j), data=Auto, subset=train)
        split[j] <- mean((mpg-predict(lm.fit, Auto))[-train]^2)
    }
    vs.mse[,i] <- split
}

vs.mse.melt <- melt(vs.mse)
vs.mse.melt$PolyDegree <- as.factor(c(rep(seq(1,10,1),15)))
colnames(vs.mse.melt) <- c("Split", "TestMSE", "PolynomialDegree")
ggplot(vs.mse.melt, aes(x=PolynomialDegree, y=TestMSE, group=Split)) +
    geom_line(aes(colour=Split)) + ylim(10,35) +
    xlab("Flexibility") + ylab("MSE") +
    ggtitle("Test MSE for different validation set")



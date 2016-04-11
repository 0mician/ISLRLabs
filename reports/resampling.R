#############################
# Solution for question 5 ###
#############################
library(ISLR)
library(boot)
library(ggplot2)

## a
set.seed(1)
attach(Default)
glm.fit.default <- glm(default~income+balance, data=Default, family = binomial)
glm.probs <- predict(glm.fit.default, Default, type="response")
contrasts(default)
glm.pred <- rep("No", 10000)
glm.pred[glm.probs>.5]="Yes"

table(glm.pred, default)

## b
train <- sample(10000, 5000)
glm.fit.vs <- glm(default~income+balance, data=Default, subset=train, family=binomial)
glm.probs.vs <- predict(glm.fit.vs, Default, type="response")[-train]
contrasts(default)
glm.pred.vs <- rep("No", 10000)
glm.pred.vs[glm.probs.vs>.5]="Yes"

table(glm.pred.vs, default)
mean(glm.pred != Default[-train, ]$default)

## c
for (i in 1:3) {
    set.seed(i)
    train <- sample(10000, 5000)
    glm.fit.vs <- glm(default~income+balance, data=Default, subset=train, family=binomial)
    glm.probs.vs <- predict(glm.fit.vs, Default, type="response")[-train]
    glm.pred.vs <- rep("No", 10000)
    glm.pred.vs[glm.probs.vs>.5]="Yes"
    print(mean(glm.pred != Default[-train, ]$default))
}

## d
for (i in 1:3) {
    set.seed(i)
    train <- sample(10000, 5000)
    glm.fit.vs <- glm(default~income+balance+student, data=Default, subset=train, family=binomial)
    glm.probs.vs <- predict(glm.fit.vs, Default, type="response")[-train]
    glm.pred.vs <- rep("No", 10000)
    glm.pred.vs[glm.probs.vs>.5]="Yes"
    print(mean(glm.pred != Default[-train, ]$default))
}

#############################
# Solution for question 6 ###
#############################

## a
set.seed(1)
glm.fit <- glm(default~income+balance, data=Default, family=binomial)
summary(glm.fit)

## b
boot.fn <- function(data, index){
    model <- glm(default~income+balance, data=data, family=binomial, subset=index)
    return(coef(model))
}

## c
boot(Default, boot.fn, 50)

#############################
# Solution for question 8 ###
#############################

## a
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

## b
qplot(x, y)
ggsave("qplot.pdf")

## c
quadratic.dataset <- data.frame(x, y)
for (i in 1:4) {
    glm.fit <- glm(y~poly(x,i))
    print(cv.glm(quadratic.dataset, glm.fit)$delta)
}

## d
set.seed(2)
for (i in 1:4) {
    glm.fit <- glm(y~poly(x,i))
    print(cv.glm(quadratic.dataset, glm.fit)$delta)
}

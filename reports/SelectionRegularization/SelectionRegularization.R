##################
### Exercise 8 ###
##################

library(ggplot2)
library(gridExtra)
library(leaps)
library(glmnet)
options(digits = 2)

## part a: simulated dataset creation
set.seed(1)
x <- rnorm(100)
noise <- rnorm(100)
x_true <- seq(-3.0,3.0,0.01)

## part b: response vector with given model y = 1x^3 - 2x^2 + 3x + 5
y <- 2*x^3 + 0.5*x^2 + 3*x + 5 + noise
f <- 2*x_true^3 + 0.5*x_true^2 + 3*x_true+ 5

p1 <- qplot(x,y)
p2 <- qplot(x_true,f, geom="line")
grid.arrange(p1, p2, ncol=2)
ggsave("fun.pdf",  arrangeGrob(p1, p2, ncol = 2), width = 16, height = 8, units = "cm")

## part c: perform best subset selection
dataset <- data.frame(x, y)
regfit.full <- regsubsets(y ~ poly(x, 10, raw = TRUE), data = dataset, nvmax = 10)
regfit.summary <- summary(regfit.full)
which.min(regfit.summary$cp)
which.min(regfit.summary$bic)
which.max(regfit.summary$adjr2)

# plotting results
cp <- qplot(1:10, regfit.summary$cp, geom="line") +
    xlab("Polynomial degree") + ylab("Cp") + scale_x_continuous(breaks=seq(1,10,1)) +
    geom_point(aes(x=4,y=regfit.summary$cp[4]),colour=I("sienna1"),size=3)

adjr2 <- qplot(1:10, regfit.summary$adjr2, geom="line") +
    xlab("Polynomial degree") + ylab("Adj-Rsquared") + scale_x_continuous(breaks=seq(1,10,1)) +
    geom_point(aes(x=4,y=regfit.summary$adjr2[4]),colour=I("sienna1"),size=3)

bic <- qplot(1:10, regfit.summary$bic, geom="line") +
    xlab("Polynomial degree") + ylab("BIC") + scale_x_continuous(breaks=seq(1,10,1)) +
    geom_point(aes(x=3,y=regfit.summary$bic[3]),colour=I("sienna1"),size=3)

grid.arrange(cp, adjr2, bic, ncol=2, nrow=2)
ggsave("BIC-AR2-Cp.pdf", arrangeGrob(cp, adjr2, bic, ncol = 2, nrow=2),
       width = 16, height = 16, units = "cm")

coefficients(regfit.full, id = 3)
coefficients(regfit.full, id = 4)

## part d:
## Forward selection
regfit.fwd <- regsubsets(y ~ poly(x, 10, raw = TRUE), data = dataset, nvmax = 10, method = "forward")
fwd.summary <-  summary(regfit.fwd)
which.min(fwd.summary$cp)
which.max(fwd.summary$adjr2)
which.min(fwd.summary$bic)

cp <- qplot(1:10, fwd.summary$cp, geom="line") +
    xlab("Polynomial degree") + ylab("Cp") + scale_x_continuous(breaks=seq(1,10,1)) +
    geom_point(aes(x=3,y=fwd.summary$cp[3]),colour=I("sienna1"),size=3)

adjr2 <- qplot(1:10, fwd.summary$adjr2, geom="line") +
    xlab("Polynomial degree") + ylab("Adj-Rsquared") + scale_x_continuous(breaks=seq(1,10,1)) +
    geom_point(aes(x=4,y=fwd.summary$adjr2[4]),colour=I("sienna1"),size=3)

bic <- qplot(1:10, fwd.summary$bic, geom="line") +
    xlab("Polynomial degree") + ylab("BIC") + scale_x_continuous(breaks=seq(1,10,1)) +
    geom_point(aes(x=3,y=fwd.summary$bic[3]),colour=I("sienna1"),size=3)

grid.arrange(cp, adjr2, bic, ncol=2, nrow=2)
ggsave("Fwd-BIC-AR2-Cp.pdf", arrangeGrob(cp, adjr2, bic, ncol = 2, nrow=2))

## Backward selection
regfit.bwd <- regsubsets(y ~ poly(x, 10, raw = TRUE), data = dataset, nvmax = 10, method = "backward")
bwd.summary <-  summary(regfit.bwd)

which.min(bwd.summary$cp)
which.max(bwd.summary$adjr2)
which.min(bwd.summary$bic)

cp <- qplot(1:10, bwd.summary$cp, geom="line") +
    xlab("Polynomial degree") + ylab("Cp") + scale_x_continuous(breaks=seq(1,10,1)) +
    geom_point(aes(x=3,y=bwd.summary$cp[3]),colour=I("sienna1"),size=3)

adjr2 <- qplot(1:10, bwd.summary$adjr2, geom="line") +
    xlab("Polynomial degree") + ylab("Adj-Rsquared") + scale_x_continuous(breaks=seq(1,10,1)) +
    geom_point(aes(x=5,y=bwd.summary$adjr2[5]),colour=I("sienna1"),size=3)

bic <- qplot(1:10, bwd.summary$bic, geom="line") +
    xlab("Polynomial degree") + ylab("BIC") + scale_x_continuous(breaks=seq(1,10,1)) +
    geom_point(aes(x=3,y=bwd.summary$bic[3]),colour=I("sienna1"),size=3)

grid.arrange(cp, adjr2, bic, ncol=2, nrow=2)
ggsave("Bwd-BIC-AR2-Cp.pdf", arrangeGrob(cp, adjr2, bic, ncol = 2, nrow=2))

## part e:
xmat <- model.matrix(y ~ poly(x, 10, raw = T), data = dataset)[, -1]
lasso.mod <- cv.glmnet(xmat, y, alpha = 1)
best.lambda <- lasso.mod$lambda.min

pdf("lasso-plot.pdf", width=8, height=8)
plot(lasso.mod)
dev.off()

# Next fit the model on entire data using best lambda
best.model <- glmnet(xmat, y, alpha = 1)
predict(best.model, s = best.lambda, type = "coefficients")

## part f:
y <- 5 + 2*x^7 + noise
dataset <- data.frame(x, y)

## Best subset
regfit.full <- regsubsets(y ~ poly(x, 10, raw = TRUE), data = dataset, nvmax = 10)
regfit.summary <- summary(regfit.full)
which.min(regfit.summary$cp)
which.min(regfit.summary$bic)
which.max(regfit.summary$adjr2)

# plotting results best subset
cp <- qplot(1:10, regfit.summary$cp, geom="line") +
    xlab("Polynomial degree") + ylab("Cp") + scale_x_continuous(breaks=seq(1,10,1)) +
    geom_point(aes(x=2,y=regfit.summary$cp[2]),colour=I("sienna1"),size=3)

adjr2 <- qplot(1:10, regfit.summary$adjr2, geom="line") +
    xlab("Polynomial degree") + ylab("Adj-Rsquared") + scale_x_continuous(breaks=seq(1,10,1)) +
    geom_point(aes(x=4,y=regfit.summary$adjr2[4]),colour=I("sienna1"),size=3)

bic <- qplot(1:10, regfit.summary$bic, geom="line") +
    xlab("Polynomial degree") + ylab("BIC") + scale_x_continuous(breaks=seq(1,10,1)) +
    geom_point(aes(x=1,y=regfit.summary$bic[1]),colour=I("sienna1"),size=3)

grid.arrange(cp, adjr2, bic, ncol=2, nrow=2)
ggsave("BIC-AR2-Cp-f.pdf", arrangeGrob(cp, adjr2, bic, ncol = 2, nrow=2),
       width = 16, height = 16, units = "cm")

## Lasso
xmat <- model.matrix(y ~ poly(x, 10, raw = T), data = dataset)[, -1]
lasso.mod <- cv.glmnet(xmat, y, alpha = 1)
best.lambda <- lasso.mod$lambda.min

best.model <- glmnet(xmat, y, alpha = 1)
predict(best.model, s = best.lambda, type = "coefficients")

pdf("lasso-plot-f.pdf", width=8, height=8)
plot(lasso.mod)
dev.off()

###################
### Exercise 10 ###
###################

## part a
set.seed(1)
x <- matrix(rnorm(20000), 1000, 20)
x[,c(2,5,6,10,19)] <- 0
y <- x %*% rnorm(20)

## part b
train <- sample(seq(1,1000,1), 100, replace = FALSE)

## part c

## part d

## part e

## part f

## part g

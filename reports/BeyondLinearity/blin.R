library(ggplot2)
library(splines)
library(MASS)
# library(boost)

attach(Boston)

##########
# part a #
##########

lm.fit = lm(nox ~ poly(dis, 3), data = Boston)
summary(lm.fit)

# creation of a grid of values for prediction
dislim = range(dis)
dis.grid = seq(from = dislim[1], to = dislim[2], by = 0.1)

# prediction & plotting
lm.pred = predict(lm.fit, list(dis = dis.grid))
pdf("parta.pdf")
plot(nox ~ dis, data = Boston, col = "darkgrey",main="Boston nox prediction using dis") 
lines(dis.grid, lm.pred, col = "red", lwd = 2)
dev.off()

##########
# part b #
##########

all.rss = rep(NA, 10)
for (i in 1:10) {
    lm.fit = lm(nox ~ poly(dis, i), data = Boston)
    all.rss[i] = sum(lm.fit$residuals^2)
}

##########
# part c #
##########

dim(Boston)
all.deltas <- rep(NA, 10)

## training using validation set (different seeds)
for (i in 1:10) {
    set.seed(i)
    train <- sample(506,334)
    lm.fit <- lm(nox~poly(dis, i,raw=TRUE), data = Boston, subset=train)
    all.deltas[i] <- mean((nox-predict(lm.fit,Boston))[-train]^2)
}
pdf("partc.pdf")
plot(1:10, all.deltas, xlab = "Polynomial degree", ylab = "Validation set error",
     main="Selection of degree using validation set", type = "l", pch = 20, lwd = 2)
dev.off()

##########
# part d #
##########

# range of values for dis, and selection of knots
range(Boston$dis)
k <- c(4,8,11)

# fitting model and plotting results
sp.fit <- lm(nox~bs(dis, df=4, knots=k), data=Boston)
summary(sp.fit)

sp.pred = predict(sp.fit, list(dis = dis.grid))
pdf("partd.pdf")
plot(nox ~ dis, data = Boston, col = "darkgrey", main="Regression spline fit")
lines(dis.grid, sp.pred, col = "red", lwd = 2)
dev.off()

##########
# part e #
##########

all.residuals = rep(NA, 16)

for (i in 3:16) {
    lm.fit = lm(nox ~ bs(dis, df = i), data = Boston)
    all.residuals[i] = sum(lm.fit$residuals^2)
}
all.residuals[-c(1, 2)]

##########
# part f #
##########

all.cv = rep(NA, 16)
set.seed(1)
train <- sample(506,334)
for (i in 3:16) {
    lm.fit = lm(nox ~ bs(dis, df = i), data = Boston,subset=train)
    all.cv[i] <- mean((nox-predict(lm.fit,Boston))[-train]^2)
}

pdf("plotf.pdf")
plot(3:16, all.cv[-c(1, 2)], lwd = 2, type = "l", xlab = "df", ylab = "Validation set error", main="Selection of degrees of freedom")
dev.off()

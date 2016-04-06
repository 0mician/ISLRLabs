#install.packages("ISLR")
library(ISLR)
library(reshape2)
library(ggplot2)
library(boot) #cv.glm()

########################
### Cross validation ###
########################

# Leave one out CV (LOOCV)
##########################

# Note: using glm here to create lm (no family specified), gives access to cv.glm()
# Note: for lm, could use analytic approach to compute Test MSE (but cv.glm
# doesn't use it)

glm.fit <- glm(mpg~horsepower, data=Auto)
coef(glm.fit) # identical to a lm.fit

cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta # MSE interval

cv.error <- rep(0,5)
for (i in 1:5){
    glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
    cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

cv.error

# K-fold CV
###########

# Note: here we use k=10, a common value for k-fold CV

set.seed(1)
cv.error.10 <- rep(0,10)
for (i in 1:10){
    glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
    cv.error.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}

cv.error.10

# Multiple CV + plots to explore variance in approx of Test MSE
###############################################################

kcvv.mse <- data.frame(matrix(0, 10, 15))
colnames(kcvv.mse) <- sprintf("kcvv%d", 1:15)

attach(Auto)

for (i in 1:15) {
    set.seed(i)
    cv.error.10 <- rep(0,10)
    for (j in 1:10){
        glm.fit <- glm(mpg~poly(horsepower,j), data=Auto)
        cv.error.10[j] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
    }
    kcvv.mse[,i] <- cv.error.10
}

kcvv.mse.melt <- melt(kcvv.mse)
kcvv.mse.melt$PolyDegree <- as.factor(c(rep(seq(1,10,1),15)))
colnames(kcvv.mse.melt) <- c("KCVVRun", "TestMSE", "PolynomialDegree")
ggplot(kcvv.mse.melt, aes(x=PolynomialDegree, y=TestMSE, group=KCVVRun)) +
    geom_line(aes(colour=KCVVRun)) + ylim(10,35) +
    xlab("Flexibility (polynomial degree)") + ylab("Test MSE") +
    ggtitle("Test MSE for different 10-fold CV runs")
ggsave("cross_validation.png")

library(ISLR)
library(ggplot2)
attach(Wage)

# first approach, fit a global function of degree 4
fit <- lm(wage~poly(age,4, raw=TRUE), data=Wage) # or wage~age+I(age^2)+..
coef(summary(fit)) # all coefficients are significant (t)
qplot(age, wage)

# creation of grid of values of age for which we want to predict Wage
agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit, newdata=list(age=age.grid),se=TRUE)
se.bands <- cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

# plot data and fit (poly degree 4)
png("step_functions_poly.png",width = 1920, height = 1080, units = "px")
par(mfrow = c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Degree-4 polynomial", outer=TRUE)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# how to decide the degree of the polynomial to fit?
fit.1 <- lm(wage~age,data=Wage)
fit.2 <- lm(wage~poly(age,2), data=Wage)
fit.3 <- lm(wage~poly(age,3), data=Wage)
fit.4 <- lm(wage~poly(age,4), data=Wage)
fit.5 <- lm(wage~poly(age,5), data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

## Analysis of Variance Table

## Model 1: wage ~ age
## Model 2: wage ~ poly(age, 2)
## Model 3: wage ~ poly(age, 3)
## Model 4: wage ~ poly(age, 4)
## Model 5: wage ~ poly(age, 5)
##   Res.Df     RSS Df Sum of Sq        F    Pr(>F)    
## 1   2998 5022216                                    
## 2   2997 4793430  1    228786 143.5931 < 2.2e-16 ***
## 3   2996 4777674  1     15756   9.8888  0.001679 ** 
## 4   2995 4771604  1      6070   3.8098  0.051046 .  
## 5   2994 4770322  1      1283   0.8050  0.369682    
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# we see that up to degree 3, introduction of more flexibility
# improves our model

coef(summary(fit.5)) # same thing (p-values)

# predicting if ind. earns more than 250K
fit <- glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
preds <- predict(fit, newdata=list(age=age.grid),se=TRUE)
# calculation of CI
pfit <- exp(preds$fit)/(1+exp(preds$fit))
xse.bands.logit <- cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))
preds <- predict(fit,newdata=list(age=age.grid),type="response",se=TRUE)

# plotting results
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age),I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
dev.off()

# fitting step function (using cut)
table(cut(age,4))
fit <- lm(wage~cut(age,4),data=Wage)
coef(summary(fit))
preds <- predict(fit, newdata=list(age=age.grid),se=TRUE)
se.bands <- cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

# plot data and fit (poly degree 4)
png("step_functions_cut.png",width = 1920, height = 1080, units = "px")
par(mfrow = c(1,1), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("step function", outer=TRUE)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
dev.off()

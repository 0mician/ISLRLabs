library(splines)
attach(Wage)

# regression splines (use of bs to generate matrix of basis)
agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
fit <- lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred <- predict(fit,newdata=list(age=age.grid),se=TRUE)

# plotting results
png("splines_regression.png")
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")
dev.off()

dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")

# natural splines fitting
fit2 <- lm(wage~ns(age,df=4),data=Wage)
pred2 <- predict(fit2,newdata=list(age=age.grid),se=TRUE)
lines(age.grid,pred2$fit,col="red",lwd=2)

png("splines_natural.png")
plot(age,wage,col="gray")
lines(age.grid,pred2$fit,lwd=2)
lines(age.grid,pred2$fit+2*pred2$se,lty="dashed")
lines(age.grid,pred2$fit-2*pred2$se,lty="dashed")
dev.off()

# smoothing splines
png("splines_smoothing.png")
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age,wage,df=16)
fit2 <- smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF", "6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
dev.off()

# local regression
png("splines_local_regression.png")
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local regression")
fit=loess(wage~age,span = .2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("16 DF", "6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
dev.off()


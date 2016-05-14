library(ISLR)
library(splines)
library(ggplot2)
library(gam)
library(akima)

attach(Wage)

# using natural splines (splines library)
gam1 <- lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

## using smooting splines (gam library)
gam.m3 <- gam(wage~s(year,4)+s(age,5)+education,data=Wage)

pdf("GAM.m3.pdf",width = 16, height = 8)
par(mfrow=c(1,3))
plot.gam(gam.m3,se=TRUE,col="blue")
dev.off()
pdf("GAM.1.pdf",width = 16, height = 8)
par(mfrow=c(1,3))
plot.gam(gam1,se=TRUE,col="red")
dev.off()

## comparison of models
gam.m1 <- gam(wage~s(age,5)+education,data=Wage)
gam.m2 <- gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")

## no evidence that the term s(year,4) in gam.m3 is necessary
summary(gam.m3) ## quick verification

## prediction using the gam model
preds <- predict(gam.m2,newdata=Wage)

## Gam with local regression fits (lo)
gam.lo <- gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
pdf("GAM.1.pdf",width = 16, height = 8)
par(mfrow=c(1,3))
plot.gam(gam.lo,se=TRUE,col="green")
dev.off()

## Gam with explicit interactions between age and year
pdf("GAM.lo.i.pdf",width = 16, height = 8)
par(mfrow=c(1,2))
gam.lo.i <- gam(wage~lo(year,age,span=0.5)+education,data=Wage)
plot(gam.lo.i)
dev.off()

## Gam logistic regression (link function)
gam.lr <- gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
pdf("GAM.lr.pdf",width = 16, height = 8)
par(mfrow=c(1,3))
plot.gam(gam.lr,se=TRUE,col="green")
dev.off()

## verifying high earners in category '< HS Grad'
table(education,I(wage>250)) # none

# Hence we remove that category and fit again
gam.lr.s <- gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
pdf("GAM.lr.s.pdf",width = 16, height = 8)
par(mfrow=c(1,3))
plot(gam.lr.s,se=TRUE,col="green")
dev.off()

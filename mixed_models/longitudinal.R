# libraries
pkg=c("xtable", "ggplot2")

install_or_load <- function(name){
  if (!is.element(name, installed.packages()[,1]))
    install.packages(name, dep = TRUE)
  require(name, character.only = TRUE)
}

for (i in pkg){
  install_or_load(i)
}


setwd("/home/sid/Dev/ISLRLabs/mixed_models/")
early.int1 <- read.table("../datasets/earlyint.txt", header=TRUE, sep=",")
# early.int1 <- xtable(early.int1[1:24,])
head(early.int1)
attach(early.int1)

## spaghetti plot
n <- length(unique(id))
interaction.plot(age,id,cog,xlab="Age(yrs)", ylab="COG", legend=F)

## descriptive statistics
early.mean <- tapply(cog, list(age,program), mean)
early.sd <- tapply(cog, list(age,program), sd)
early.var <- tapply(cog, list(age,program),var)
early.n <- table(age,program)

boxplot(cog~age, xlab="Age(yrs)",ylab="COG")
par(mfrow=c(2,1))
boxplot(cog[program==0]~age[program==0], main="No intervention", xlab="Age(yrs)",ylab="COG")
boxplot(cog[program==1]~age[program==1], main="Early intervention", xlab="Age(yrs)",ylab="COG")

## error bars plotting
errbar <- function(x,y,height,width,lty=1,col="black"){
  arrows(x, y, x, y+height, angle=90, length=width, lty=lty, col=col)
  arrows(x, y, x, y-height, angle=90, length=width, lty=lty, col=col)
}

par(mfrow=c(1,1))
plot(age[id==1], early.mean[,1], type="b", xlim=c(1,2), ylim=c(40,160), xlab="Age(yrs)", ylab="COG", axes=FALSE,
     main="Mean evolution (1 SE)")
axis(side=1, at=c(1,1.5,2), labels=c(1,1.5,2))
axis(side=2, at=c(40,160,20))

box()
points(age[id==1], early.mean[,2], type="b", col="red")
errbar(age[id==1]-.005, early.mean[,1], early.sd[,1], .1)
errbar(age[id==1]+.005, early.mean[,2], early.sd[,2], .1, col="red")

## correlation
early.int2 <- reshape(early.int1, timevar="age", idvar= c("id", "program"), direction="wide")
head(early.int2)
cor(early.int2[,3:5]) # correlation decays over time (makes sense)

## first level, linear regressions visualisation
# creation of time var (centering?)
early.int1$age0 <- early.int1$age - 1

cf <- sapply(early.int1$id, function(x){coef(lm(cog~age0, data=subset(early.int1, id==x)))})
Sx <- reorder(early.int1$id, cf[1,])
xyplot(cog~age0|Sx, groups=program, data=early.int1,
       type=c('p', 'r'), auto.key=TRUE, aspect="xy",
       par.settings=list(axis.text=list(cex=0.6),
                         fontsize=list(text=8, points=10)),
       scales=list(x=list(at=c(0,0.5,1), labels=c("0", "0.5", "1"))))

# visualization of distribution of slopes, intercept, R²
# coef
lin.reg.coef <- by(early.int1, early.int1$id, function(data){coef(lm(cog~age0, data=data))})
lin.reg.coef1 <- unlist(lin.reg.coef)
names(lin.reg.coef1) <- NULL
lin.reg.coef2 <- matrix(lin.reg.coef1, length(lin.reg.coef1)/2, 2, byrow= TRUE)
# R²
lin.reg.r.squared <- by(early.int1, early.int1$id, function(data){summary(lm(cog~age, data=data))$r.squared})
lin.reg.r.squared1 <- as.vector(unlist(lin.reg.r.squared))
par(mfrow=c(3,1))
hist(lin.reg.coef2[,1], xlab="Intercept", col="lightblue", main="Histogram of individual intercept")
hist(lin.reg.coef2[,2], xlab="Slope", col="lightblue", main="Histogram of individual slopes")
hist(lin.reg.r.squared1, xlab="Rsq", col="lightblue", main="Histogram of individual Rsq")

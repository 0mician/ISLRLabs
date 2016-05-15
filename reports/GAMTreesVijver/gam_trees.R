##############################
# Solution for question 10 ###
##############################
library(ISLR)
library(boot)
library(ggplot2)
library(leaps)
library(gam)

attach(College)
set.seed(1)
train <- sample(c(TRUE,FALSE), nrow(College), rep=TRUE)
test <- (!train)

## a
model.fwd <- regsubsets(Outstate~.,data=College[train,], nvmax=17,method="forward")
test.mat <- model.matrix(Outstate~., data=College[test,])

val.errors <- rep(NA, 17)
for(i in 1:17){
    coefi <- coef(model.fwd, id=i)
    pred <- test.mat[,names(coefi)] %*% coefi
    val.errors[i] <- mean((College$Outstate[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(model.fwd, 6)

## b
model.gam <- gam(Outstate~Private+Room.Board+Terminal+perc.alumni+Expend+Grad.Rate,data=College[train,])
pdf("gam_trees.pdf", width=16, height=12)
par(mfrow = c(3, 2))
plot.gam(model.gam,se=TRUE,col="green")
dev.off()

model.gam.s2 <- gam(Outstate~Private+s(Room.Board,df=2)+s(Terminal,df=2)+s(perc.alumni,df=2)+s(Expend,df=2)+s(Grad.Rate,df=2),data=College[train,])
pdf("gam_trees_s_2.pdf", width=16, height=12)
par(mfrow = c(3, 2))
plot.gam(model.gam.s,se=TRUE,col="green")
dev.off()

model.gam.s.3 <- gam(Outstate~Private+s(Room.Board,df=3)+s(Terminal,df=3)+s(perc.alumni,df=3)+s(Expend,df=3)+s(Grad.Rate,df=3),data=College[train,])
pdf("gam_trees_s_3.pdf", width=16, height=12)
par(mfrow = c(3, 2))
plot.gam(model.gam.s.3,se=TRUE,col="green")
dev.off()

model.gam.s.4 <- gam(Outstate~Private+s(Room.Board,df=4)+s(Terminal,df=4)+s(perc.alumni,df=4)+s(Expend,df=4)+s(Grad.Rate,df=4),data=College[train,])

## c
gam.tss <- mean((College[test,]$Outstate - mean(College[test,]$Outstate))^2)

gam.pred <- predict(model.gam,College[test,])
gam.err <- mean((College[test,]$Outstate - gam.pred)^2)
rss <- 1 - gam.err/gam.tss

gam.pred.s2 <- predict(model.gam.s2,College[test,])
gam.err.s2 <- mean((College[test,]$Outstate - gam.pred.s2)^2)
rss.2 <- 1 - gam.err.s2/gam.tss

gam.pred.s3 <- predict(model.gam.s.3,College[test,])
gam.err.s3 <- mean((College[test,]$Outstate - gam.pred.s3)^2)
rss.3 <- 1 - gam.err.s3/gam.tss


gam.pred.s4 <- predict(model.gam.s.4,College[test,])
gam.err.s4 <- mean((College[test,]$Outstate - gam.pred.s4)^2)
rss.4 <- 1 - gam.err.s4/gam.tss

## d
summary(model.gam.s2)

##############################
# Solution for Vijver data ###
##############################

library(glmnet)
library(tree)
library(ggplot2)

load("VIJVER.Rdata")
set.seed(1)
x <- model.matrix(meta~.,data)[,-1]
y <- data$meta

# Performance with lasso
grid <- 10^seq(10,-2,length=100)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)

lasso.mod <- glmnet(y=y[train],x=(x[train,]),alpha=1,family="binomial")
plot(lasso.mod)
cv.out <- cv.glmnet(x[train ,],y[train],alpha=1,family="binomial")
plot(cv.out)

lasso.pred <- predict(lasso.mod,s=cv.out$lambda.min,newx=x[test,],type="response")
plot(lasso.pred~y[test])
pred <- rep("DM",length(test))
pred[lasso.pred>0.5]="NODM"
table(y[test],pred)
perf.lasso <- length(which(pred==y[test]))/length(test)
perf.lasso

## Performance with Ridge
ridge.mod <- glmnet(y=y[train],x=(x[train,]),alpha=0,family="binomial")
plot(ridge.mod)
ridge.cv <- cv.glmnet(x[train ,],y[train],alpha=0,family="binomial")
plot(ridge.cv)
ridge.pred <- predict(ridge.mod,s=ridge.cv$lambda.min,newx=x[test,],type="response")
plot(ridge.pred~y[test])
pred <- rep("DM",length(test))
pred[ridge.pred>0.5] <- "NODM"
table(y[test],pred)
perf.ridge <- length(which(pred==y[test]))/length(test)
perf.ridge

##
tree.mod <- tree(meta~.,data=data,subset=train)

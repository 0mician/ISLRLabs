##############################
# Solution for question 10 ###
##############################
library(ISLR)
library(boot)
library(ggplot2)
library(leaps)
library(gam)
library(ROCR)

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
model.gam <- gam(Outstate~Private+Room.Board+Terminal+perc.alumni+Expend+
                     Grad.Rate,data=College[train,])
pdf("gam_trees.pdf", width=16, height=12)
par(mfrow = c(3, 2))
plot.gam(model.gam,se=TRUE,col="green")
dev.off()

model.gam.s2 <- gam(Outstate~Private+s(Room.Board,df=2)+s(Terminal,df=2)+
                        s(perc.alumni,df=2)+s(Expend,df=2)+s(Grad.Rate,df=2),
                    data=College[train,])
pdf("gam_trees_s_2.pdf", width=16, height=12)
par(mfrow = c(3, 2))
plot.gam(model.gam.s,se=TRUE,col="green")
dev.off()

model.gam.s.3 <- gam(Outstate~Private+s(Room.Board,df=3)+s(Terminal,df=3)+
                         s(perc.alumni,df=3)+s(Expend,df=3)+s(Grad.Rate,df=3),
                     data=College[train,])
pdf("gam_trees_s_3.pdf", width=16, height=12)
par(mfrow = c(3, 2))
plot.gam(model.gam.s.3,se=TRUE,col="green")
dev.off()

model.gam.s.4 <- gam(Outstate~Private+s(Room.Board,df=4)+s(Terminal,df=4)+
                         s(perc.alumni,df=4)+s(Expend,df=4)+s(Grad.Rate,df=4),
                     data=College[train,])

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
library(randomForest)
library(gbm)
library(ggplot2)

load("VIJVER.Rdata")
set.seed(1)
x <- model.matrix(meta~.,data)[,-1]
y <- data$meta

# Performance with lasso
grid <- 10^seq(10,-2,length=100)
train <- sample(1:nrow(x), 126)

lasso.mod <- glmnet(y=y[train],x=(x[train,]),alpha=1,family="binomial")
plot(lasso.mod)
cv.out <- cv.glmnet(x[train ,],y[train],alpha=1,family="binomial")
plot(cv.out)
vals <- predict(lasso.mod,s=cv.out$lambda.min,type="coefficients")
predictors.lasso <- colnames(x)[vals@i]

lasso.pred <- predict(lasso.mod,s=cv.out$lambda.min,newx=x[-train,],type="response")
plot(lasso.pred~y[-train])
pred <- rep("DM",188-length(train))
pred[lasso.pred>0.5]="NODM"
table(y[-train],pred)
perf.lasso <- length(which(pred==y[-train]))/(188-length(train))
perf.lasso

## Performance with ridge
ridge.mod <- glmnet(y=y[train],x=x[train,],alpha=0,family="binomial",lambda=grid)
plot(ridge.mod)
ridge.cv <- cv.glmnet(x[train,],y[train],alpha=0,family="binomial")
plot(ridge.cv)
ridge.pred <- predict(ridge.cv,s=ridge.cv$lambda.min,newx=x[-train,],type="response")
plot(ridge.pred~y[test])
pred <- rep("DM",188-length(train))
pred[ridge.pred>0.5] <- "NODM"
table(y[-train],pred)
perf.ridge <- length(which(pred==y[-train]))/(188-length(train))
perf.ridge

##########################
## Classification trees ##
##########################

set.seed(1)
train <- sample(1:nrow(data), 126)
data.test <- data[-train,]
meta.test <- data$meta[-train]

## simple tree
##############

tree.data <- tree(meta~.,data=data,subset=train)
tree.pred <- predict(tree.data, data.test, type="class")
table(tree.pred, meta.test)
(15+25)/62 # perf=64.5%

pdf("tree_simple.pdf")
plot(tree.data)
text(tree.data, pretty=0)
dev.off()

## pruned simple tree
#####################

cv.data <- cv.tree(tree.data, FUN=prune.misclass)
cv.data # best depth=4
prune.data <- prune.misclass(tree.data, best=4)
tree.pred <- predict(prune.data, data.test, type="class")
table(tree.pred, meta.test)
(13+26)/62 # perf=63%

pdf("tree_pruned.pdf")
plot(prune.data)
text(prune.data, pretty=0)
dev.off()

#############################################################################
## Note: for the following techniques, I used the variables selected by lasso
data.lasso <- data[c("meta", predictors.lasso)]
data.test <- data.lasso[-train,]
meta.test <- data.lasso$meta[-train]

## bagging
##########

bag.data <- randomForest(meta~., data=data.lasso, subset=train, mtry=12, importance=TRUE)
yhat.bag <- predict(bag.data, newdata=data.test)
plot(yhat.bag, meta.test)
table(yhat.bag, meta.test)
(18+23)/62
importance(bag.data)

## random forest
################

rf.data <- randomForest(meta~., data=data.lasso, subset=train, importance=TRUE)
yhat.bag <- predict(rf.data, newdata=data.test)
plot(yhat.bag, meta.test)
table(yhat.bag, meta.test)
(18+24)/62
importance(rf.data)

## boosting
###########
## converting to binary (0,1) response
data.gbm <- data.lasso[,-1]
boolean <- ifelse(data.lasso$meta=="NODM", 1, 0)
data.gbm <- data.frame(boolean, data.gbm)
data.test <- data.gbm[-train,]

boost.data <- gbm(boolean~., data=data.gbm[train,], distribution="bernoulli",
                  n.trees=5000, interaction.depth=4)

yhat.boosting <- predict(boost.data, newdata=data.test, n.trees=100,distribution="bernoulli")
yhat.pred <- rep(0, 62)
yhat.pred[yhat.boosting>.5]=1
plot(yhat.boosting, data.test)
table(yhat.pred, boolean[-train])
(27+12)/62

## ploting variable importance for each techniques
pdf("bag_data.pdf")
varImpPlot(bag.data)
dev.off()

pdf("rf_data.pdf")
varImpPlot(rf.data)
dev.off()

pdf("boost_data.pdf")
summary(boost.data)
dev.off()

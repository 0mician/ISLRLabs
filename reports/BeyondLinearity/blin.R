library(glmnet)
library(polycor)
library(ROCR)
library(leaps)
library(bestglm)
library(ggplot2)
library(pls)

## Data exploration

load("VIJVER.Rdata")
str(data); dim(data);

hetcor(data$meta, data$J00129)
hetcor(data$meta, data$NM_002318)
hetcor(data$meta, data$NM_003070)

## Systematic identification of correlated genex/outcome
genes <- character()
i <- 1
for(gene in names(data)[2:length(data)]) {
    if(abs(hetcor(data$meta, data[gene])$correlations[1,2]) > 0.45){
        genes[i] <- gene
        i <- i + 1
    }
}

reduced.data <- data.frame(data$meta)
for(gene in genes){
    reduced.data <- cbind(reduced.data, data[gene])
}

## Use the identified variables to create a model
regfit.full <- regsubsets(data.meta~., reduced.data, nvmax = length(genes))
reg.summary <- summary(regfit.full)

## using bic to make a decision, n=4
which.min(reg.summary$bic)
coef(regfit.full,4)

## fitting model with the 4 params
reg <- glm(meta~NM_000987+NM_003258+NM_004119+NM_002811, data=data,family = binomial(link=logit))
summary(reg)
reg.probs <- predict(reg, type="response")
contrasts(data$meta)
table(data$meta, fitted(reg)>0.5)
predict <- fitted(reg)
pred <- prediction(predict, data$meta)
perf <- performance(pred, measure="tpr", x.measure = "fpr")
performance(pred, measure="auc")

## plotting the results
pdf("bic-auc.pdf", width = 16, height = 8)
par(mfrow = c(1,2))
plot(reg.summary$bic, ylab="bic", type="l")
plot(perf, col="red")
dev.off()

########################################################################
## using CV (10-k fold) to make a decision                            ## 
## this needs more work, doesn't work as is (maybe move to bestglm()) ##
########################################################################

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(reduced.data), replace=TRUE)
cv.errors <- matrix(NA, k, length(genes), dimnames = list(NULL, paste(1:length(genes))))

predict.regsubsets <- function(object, newdata, id, ...){
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id=id)
    xvars <- names(coefi)
    mat[,xvars]%*%coefi
}

for(j in 1:k){
    best.fit <- regsubsets(data.meta~., data=reduced.data[folds!=j,],nvmax = length(genes))
    for(i in 1:length(genes)){
        pred <- predict(best.fit, reduced.data[folds==j,],id=i)
        cv.errors[j,i] <- mean((reduced.data$data.meta[folds==j]-pred)^2)
    }
}

mean.cv.errors <- apply(cv.errors,2,mean)
mean.cv.errors


hetcor(data$meta, data$NM_003258)
hetcor(data$meta, data$NM_007267)
plot(data$meta, data$NM_003258)
shapiro.test(data$NM_003258)
t.test(data$meta, data$NM_003258)

predictor <- split(data$NM_003258,data$meta)
shapiro.test(predictor$DM)
shapiro.test(predictor$NODM)
var.test(predictor$DM,predictor$NODM)
t.test(predictor$DM,predictor$NODM)

reg <- glm(meta~NM_003258+NM_007267, data=data,family = binomial(link=logit))
summary(reg)
reg.probs <- predict(reg, type="response")
contrasts(data$meta)
table(data$meta, fitted(reg)>0.5)

predict <- fitted(reg)
pred <- prediction(predict, data$meta)
perf <- performance(pred, measure="tpr", x.measure = "fpr")
plot(perf, col="red")
performance(pred, measure="auc")


reg2 <- glm(meta~NM_003258, data=data,family = binomial(link=logit))
summary(reg2)
reg2.probs <- predict(reg2, type="response")
contrasts(data$meta)
table(data$meta, fitted(reg2)>0.5)

predict2 <- fitted(reg2)
pred2 <- prediction(predict2, data$meta)
perf2 <- performance(pred2, measure="tpr", x.measure = "fpr")
plot(perf2, col="red")
performance(pred2, measure="auc")

######################
## multicolinearity ##
######################

pairs(reduced.data[,2:8])
cor(reduced.data$NM_002808,reduced.data$NM_002811)
pdf("pairs-colinearity.pdf", width = 16, height = 16)
pairs(reduced.data[,9:16])
dev.off()
cor(reduced.data$NM_003981,reduced.data$NM_004701)

##########################
## phenotype prediction ##
##########################

set.seed(1)
x <- model.matrix(meta~.,data)[,-1]
y <- data$meta

## Ridge regression with CV for lambda
grid <- 10^seq(10,-2,length=100)
ridge.cv <- cv.glmnet(x,y,alpha=0, family='binomial')
plot(ridge.cv)
ridge.bestlam <- cv.out$lambda.min

ridge.mod <-  glmnet(x,y,alpha=0,lambda=grid,family='binomial')
ridge.pred <- predict(ridge.mod, type="response",s=ridge.bestlam, newx=x)
table(y, ridge.pred>0.5)

ridge.pred <- prediction(ridge.pred, y)
ridge.perf <- performance(ridge.pred, measure="tpr", x.measure = "fpr")
performance(ridge.pred, measure="auc")

## Lasso with CV for lambda
lasso.cv <- cv.glmnet(x,y,alpha=1,lambda=grid,family='binomial')
plot(lasso.mod)
lasso.bestlam <- lasso.cv$lambda.min

lasso.mod <- glmnet(x,y,alpha=1,lambda=grid,family='binomial')
lasso.coef <- predict(lasso.mod, type="coefficients",s=lasso.bestlam)
lasso.coef[lasso.coef!=0]
lasso.pred <- predict(lasso.mod, type="response",s=lasso.bestlam, newx=x)
table(y, lasso.pred>0.5)

lasso.pred <- prediction(lasso.pred, y)
lasso.perf <- performance(lasso.pred, measure="tpr", x.measure = "fpr")
performance(lasso.pred, measure="auc")

## plotting results of ridge and lasso (ROC)
pdf("roc-lasso-ridge.pdf", width = 16, height = 8)
par(mfrow = c(1,2))
plot(ridge.perf, col="red", main="ridge regression")
plot(lasso.perf, col="red", main="lasso")
dev.off()


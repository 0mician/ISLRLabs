# TODO: Add comment
# 
# Author: robjelier
###############################################################################



library(ISLR) 
library(leaps) 
library(gam)

#A. Split the data, and forward stepwise selection

set.seed(1)
train = sample(1:dim(College)[1],dim(College)[1]*1/2) 
test = (-train)
y = College$Outstate
summary(College)
dim(College)
attach(College)

regfit.fwd=regsubsets(Outstate~. ,College[train,],method="forward")
summary(regfit.fwd)
par(mfrow=c(1,1))
plot(summary(regfit.fwd)$bic,type="b")
plot(regfit.fwd, scale="bic")
#B-D. Fit a GAM, plot the results, evaluate the model. Are there non-linear effects? 
gam6 = gam(Outstate~Private +s(Room.Board,4) +s(PhD,4)+ s(perc.alumni,4)+s(Expend,4)+s(Grad.Rate,4) ,data=College,subset=train) 
par(mfrow=c(2,3))
plot(gam6,se=TRUE,col="purple")
summary(gam6)
gam6a = gam(Outstate~Private +s(Room.Board,4) +s(PhD,2)+ perc.alumni+s(Expend,4)+s(Grad.Rate,2)  ,data=College,subset=train) 
plot(gam6a,se=TRUE,col="blue")
summary(gam6a)
predgam = predict(gam6, newdata=College[test,]) 
msegam1 = mean((predgam-y[test])^2)
predgam = predict(gam6a, newdata=College[test,]) 
msegam2 = mean((predgam-y[test])^2)
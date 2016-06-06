library(mice)
library(lattice)
library(VIM)
library(aod)
library(BaM)

titanic.missing <- read.table("../datasets/titanic.txt", header=TRUE, sep=",")
titanic.missing <- titanic.missing[-c(1,4,6,7,8,9,10)][c(2,1,4,3)]
head(titanic.missing)

## exploring missingness
titanic.missing.aggr <- aggr(titanic.missing, numbers=TRUE, prop=FALSE, 
                             ylab=c("Histogram of missing data", "Pattern"))
titanic.missing.aggr
aggr(titanic.missing, combined=TRUE, numbers=TRUE, prop=TRUE, 
     cex.numbers=0.87, varheight=FALSE)
barMiss(titanic.missing[,c("survived", "age")])
barMiss(titanic.missing[,c("pclass", "age")])
histMiss(titanic.missing)

## complete case analysis
titanic.log.cc <- glm(survived~pclass+sex+age, family=binomial, data=titanic.missing)
summary(titanic.log.cc  )
wald.test(b=coef(titanic.log.cc), Sigma=vcov(titanic.log.cc), Terms=2:3) # global effect of class
exp(cbind(OR=titanic.log.cc$coefficients, confint(titanic.log.cc))) # OR

## multiple imputation
######################
pattern = md.pattern(titanic.missing)
pattern
pairs <- pairs(md.pairs(titanic.missing))
pairs # not working here

imp <- mice(titanic.missing, meth=c("","","","pmm"), m=10)
imp

# diagnostics
imp$imp$age[1:10,1:5] # any fishy values? age=187?
complete(imp,1)[1:10,] # in first imp dataset (10 have been created)

com <- complete(imp, "long", inc=TRUE)
col <- rep(c("blue", "red")[1+as.numeric(is.na(imp$data$age))],101)
stripplot(age~.imp, data=com, jit=TRUE, fac=0.8, col=col, pch=20, cex=1.4,
          xlab="Imputation number")

# analysis
fit <- with(data=imp, exp=glm(survived~pclass+sex+age, family=binomial))
# creating a dataset with the results of all analysis (optional)
MI.matrix <- matrix(0, 10, 5)
for(k in 1:10){
  MI.matrix[k,] <- coefficients(fit$analyses[[k]])
}
MI.results <- data.frame(Intercept=MI.matrix[,1], pclass2=MI.matrix[,2],
                         pclass3=MI.matrix[,3], sex=MI.matrix[,4], age=MI.matrix[,5])
MI.results[1:10,]

# combining the results using rubin's rule
est <- pool(fit)
summary(est)

## Inverse Probability Weighting
################################
# missing value indicator
titanic.missing$r <- as.numeric(!is.na(titanic.missing$age))*as.numeric(!is.na(titanic.missing$sex))
head(titanic.missing)
titanic.ipw.glm <- glm(r~pclass+survived+sex, family=binomial, data=titanic.missing)
summary(titanic.ipw.glm)
titanic.results.ipw <- glm(r~pclass+survived+sex, family=binomial, data=titanic.missing, weights=titanic.missing$w)
summary(titanic.results.ipw)

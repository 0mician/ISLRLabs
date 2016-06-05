library(pastecs)
library(qpcR)

setwd(dir = "/home/sid/Dev/ISLRLabs/linear_models")
donner = read.table(file = '../datasets/donner-class.txt',
                    row.names=1,
                    header=TRUE)
head(donner)

# keeping only relevant vars
donner.na <- na.omit(subset(donner, select=c('Age','Outcome','Sex')))
donner.na$fem <- as.numeric(donner.na$Sex=="Female")
head(donner.na)

# fitting model
donner.log <- glm(Outcome~Age + fem, data=donner.na, family=binomial(link="logit"))
summary(donner.log)

# estimation of odds ratio
exp(donner.log$coefficients)
exp(confint(donner.log))
exp(cbind(OR=donner.log$coefficients, confint(donner.log)))
exp(donner.log$coefficients*10) # odd ration for survival after 10 yrs increased
exp(c(OR=donner.log$coefficients[2]*10, confint(donner.log)[2,]*10))

# plotting results
logit <- function(x){log(x/(1-x))}
ilogit <- function(x, a, b){exp(a+b*x)/(1+exp(a+b*x))}
cl <- coef(donner.log)
plot(donner.na$Age, jitter(donner.na$Outcome,.2),col=c("red","black")[donner.na$Sex],pch=20,
     cex=1.2,xlab="Age",ylab="Status (jittered)")
curve(ilogit(cl[1]+cl[2]*x+cl[3]*0,0,1),add=TRUE)
curve(ilogit(cl[1]+cl[2]*x+cl[3]*1,0,1),add=TRUE, col="red")
legend("topright", pch=20,lty="solid",col=c("red","black"),c("women","men"))

# predicting outcome
newdata2 <- data.frame(fem=1, Age=mean(donner.na$Age))
newdata2$greP <- predict(donner.log, newdata=newdata2, type="response")
newdata2
newdata3 <- data.frame(fem=0, Age=mean(donner.na$Age))
newdata3$greP <- predict(donner.log, newdata=newdata3, type="response")
newdata3
# same thing but combined
newdata4 <- data.frame(fem=c(1,0), Age=mean(donner.na$Age))
newdata4$greP <- predict(donner.log, newdata=newdata4, type="response")
newdata4

# model with interaction
m4 <- glm(Outcome ~ Age*fem, data=donner.na, family=binomial(link="logit"))
summary(m4)
# plotting results
logit <- function(x){log(x/(1-x))}
ilogit <- function(a, b, x, c, y, d, z){exp(a+b*x+c*y+d*z)/(1+exp(a+b*x+c*y+d*z))}
cl <- coef(m4)
plot(donner.na$Age, jitter(donner.na$Outcome,.2),col=c("red","black")[donner.na$Sex],pch=20,
     cex=1.2,xlab="Age",ylab="Status (jittered)")
curve(ilogit(cl[1], cl[2], x, cl[3], 1, cl[4], x), add=TRUE, col="red")
curve(ilogit(cl[1], cl[2], x, cl[3], 0, cl[4], 0),add=TRUE)
legend("topright", pch=20,lty="solid",col=c("red","black"),c("women","men"))

# model selection
m1 <- glm(Outcome~Age, data=donner.na, family=binomial(link = logit))
m2 <- glm(Outcome~fem, data=donner.na, family=binomial(link = logit))
m3 <- glm(Outcome~Age+fem, data=donner.na, family=binomial(link = logit))
m4 <- glm(Outcome~Age*fem, data=donner.na, family=binomial(link = logit))

model.list <- list(m1, m2, m3, m4)
aics2 <- sapply(model.list, function(x){AIC(x)})
akaike.weights(aics2)
aics2
cbind(c("m1","m2","m3","m4"), aics2, akaike.weights(aics2)$deltaAIC)


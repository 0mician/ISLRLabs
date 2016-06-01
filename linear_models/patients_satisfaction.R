library(pastecs)

setwd(dir = "/home/sid/Dev/ISLRLabs/linear_models")
satisfaction = read.table(file = '../datasets/satisfaction.txt', header=TRUE)
head(satisfaction)
plot(satisfaction) # equivalent to pairs

cor(satisfaction)
stat.desc(satisfaction, basic=TRUE, desc=TRUE)

# fitting model
satis.lm <- lm(satis~age+severity+anxiety, data=satisfaction)
summary(satis.lm)

# null model (without covariates), and comparison between models
# p-value from summary(satis.lm) == p-value comparison (hence the p-value in
# summary is testing H0: all betas are 0, H1: at least some are not)
satis.null <- lm(satis~1, data=satisfaction)
anova(satis.null, satis.lm  )

# anova of a model (different from coefficients in the summary(lm))
# why? Anova compares model nestedly, each line vs. the previous one
anova(satis.lm)

# change order of lm doesn't change p-values in summary.
# why? test for each variable if out vs. with other covariates in
# (no nesting like ANOVA)
satis.lm <- lm(satis~age+anxiety+severity, data=satisfaction)
summary(satis.lm)
satis.lm <- lm(satis~anxiety+age+severity, data=satisfaction)
summary(satis.lm)
anova(satis.lm)

# let's leave severity out
satis.lm.final <- lm(satis~age+anxiety, data=satisfaction)

# predicting new value
newdata <- data.frame(age=43, anxiety=2.7)
predict(satis.lm.final, newdata, interval="predict")
predict(satis.lm.final, newdata, interval="confidence")

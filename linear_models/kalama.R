library(pastecs)

options(digits=2) # nicer
setwd(dir = "/home/sid/Dev/ISLRLabs/linear_models")

kalama = read.table("../datasets/kalama.txt", header=TRUE)

# basic summary statistics
stat.desc(kalama[,c("age","height")], basic=TRUE, desc=TRUE)
cov(kalama$age, kalama$height)
cor(kalama$age, kalama$height)
cor.test(kalama$age, kalama$height, alternative="two.sided", method="pearson")

# simple linear model
kalama.lm <- lm(height~age, data=kalama)
kalama.anova <- anova(kalama.lm)
summary(kalama.lm) # age explains really well the variability in size
kalama.anova

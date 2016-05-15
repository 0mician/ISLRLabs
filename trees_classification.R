library(ISLR)
library(tree)
library(ggplot2)

attach(Carseats)

# recoding Sales as a binary variable
High <- ifelse(Sales<=8,"No","Yes")
Carseats <- data.frame(Carseats, High)

# fitting tree
tree.carseats <- tree(High~.-Sales,Carseats)
summary(tree.carseats)
tree.carseats # outputs tree textually
png("tree_classification.png") 
plot(tree.carseats) # tree come with overloaded plot function
text(tree.carseats,pretty=0)
dev.off()

# Assessement of performance
set.seed(1)
train <- sample(1:nrow(Carseats),200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]

tree.carseats <- tree(High~.-Sales,Carseats,subset=train)
tree.pred <- predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(98+56)/(200) # 77% accuracy

# trying to prune tree to reduce overfitting
cv.carseats <- cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats # size of 4 gives us the smallest CV error

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

prune.carseats <- prune.misclass(tree.carseats,best=4)
png("tree_classification_pruned.png") 
plot(prune.carseats)
text(prune.carseats,pretty=0)
dev.off()

# assessement of performance of pruned tree
tree.pred <- predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(92+55)/(200) # 74%


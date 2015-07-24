# 1. Basic commands
x <- c(1, 3, 2, 5)
x

x <- c(1, 6, 2)
x

y <- c(1, 4, 3)
length(x)
length(y)
x+y

ls()

rm(x, y) # rm(list=ls())

?matrix

x <- matrix(data=c(1, 2, 3, 4), nrow=2, ncol=2)
x

x <- matrix(c(1, 2, 3, 4), 2, 2)
x

x <- matrix(c(1, 2, 3, 4), 2, 2, byrow=TRUE)
x

sqrt(x)
x^2

x <- rnorm(50)
y <- x + rnorm(50, mean=50, sd=.1)
cor(x,y)

set.seed(1303)
rnorm(50)

set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

# 2. Graphics
x <- rnorm(100)
y <- rnorm(100)
plot(x, y, xlab="this is the x-axis", ylab="this is the y-axis", main="plot of x vs. y")

pdf("figure1.pdf")
plot(x, y, col="green")
dev.off()

x <- seq(1, 10)
x
x <- 1:10
x
x <- seq(-pi, pi, length=50)
x

y <- x
f <- outer(x, y, function(x, y) cos(y) / (1+x^2))
contour(x, y, f)
contour(x, y, f, nlevels=45, add=TRUE)

fa <- (f-t(f))/2
contour(x, y, fa, nlevels=15)
image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta=30)
persp(x, y, fa, theta=30, phi=40)

# 3. Indexing Data
# note: index starts at 1 in R
A <- matrix(1:16, 4, 4) 
A[2, 3] 
A[c(1, 3), c(2, 4)]
A[1:3, 2:4]
A[1:2,]
A[, 1:2]
A[1,] # returns a vector

A[-c(1, 3),] # - sign tells R to keep all columns but c(1, 3)
A[-c(1, 3), -c(1, 3, 4)]

# 4. Loading data
Auto <- read.table("datasets/Auto.data", header=TRUE, na.strings='?')
fix(Auto)
dim(Auto)

Auto <- na.omit(Auto) # removes incomplete entries
dim(Auto)
names(Auto) # returns a vector with column names

# 5. Additional graphical and numerical summaries

# different ways to access column vectors in dataset
plot(Auto$cylinders, Auto$mpg)
with(Auto, plot(cylinders, mpg))
attach(Auto)
plot(cylinders, mpg)

cylinders <- as.factor(cylinders) # converts qualitative to quantitative

# x-axis (cylinders) is categorical -> plot issues boxplots automagically
plot(cylinders, mpg, col="red", varwidth=TRUE, xlab="Cylinders", ylab="MPG")
hist(mpg, col=2, breaks=15)

pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

plot(horsepower, mpg)
identify(horsepower, mpg, name) # using mouse, can identify points on the graph

summary(mpg)
summary(Auto)

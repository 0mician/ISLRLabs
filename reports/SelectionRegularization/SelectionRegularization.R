##################
### Exercise 8 ###
##################

library(ggplot2)

## part a: simulated dataset creation
set.seed(1)
x <- rnorm(100)
noise <- rnorm(100)

## part b: response vector with given model y = 1x^3 - 2x^2 + 3x + 5
y <- x^3 - 2*x^2 + 3*x + 5
qplot(x,y)
ggsave("fun.pdf")

## part c: perform best subset selection

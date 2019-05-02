library(lattice)
library(dplyr)
set.seed(1001)
allRand <- data.frame(
  x = rnorm(1000)
  , y = rnorm(1000)
  , z = rnorm(1000)
)
allRand <- allRand %>% mutate(
  a = sqrt(x^2 + y^2 + z^2)
  , af = factor(round(a))
  , b = abs(x) + abs(y) + abs(z)
  , bf = factor(round(b))
)


cloud(z~x*y, data = allRand
      , col = allRand$af)
cloud(z~x*y, data = allRand
      , col = allRand$bf)

shingle.a <- equal.count(allRand$a)
cloud(z~x*y | shingle.a , data = allRand, col = allRand$af)
cloud(z~x*y | af , data = allRand, col = allRand$af)

shingle.b <- equal.count(allRand$b)
cloud(z~x*y | shingle.b , data = allRand, col = allRand$bf)
cloud(z~x*y | bf , data = allRand, col = allRand$bf)


splom(allRand[,-(5:7)], alpha = 0.3, col = allRand$af)
splom(allRand[,-c(4,5,7)], alpha = 0.3, col= allRand$bf)

require(psych)
library(pastecs)
require(e1071)
library(modeest)
demo <- read.csv("demographics.csv")
m <- mean(demo$income)
s <- sd(demo$income)
v <- var(demo$income)
mn <- min(demo$income)
mx <- max(demo$income)
r <- range(demo$income)
r2 <- mx - mn
md <- median(demo$income)
q <- quantile(demo$income)

demo2 <- cbind(demo$age, demo$income, demo$carpr)
colnames(demo2) <- c("age", "inc", "pr")
View(demo2)
describe(demo2)
describe(demo2, na.rm = TRUE, trim = 0.05, check = TRUE)

options(scipen = 100)
options(digits = 2)
stat.desc(demo2)
stat.desc(demo2, basic = FALSE)
stat.desc(demo2, desc = FALSE)

skewness(demo$income)
kurtosis(demo$income)

quantile(demo$income, probs= c(0.17, 0.57, 0.97))
quantile(demo$income)

mlv(demo$income, method = "mfv") # interesting

describeBy(demo$income, demo$educ)

aggregate(demo$income
          , by=list(demo$marital)
          , FUN = mean)
aggregate(demo$income
          , by=list(demo$marital)
          , FUN = sd)
aggregate(demo$income
          , by=list(demo$marital)
          , FUN = median)
aggregate(demo$income
          , by=list(demo$marital)
          , FUN = var)
aggregate(demo$income
          , by=list(demo$marital
                    , demo$gender)
          , FUN = mean)

bl <- read.csv("bankloan.csv")
dm <- read.csv("directmail.csv")
ic <- read.csv("icecream.csv")
mean(bl$balance)
var(bl$balance)
sd(bl$balance)
describe(bl$balance)
describe(dm$age)
stat.desc(bl$balance)
skew(ic$icecream)
kurtosis(ic$icecream)
quantile(ic$icecream, probs = c(0.07, 0.38, 0.72))
describeBy(ic$icecream, ic$month)
aggregate(dm$age
          , by=list(dm$educ)
          , FUN = mean)

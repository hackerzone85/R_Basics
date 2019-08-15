demo <- read.csv("demographics.csv")
t.test(demo$income
       , alternative = "two.sided"
       , mu = 70)

mytable <- table(demo$gender)
mytable

binom.test(mytable
           , p = 0.5
           , alternative = "two.sided"
           , conf.level = 0.95)

# is the prop of males 60% ?
# success in this case is a Female, so looking for 0.4
binom.test(mytable
           , p = 0.4
           , alternative = "two.sided"
           , conf.level = 0.95)
# here, reject the null
# True prop male not equal to 60%

mytable <- table(demo$educ)
n <- length(mytable)
thprop <- 1/n
chisq.test(mytable, p = rep(thprop, 5)) # not an equal distribution
ch <- chisq.test(mytable, p = rep(thprop, 5))
exp <- ch$expected
res <- ch$residuals
stres <- ch$stdres

# try a different distr
chisq.test(mytable, p = c(0.3, 0.3, 0.2, 0.1, 0.1)) # not an equal distribution

hw <- read.csv("hw.csv")
t.test(hw$weight
       , alternative = "less"
       , mu = 75)

mnet <- read.csv("mobilenet.csv")
t.test(mnet$hours
       , alternative = "greater"
       , mu = 9)

bank <- read.csv("bankloan.csv")
mytable <- table(bank$loan)
binom.test(mytable, p = 0.9, alternative = "less")
binom.test(mytable, p = 0.9
           , alternative = "two.sided"
           , conf.level = 0.975)

mail <- read.csv("directmail.csv")
mytable <- table(mail$educ)
ch <- chisq.test(mytable, p = rep(1/length(mytable), length(mytable)))
ch
mytable
ch$expected
ch$residuals
ch$stdres

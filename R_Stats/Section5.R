library(lattice)
library(ggplot2)
demo <- read.csv("demographics.csv")
shapiro.test(demo$income)

m <- mean(demo$income)
std <- sd(demo$income)

ggplot(data = demo) +
  geom_histogram(aes(x = income, y = ..density..)
                 , fill = "red") +
  stat_function(fun = dnorm
                , args = list(mean = m
                          , sd = std))

# convert to a standard normal and look for values
# gt le abs 3
zinco <- scale(demo$income, scale = TRUE)
sort(zinco, decreasing = TRUE) 

bank <- read.csv("bankloan.csv")
shapiro.test(bank$balance)
m <- mean(bank$balance, na.rm = TRUE)
std <- sd(bank$balance, na.rm = TRUE)

ggplot(data = bank) +
  geom_histogram(aes(x = balance, y = ..density..)
                 , fill = "red") +
  stat_function(fun = dnorm
                , args = list(mean = m
                              , sd = std))

cou  <- read.csv("countries.csv")
shapiro.test(cou$literacy)
m <- mean(cou$literacy, na.rm = TRUE)
std <- sd(cou$literacy, na.rm = TRUE)

ggplot(data = cou) +
  geom_histogram(aes(x = literacy, y = ..density..)
                 , fill = "red") +
  stat_function(fun = dnorm
                , args = list(mean = m
                              , sd = std))

mob <- read.csv("mobilenet.csv")
shapiro.test(mob$hours)
m <- mean(mob$hours)
std <- sd (mob$hours)

ggplot(data = mob) +
  geom_histogram(aes(x = hours, y = ..density..)
                 , fill = "red") +
  stat_function(fun = dnorm
                , args = list(mean = m
                              , sd = std))

zbalance <- scale(bank$balance, scale = TRUE)
which(abs(zbalance) > 3)

zlit <- scale(cou$literacy, scale = TRUE)
which(abs(zlit) > 3)

zhours <- scale(mob$hours, scale = TRUE)
which(abs(zhours) > 3)
cou$literacy


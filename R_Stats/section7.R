library(ggplot2)
library(lattice)
library(pspearman)
library(ppcor)
library(gmodels)
library(lsr)
hw <- read.csv("hw.csv")

# assumption checks
# variables normally distrib'd?
boxplot(hw$height)
boxplot(hw$weight)
shapiro.test(hw$height)
shapiro.test(hw$weight)
qqnorm(hw$height)
qqnorm(hw$weight)
hs <- scale(hw$height, scale = TRUE)
ws <- scale(hw$weight, scale = TRUE)
sort(hs, decreasing = TRUE)
sort(hs, decreasing = FALSE)
sort(ws, decreasing = TRUE)
sort(ws, decreasing = FALSE)

m <- mean(hw$height)
std <- sd(hw$height)

ggplot(data = hw) +
  geom_histogram(aes(x = height, y = ..density..)
                 , fill = "red") +
  stat_function(fun = dnorm
                , args = list(mean = m
                              , sd = std))

m <- mean(hw$weight)
std <- sd(hw$weight)

ggplot(data = hw) +
  geom_histogram(aes(x = weight, y = ..density..)
                 , fill = "red") +
  stat_function(fun = dnorm
                , args = list(mean = m
                              , sd = std))

hq <- data.frame(v1 = hs, att = "h")
wq <- data.frame(v1 = ws, att = "w")
q <- rbind(hq, wq)
q$att <- factor(q$att)
qq(att~v1, data = q)

model <- lm(weight~height, data = hw)
model

height <- c(min(hw$height), max(hw$height))
height

fit <- predict(model, data.frame(height))
endpoints <- data.frame(height, fit)

endpoints

ggplot() +
  geom_point(data = hw
             , aes(x = height
                   , y = weight
                   , shape = gender
                   , colour = gender)) +
  scale_x_continuous(limits = c(150, 195)) +
  geom_line(data = endpoints
            , aes(x = height
                  , y = fit)
            , color = "red"
            , size = 1.5)

cor.test(hw$height
         , hw$weight
         , method = "pearson"
         , alternative = "two.sided"
         , conf.level = 0.95)

cor.test(hw$height
         , hw$weight
         , method = "spearman"
         , alternative = "two.sided"
         , conf.level = 0.95
         , exact = FALSE)

spearman.test(hw$height
          , hw$weight
          , approximation = "AS89")

spearman.test(hw$height
              , hw$weight
              , approximation = "t-distribution")

cor.test(hw$height
         , hw$weight
         , method = "kendall"
         , alternative = "two.sided"
         , conf.level = 0.95
         , exact = FALSE)

ice <- read.csv("icecream.csv")
cor.test(ice$attacks
         , ice$icecream
         , method = "pearson"
         , alternative = "two.sided"
         , conf.level = 0.95)
# this is not logical

pcor.test(ice$attacks
          , ice$icecream
          , ice$temp
          , method = "pearson")

# chi square
bf <- read.csv("breakfast.csv")
table(bf)
CrossTable(bf$agecat, bf$bfast, expected = TRUE
           , prop.r = FALSE
           , prop.c = FALSE
           , prop.t = FALSE
           , prop.chisq = TRUE)

# if any expected vals are less than 5, run fisher exact.
fisher.test(bf$agecat, bf$bfast, simulate.p.value = TRUE)

# cramer's v - how strong is the association?
cramersV(bf$agecat, bf$bfast)

mnet <- read.csv("mobilenet.csv")
# assumption checks
# variables normally distrib'd?
boxplot(mnet$hours)
boxplot(mnet$income)
shapiro.test(mnet$hours)
shapiro.test(mnet$income)
qqnorm(mnet$hours)
qqnorm(mnet$income)
hh <- scale(mnet$hours, scale = TRUE)
ii <- scale(mnet$income, scale = TRUE)
sort(hh, decreasing = TRUE)
sort(hh, decreasing = FALSE)
sort(ii, decreasing = TRUE)
sort(ii, decreasing = FALSE)

# not normal and 1 extreme outlier in ii.

m <- mean(mnet$hours)
std <- sd(mnet$hours)

ggplot(data = mnet) +
  geom_histogram(aes(x = hours, y = ..density..)
                 , fill = "red") +
  stat_function(fun = dnorm
                , args = list(mean = m
                              , sd = std))

m <- mean(mnet$income)
std <- sd(mnet$income)

ggplot(data = mnet) +
  geom_histogram(aes(x = income, y = ..density..)
                 , fill = "red") +
  stat_function(fun = dnorm
                , args = list(mean = m
                              , sd = std))

hhq <- data.frame(v1 = hh, att = "h")
iiq <- data.frame(v1 = ii, att = "i")
q <- rbind(hhq, iiq)
q$att <- factor(q$att)
qq(att~v1, data = q)

model <- lm(hours~income, data = mnet)
model

inc <- c(min(mnet$income), max(mnet$income))
inc

fit <- predict(model, data.frame(income = inc))
endpoints <- data.frame(inc, fit)

endpoints

ggplot(data = mnet
       , aes(x = income
             , y = hours)) +
  geom_point() +
  # geom_smooth(data = mnet, method = "lm") +
    geom_line(data = endpoints
            , aes(x = inc
                  , y = fit)
            , color = "red"
            , size = 1.5)

# is the pearson robust to these assumption violations?
cor.test(mnet$hours
         , mnet$income
         , method = "pearson"
         , alternative = "two.sided"
         , conf.level = 0.95)
cor.test(mnet$hours
         , mnet$income
         , method = "spearman"
         , alternative = "two.sided"
         , conf.level = 0.95
         , exact = FALSE)
cor.test(mnet$hours
         , mnet$income
         , method = "kendall"
         , alternative = "two.sided"
         , conf.level = 0.95
         , exact = FALSE)

cou <- read.csv("countries.csv")
# assumption checks
# variables normally distrib'd?
boxplot(cou$urban)
boxplot(cou$gdp)
shapiro.test(cou$urban)
shapiro.test(cou$gdp)
qqnorm(cou$urban)
qqnorm(cou$gdp)
hh <- scale(cou$urban, scale = TRUE)
ii <- scale(cou$gdp, scale = TRUE)
sort(hh, decreasing = TRUE)
sort(hh, decreasing = FALSE)
sort(ii, decreasing = TRUE)
sort(ii, decreasing = FALSE)

# not normal and 1 extreme outlier in ii.

m <- mean(cou$urban, na.rm = TRUE)
std <- sd(cou$urban, na.rm = TRUE)

ggplot(data = cou) +
  geom_histogram(aes(x = urban, y = ..density..)
                 , fill = "red") +
  stat_function(fun = dnorm
                , args = list(mean = m
                              , sd = std))

m <- mean(cou$gdp, na.rm = TRUE)
std <- sd(cou$gdp, na.rm = TRUE)

ggplot(data = cou) +
  geom_histogram(aes(x = gdp, y = ..density..)
                 , fill = "red") +
  stat_function(fun = dnorm
                , args = list(mean = m
                              , sd = std))

hhq <- data.frame(v1 = hh, att = "h")
iiq <- data.frame(v1 = ii, att = "i")
q <- rbind(hhq, iiq)
q$att <- factor(q$att)
qq(att~v1, data = q)

model <- lm(gdp~urban, data = cou)
model

urb <- c(min(cou$urban, na.rm = TRUE), max(cou$urban, na.rm = TRUE))
urb

fit <- predict(model, data.frame(urban = urb))
endpoints <- data.frame(urb, fit)

endpoints

ggplot(data = cou
       , aes(x = urban
             , y = gdp)) +
  geom_point() +
  # geom_smooth(data = mnet, method = "lm") +
  geom_line(data = endpoints
            , aes(x = urb
                  , y = fit)
            , color = "red"
            , size = 1.5)

cou <- cou[complete.cases(cou), ]
cor.test(cou$urban
          , cou$gdp
          , method = "pearson")
cor.test(cou$urban
          , cou$gdp
          , method = "spearman"
         , exact = FALSE)
cor.test(cou$urban
          , cou$gdp
          , method = "kendall"
         , exact = FALSE)

pcor.test(cou$urban
          , cou$gdp
          , cou$literacy
          , method = "pearson")
pcor.test(cou$urban
          , cou$gdp
          , cou$literacy
          , method = "spearman")
pcor.test(cou$urban
          , cou$gdp
          , cou$literacy
          , method = "kendall")

mail <- read.csv("directmail.csv")
table(mail$income, mail$educ)
chisq.test(table(mail$income, mail$educ))
CrossTable(table(mail$income, mail$educ)
           , expected = TRUE
           , prop.c = FALSE
           , prop.r = FALSE
           , prop.t = FALSE)


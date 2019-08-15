library(ggplot2)
library(plyr)
demo <- read.csv("demographics.csv")

ggplot() +
  geom_histogram(data = demo
                 , aes(x=income)
                 , fill = "red"
                 , colour = "black")

ggplot() +
  geom_histogram(data = demo
                 , aes(x=income
                       , y=..density..)
                 , fill = "red"
                 , colour = "black") +
  facet_grid(gender~marital)

ggplot() +
  geom_histogram(data = demo
                 , aes(x=income
                       , y=..density..
                        , fill = gender)
                 , colour = "black")

mydata <- count(demo, 'income')
View(mydata)
cumul <- cumsum(mydata$freq)
cumperc <- cumul/nrow(demo)
mydata <- cbind(mydata, cumperc)

ggplot() +
  geom_line(data = mydata
            , aes(x = income
                  , y = cumperc))
ggplot() +
  geom_step(data = mydata
            , aes(x = income
                  , y = cumperc))

male <- demo[demo$gender == "Male",]
female <- demo[demo$gender == "Female",]

mydata_m <- count(male, 'income')
cumul_m <- cumsum(mydata_m$freq)
cumperc_m <- cumul_m/nrow(male)
mydata_m <- cbind(mydata_m, cumperc_m)
mydata_f <- count(female, 'income')
cumul_f <- cumsum(mydata_f$freq)
cumperc_f <- cumul_f/nrow(female)
mydata_f <- cbind(mydata_f, cumperc_f)

lgd <- scale_color_manual("Legend"
                          , values = c(Male = "blue"
                                       , Female = "pink"))
ggplot() +
  lgd +
  geom_line(data = mydata_m
            , aes(x = income
                  , y = cumperc_m
                  , colour = "Male")
            , size = 1.3) +
  geom_line(data = mydata_f
            , aes(x = income
                  , y = cumperc_f
                  , colour = "Female")
            , size = 1.3)

ggplot(data = demo
       , aes(x = educ
             , y = income
             , fill = educ)) +
  stat_summary(fun.y = mean
               , geom = "bar")
  
ggplot(data = demo
       , aes(x = educ
             , y = income)) +
  stat_summary(fun.y = mean
               , geom = "bar"
               , fill = "red")

ggplot(data = demo
       , aes(x = educ
             , y = income
             , fill = gender)) +
  stat_summary(fun.y = mean
               , geom = "bar"
               , position = "dodge")
ggplot(data = demo
       , aes(x = educ
             , y = income
             , fill = gender)) +
  stat_summary(fun.y = mean
               , geom = "bar"
               , position = "stack")

library(ggplot2)
hw <- read.csv("hw.csv")

ggplot() +
  geom_point(data = hw
             , aes(x = height
                   , y = weight
                   , shape = gender
                   , colour = gender)) +
  scale_x_continuous(limits = c(150, 195))

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

demo <- read.csv("demographics.csv")

ggplot() + geom_boxplot(data = demo
                        , aes(x = gender
                              , y = income
                              , fill = marital)
                        , outlier.colour = "red"
                        , outlier.shape = 4)


ggplot(data = hw, aes(x = height)) +
  geom_histogram(aes(y = ..density..)) +
  scale_x_continuous(limits = c(150, 195)) +
  stat_function(fun = dnorm
                , args = list(mean = mean(hw$height)
                              , sd = sd(hw$height)
                )
                , colour = "red")

ggplot(data = hw, aes(x = weight)) +
  geom_histogram(aes(y = ..density..)) +
  #scale_x_continuous(limits = c(150, 195)) +
  stat_function(fun = dnorm
                , args = list(mean = mean(hw$weight)
                              , sd = sd(hw$weight)
                )
                , colour = "red")

math <- read.csv("math.csv")
math <- math[order(math$grade1), ]
math$cumgrad1 <- cumsum(math$grade1)
math$grad1 <- math$cumgrad1/sum(math$grade1)
math$cumgrad2 <- cumsum(math$grade2)
math$grad2 <- math$cumgrad2/sum(math$grade2)
idx <- 1:nrow(math)

ggplot(data = math) + 
  geom_line(aes(x = idx
                , y = grad2))

mail <- read.csv("directmail.csv")
ggplot(data = mail) +
  geom_bar(aes(x = gender))
ggplot(data = mail) +
  geom_bar(aes(x = educ))

toyota <- read.csv("toyota.csv")
ggplot(data = toyota) +
  geom_bar(aes(x = continent))

mathmod <- lm(grade2~grade1, data = math)
summary(mathmod)
mnmx <- c(min(math$grade1), max(math$grade1))
fit <- predict(mathmod
               , newdata = data.frame(grade1 = mnmx))

mathendpoints <- data.frame(grade1 = mnmx, grade2 = fit)

ggplot(data = math) +
  geom_point(aes(x = grade1, y = grade2)) +
  geom_line(data = mathendpoints
            , aes(x = grade1, y = grade2)
            , colour = "red")

gym <- read.csv("gym.csv")
gymmod <- lm(romania~unitedstates, data = gym)
summary(gymmod)
mnmx <- c(min(gym$romania), max(gym$romania))
fit <- predict(gymmod
               , newdata = data.frame(unitedstates = mnmx))

gymendpoints <- data.frame(unitedstates = mnmx
                           , romania = fit)

ggplot(data = gym) +
  geom_point(aes(x = unitedstates, y = romania)) +
  geom_line(data = gymendpoints
            , aes(x = unitedstates, y = romania)
            , colour = "red")

mail <- read.csv("directmail.csv")

ggplot(data = mail) +
  geom_boxplot(aes(x = educ, y = age))

ggplot(data = mail) +
  geom_boxplot(aes(x = income
                   , y = age
                   , colour = gender)
               , position = "dodge")

ggplot(data = mail) +
  geom_histogram(aes(x = resid
                   , fill = gender)
               , position = "dodge")

ggplot(data = mail) +
  geom_boxplot(aes(x = gender
                   , y = resid
                   , fill = educ)
               , position = "dodge")

ggplot(data = mail) +
  geom_boxplot(aes(x = gender
                   , y = resid
                     , fill = educ
                      , colour = income)
                 , position = "dodge")

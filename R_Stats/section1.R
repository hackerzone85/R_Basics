demo <- read.csv("demographics.csv")
require(dplyr)
View(demo)

demo2 <-demo[demo$gender == "Female",]
View(demo2)

demo2 <- demo[demo$income > 100,]
View(demo2)

demo2 <-demo[demo$income > 100, c(1,3,7)]
View(demo2)

demo2 <-demo[demo$income > 100, -c(6:8)]
View(demo2)

demo2 <-demo[demo$gender == "Female" &
               demo$income > 100,]
View(demo2)

demo <-read.csv("demographics.csv")
View(demo)

demo2 <- filter(demo, marital == "Unmarried")
View(demo2)

demo2 <-filter(demo, marital == "Unmarried"
               , age < 50)
View(demo2)

demo2 <-select(demo, age, marital, income)
View(demo2)

demo2 <-filter(demo2
               , marital == "Unmarried"
               , age < 50)
View(demo2)

demo <-read.csv("demographics.csv")
View(demo)

demo$gender2[demo$gender == "Male"] = "1"
demo$gender2[demo$gender == "Female"] = "2"
View (demo)

require(plyr)
demo$gender3 = revalue(demo$gender
                       , c("Male"="1"
                           , "Female"="2"))
View(demo)

demo$gender2 = factor(demo$gender2)
demo$gender = revalue(demo$gender
                      , c("Male"="1"
                          , "Female"="2"))
View(demo)

demo <-read.csv("demographics.csv")
View(demo)

demo$incat[demo$income < 200] <- "Low Income"
demo$incat[demo$income >= 200] <- "High Income"
demo$incat2 <- cut(demo$income
                   , breaks = c(-Inf, 150, 300, Inf)
                   , labels = c("Low Income"
                                , "Medium Income"
                                , "High Income"
                                )
                   )
demo$incat3 <- cut(demo$income
                   , breaks = c(-Inf, 150, 300, Inf)
                   , labels = c("Low Income"
                                , "Medium Income"
                                , "High Income"
                                )
                   , right = FALSE
)

View(demo)

demo <-read.csv("demographics.csv")
demo2 <- demo[order(demo$income), ]
View(demo2)

demo3 <- demo[order(-demo$income), ]
View(demo3)

demo2 <- demo[order(demo$income, demo$age), ]
View(demo2)

demo3 <- demo[order(demo$income, -demo$age), ]
View(demo3)

math <- read.csv("math.csv")
View(math)

math$diff <- math$grade1 - math$grade2
math$avg <- (math$grade1 + math$grade2) / 2
View(math)

bl <- read.csv("bankloan.csv")
bl2 <- bl[bl$marital == "married", ]
bl3 <- filter(bl
              , education == "secondary" | 
                education == "tertiary")
View(bl3)

bl4 <- bl[bl$age > 40, ]
bl5 <- filter(bl, age < 35 & marital == "single")
bl6 <- bl[bl$loan == "yes" & bl$balance > 1000, ]
bl$maricat[bl$marital == "married"] <- 1
bl$maricat[bl$marital == "single"] <- 0
bl$loancat[bl$loan == "yes"] <- 1
bl$loancat[bl$loan == "no"] <- 0
bl$balcat <- cut(bl$balance
                   , breaks = c(-Inf, 500, 1000, Inf)
                   , labels = c("Low"
                                , "Medium"
                                , "High"
                   )
)

bl <- bl[order(bl$age), ]
View(bl)
bl <- bl[order(-bl$age, bl$balance), ]
View(bl)

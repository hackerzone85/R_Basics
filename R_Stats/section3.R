library(plyr)
library(gmodels)

demo <- read.csv("demographics.csv")
myTable <- table(demo$educ, exclude = NULL)
myTable

cumu <- cumsum(myTable)
cumu

rela <- prop.table(myTable)
rela

n <- nrow(demo)
cumu.freq <- cumu/n
cumu.freq

myTable2 <- cbind(Freq = myTable
                  , Cum = cumu
                  , Rel = rela
                  , CuRe = cumu.freq)
myTable2

# plyr
ptable <- count(demo, "educ")
ptable

ptable$perc <- ptable$freq/nrow(demo)
ptable

ptable$cum <- cumsum(ptable$freq)
ptable

ptable$cum.freq <- ptable$cum/nrow(demo)
ptable

ct <- xtabs(~gender+carcat, data = demo)
ftable(ct)

CrossTable(demo$gender, demo$carcat, prop.chisq = FALSE)
CrossTable(demo$gender, demo$carcat
           , digits = 2, expected = TRUE
           , prop.r = FALSE, prop.c = FALSE
           , chisq = TRUE, fisher = TRUE, mcnemar = FALSE
           , prop.chisq = FALSE, missing.include = FALSE)


mail <- read.csv("directmail.csv")
View(mail)
m1table <- table(mail$educ, exclude = NULL)
m1table
m2table <- table(mail$income, exclude = NULL)
m2table
m3table <- table(mail$age, exclude = NULL)
m3table

news <- read.csv("newspapers.csv")
View(news)

n1table <- count(news$age)
n1table

n2table <- count(news$political)
n2table

vita <- read.csv("vitamin3.csv")
View(vita)

v1 <- xtabs(~dose+gender, data = vita)
v1
v2 <- xtabs(~dose+type, data = vita)
v2
v3 <- xtabs(~gender+type, data = vita)
v3

CrossTable(vita$gender, vita$dose, prop.chisq = FALSE)
CrossTable(vita$dose, vita$type, prop.chisq = FALSE)
CrossTable(vita$gender, vita$type, prop.chisq = FALSE)

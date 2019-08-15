library(psy)
library(fmsb)
library(irr)

brd <- read.csv("brandsurvey.csv")
View(brd)

brd <- brd[, 1:5]
cronbach(brd)
CronbachAlpha(brd)

tch <- read.csv("teachers.csv")
View(tch)

# this doesn't give pval
ckappa(tch)
Kappa.test(tch$teacher1, tch$teacher2)

# kendall w for 3 or more raters
gym <- read.csv("gym.csv")
kendall(gym, correct = TRUE)


ju <- read.csv("juices.csv")
View(ju)
Kappa.test(ju$b6, ju$b7)

ma <- read.csv("math.csv")
kendall(ma, correct = TRUE)

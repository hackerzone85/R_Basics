library(vcd)
library(vcdExtra)
library(extracat)
library(MASS)
library(datasets)
hec <- margin.table(HairEyeColor, 2:1)
tile(hec)
fluctile(hec)

data("Mental", package = "vcdExtra")
mental <- xtabs(Freq ~ ses + mental, data = Mental)
spineplot(mental)
fluctile(hec)

Berkeley <- margin.table(UCBAdmissions, 2:1)
library(gmodels)
CrossTable(Berkeley, prop.chisq = FALSE, prop.c = FALSE, 
           format = "SPSS")

p <- c(0.05, .1, .25, .50, .75, .9, .95)
odds <- p / (1 - p)
logodds <- log(odds)
logits <- data.frame(p, odds, logodds)
logits

data("UCBAdmissions")
UCB <- margin.table(UCBAdmissions, 1:2)
(LOR <- loddsratio(UCB))
(OR <- loddsratio(UCB, log = FALSE))
summary(LOR)
confint(LOR)
confint(OR)
fisher.test(UCB)

data("Arthritis", package = "vcd")
Art <- xtabs(~ Treatment + Improved, data = Arthritis)
Art
round(100 * prop.table(Art, margin = 1), 2)
assocstats(Art)

Art2 <- xtabs(~ Treatment + Improved + Sex, data = Arthritis)
Art2
assocstats(Art2)
CMHtest(Art2)
# the test was more significant for female 
# because of the greater number of female in the data
apply(Art2, 3, sum)

data("Mental", package = "vcdExtra")
mental <- xtabs(Freq ~ ses + mental, data = Mental)
assocstats(mental)    # standard chisq tests
CMHtest(mental)       # CMH tests

# general association
cmhdemo1 <- read.table(header=TRUE, sep="", text="
                       b1  b2   b3  b4  b5
                       a1    0  15   25  15   0
                       a2    5  20    5  20   5
                       a3   20   5    5   5  20
                       ")
cmhdemo1 <- as.matrix(cmhdemo1)

# linear association
cmhdemo2 <- read.table(header=TRUE, sep="", text="
                       b1  b2   b3  b4  b5
                       a1    2   5    8   8   8
                       a2    2   8    8   8   5
                       a3    5   8    8   8   2
                       a4    8   8    8   5   2
                       ")

cmhdemo2 <- as.matrix(cmhdemo2)

CMHtest(cmhdemo1)
CMHtest(cmhdemo2)

sieve(cmhdemo1, shade=TRUE, main="General association",
  gp = shading_sieve(interpolate = 0, lty = c("solid", "longdash")))
sieve(cmhdemo2, shade=TRUE, main="Linear association",
  gp = shading_sieve(interpolate = 0, lty = c("solid", "longdash")))

# for 2x2xk tables, determine if the odds ratios differ across the strata
woolf_test(UCBAdmissions)
woolf_test(Art2)

# the loglinear model agrees with the woolf test on the art data
loglm(~ (Treatment + Improved + Sex)^2, data = Art2)
loglm(~ (Gender + Admit + Dept)^2, data = UCBAdmissions)


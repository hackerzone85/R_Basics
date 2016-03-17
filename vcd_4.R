library(vcd)
library(vcdExtra)
library(extracat)
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
fisher.test(UCB)

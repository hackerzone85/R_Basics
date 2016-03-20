cust.df <- read.csv("http://goo.gl/PmPkaG")
summary(cust.df)

spend.m1 <- lm(online.spend ~ .
               , data=subset(cust.df[ , -1], online.spend > 0))
summary(spend.m1)

# 98% variance explained? 
# standard error store.trans so high? 
# no pattern with online visits?

library(gpairs)
library(forecast) # Box Cox
gpairs(cust.df)

autoTransform <- function(x) {
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

# clean up complete cases only
cust.df.bc <- cust.df[complete.cases(cust.df), -1]
# online spend values greater than zero only
cust.df.bc <- subset(cust.df.bc, online.spend > 0)
# exclude email (yes/no factor)
numcols <- which(colnames(cust.df.bc) != "email")
# apply function
cust.df.bc[ , numcols] <- lapply(cust.df.bc[ , numcols], autoTransform)

summary(cust.df.bc)
gpairs(cust.df.bc)

spend.m2 <- lm(online.spend ~ .
               , data=cust.df.bc)
summary(spend.m2)

# this model still no better than one that predicts spend from transactions alone.
spend.m3 <- lm(online.spend ~ online.trans, data=cust.df.bc)
anova(spend.m3, spend.m2)

# collinear variables inflate one another's variance.
library(car)
vif(spend.m2)

# options
# 1. omit correlated variables
# 2. extract principle components or factors from correlated variables
# 3. use another approach to combine/transform variables (e.g. spend per transaction)
# 4. use a robust method such as RF

# option 1
spend.m4 <- lm(online.spend ~ . -online.trans -store.trans
               , data=cust.df.bc)
vif(spend.m4)
summary(spend.m4)

# option 2
pc.online <- prcomp(cust.df.bc[ , c("online.visits", "online.trans")])
cust.df.bc$online <- pc.online$x[ , 1]
pc.store <- prcomp(cust.df.bc[ , c("store.trans", "store.spend")])
cust.df.bc$store <- pc.store$x[ , 1]

spend.m5 <- lm(online.spend ~ email + age + credit.score +
                 distance.to.store + sat.service +
                 sat.selection + online + store
               , data=cust.df.bc)
vif(spend.m5)
summary(spend.m5)

# exploring logis
exp(0) / (exp(0) + 1) # equal probability

# computing logistic by hand; could use plogis()
plogis(-Inf) # infinitely low = likelihood 0
plogis(2) # moderate probability = 88% chance of outcome
plogis(-0.2) # weak likelihood

0.88 / (1-0.88) # odds
log(0.88 / (1-0.88)) # log odds
qlogis(0.88) # same

pass.df <- read.csv("http://goo.gl/J8MH6A")
pass.df$Promo <- factor(pass.df$Promo
                        , levels=c("NoBundle", "Bundle"))
summary(pass.df)
with(pass.df, table(Pass, Promo, Channel))
mosaic(pass.tab, shade = TRUE)
mosaic(aperm(pass.tab, c(1, 3, 2)), shade = TRUE)
doubledecker(table(pass.df))

# create the data by hand
pass.tab <- c(242, 639, 38, 359, 284, 27, 449, 223, 83, 278, 49, 485)
dim(pass.tab) <- c(3, 2, 2)
class(pass.tab) <- "table"
dimnames(pass.tab) <- list(Channel=c("Mail", "Park", "Email")
                           , Promo=c("Bundle", "NoBundle")
                           , Pass=c("YesPass", "NoPass") )
library(vcdExtra)
pass.df <- expand.dft(pass.tab)
str(pass.df)

pass.m1 <- glm(Pass ~ Promo, data=pass.df, family=binomial)
summary(pass.m1)
plogis(0.3888) / (1-plogis(0.3888))

pass.m2 <- glm(Pass ~ Promo + Channel
               , data=pass.df
               , family=binomial)
summary(pass.m2)
exp(coef(pass.m2))
exp(confint(pass.m2))

pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel
               , data=pass.df, family=binomial)
summary(pass.m3)
exp(coef(pass.m3))
exp(confint(pass.m3))

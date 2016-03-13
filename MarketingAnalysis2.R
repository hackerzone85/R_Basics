cust.df <- read.csv("http://goo.gl/PmPkaG")
ncust <- 1000
rnbinom(ncust, size=0.3
        , mu = 15 + 
          ifelse(cust.df$email=="yes", 15, 0) + 
          - 0.7 * (cust.df$age-median(cust.df$age)))

rnbinom(10, size = 0.3, mu = 15)
rbinom(10, size = 15, prob = 0.7)

str(cust.df)
plot(x=cust.df$age, y=cust.df$credit.score)

plot(cust.df$age, cust.df$credit.score, col="blue"
     , xlim=c(15, 55), ylim=c(500, 900)
     , main="Active Customers as of June 2014"
     , xlab="Customer Age (years)"
     , ylab="Customer Credit Score")
abline(h=mean(cust.df$credit.score)
       , col="dark blue", lty="dotted")
abline(v=mean(cust.df$age), col="dark blue"
       , lty="dotted")

plot(cust.df)

plot(cust.df$store.spend
     , cust.df$online.spend
     , main="Customers as of June 2014"
     , xlab="Prior 12 months in-store sales ($)"
     , ylab="Prior 12 months online sales ($)"
     , cex=0.7)

hist(cust.df$store.spend
     , breaks=(0:ceiling(max(cust.df$store.spend)/10))*10
     , main="Customers as of June 2014"
     , xlab="Prior 12 months store sales ($)"
     , ylab="Count of customers")

hist(cust.df$online.spend
     , breaks=(0:ceiling(max(cust.df$online.spend)/10))*10
     , main="Customers as of June 2014"
     , xlab="Prior 12 months online sales ($)"
     , ylab="Count of customers")

my.col <- c("black", "green3")
my.pch <- c(1, 19)

plot(cust.df$age, cust.df$credit.score
     , col=my.col[cust.df$email]
     #, pch=my.pch[cust.df$email]
     , xlim=c(15, 55), ylim=c(500, 900)
     , main="Active Customers as of June 2014"
     , xlab="Customer Age (years)"
     , ylab="Customer Credit Score")
abline(h=mean(cust.df$credit.score)
       , col="dark blue", lty="dotted")
abline(v=mean(cust.df$age), col="dark blue"
       , lty="dotted")
legend(x="topright"
       , legend=paste("email on file:"
                      , levels(cust.df$email))
       , col=my.col
       , pch=1#my.pch
       )

plot(cust.df$store.spend + 1, cust.df$online.spend + 1
     , log = "xy"
     , col=my.col[cust.df$email]
     #, pch=my.pch[cust.df$email]
     , main="Customers as of June 2014"
     , xlab="Prior 12 months in-store sales ($)"
     , ylab="Prior 12 months online sales ($)"
     , cex=0.7)

plot(cust.df$store.spend + 1 , cust.df$online.spend + 1 # can't have log 0
     , log = "xy"
     , col=my.col[cust.df$email]
     , main="Customers as of June 2014"
     , xlab="Prior 12 months in-store sales ($)"
     , ylab="Prior 12 months online sales ($)"
     , cex=0.7)

par(mfrow=c(2, 2))
plot(cust.df$distance.to.store, cust.df$store.spend
     , main="store")
plot(cust.df$distance.to.store, cust.df$online.spend
     , main="online")
plot(cust.df$distance.to.store, cust.df$store.spend+1
     , log="xy", main="store, log")
plot(cust.df$distance.to.store, cust.df$online.spend+1
     , log="xy", main="online, log")
par(mfrow=c(1, 1))

pairs(formula = ~ age + credit.score + email +
        distance.to.store + online.visits + 
        online.trans + online.spend + 
        store.trans + store.spend
      , data=cust.df)

library(car)
scatterplotMatrix(formula = ~ age + credit.score + email +
        distance.to.store + online.visits + 
        online.trans + online.spend + 
        store.trans + store.spend
      , data=cust.df, diagonal = "histogram")



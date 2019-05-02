library(ggplot2)
reps <- 50000
nexps <- 5
rate <- 0.1
set.seed(0)
system.time(
  x1 <- replicate(reps, sum(rexp(n=nexps, rate=rate)))
)

ggplot(data.frame(x1), aes(x1)) + 
  geom_histogram(aes(y=..density..)) +
  stat_function(fun=function(x)dgamma(x, shape=nexps, scale=1/rate),
                color="red", size=2)

set.seed(0)
system.time(x1 <- sapply(1:reps
  , function(i){sum(rexp(n=nexps, rate=rate))}))
set.seed(0)
system.time(x1 <- lapply(1:reps
  , function(i){sum(rexp(n=nexps, rate=rate))})) # list apply
set.seed(0)
system.time(x1 <- apply(matrix(rexp(n=nexps*reps, rate=rate)
                        , nrow=nexps),2,sum)) # apply on a matrix


sampa=rnorm(1000000,0,1)
sampb=rnorm(1500000,3,1)
combined = c(sampa, sampb)
plt = ggplot(data.frame(combined), aes(x=combined)) + stat_bin(binwidth=0.25, position="identity")
plt
pltd = ggplot(data.frame(combined)
              , aes(x=combined)) +
  stat_density(position="identity")
pltd


pop1=rnorm(2000000)
pop2=rnorm(1000000, 1, 2)
combined = c(pop1, pop2)
plt= ggplot(data.frame(data=c(combined, pop1, pop2)
                       , labels=rep(c("combined", "pop1", "pop2")
                                    , c(3e6, 2e6, 1e6)))
            , aes(x=data)) +
  stat_bin(aes(fill=labels)
            , position="identity"
           , binwidth=0.25, alpha=0.5) +
  theme_bw()
plt

library(MASS)
Sigma=matrix(c(5,3,3,2),2,2)
ex1=mvrnorm(10000,rep(0,2),Sigma)
Sigma=matrix(c(9,-5,-1,5),2,2)
ex2=mvrnorm(n=10000, rep(3, 2), Sigma)
d <- data.frame(rbind(ex1, ex2))
d$samp <- rep(c("ex1", "ex2"), rep(10000, 2))

g <- ggplot(data = d
  , aes(x=X1, y=X2, colour = samp)) +
  # geom_point() +
  geom_density2d() +
  theme_bw()
g

library(MASS)
library(cluster)
# Finding the 75% highest density / minimum volume ellipse
fit <- cov.mve(ex1
               , quantile.used = nrow(ex1) * 0.75)
points_in_ellipse <- ex1[fit$best, ]
ellipse_boundary <- predict(ellipsoidhull(points_in_ellipse))
# Plotting it
plot(ex1, col = rgb(0, 0, 0, alpha = 0.2))
lines(ellipse_boundary, col="lightgreen", lwd=3)
legend("topleft", "50%", col = "lightgreen", lty = 1, lwd = 3)

plot(ex1, col = rgb(0, 0, 0, alpha = 0.2))
for(coverage in c(0.95, 0.75, 0.5)) {
  fit <- cov.mve(ex1, quantile.used = nrow(ex1) * coverage)
  ellipse_boundary <- predict(ellipsoidhull(ex1[fit$best, ]))
  polygon(ellipse_boundary
          , col = sample(colors(), 1)
          , border = NA)
}

qplot(ex2[, 1], ex2[, 2], geom = "hex")
qplot(d[, 1], d[, 2], geom = "hex")

library(BayesianFirstAid)
fit <- bayes.cor.test(ex2[, 1], ex2[, 2], na.rm = TRUE)
plot(fit)
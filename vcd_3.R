library(vcd)
library(vcdExtra)
# distributions
set.seed(12345)
k <- 0:12
Pk <- dbinom(k, 12, 1/3)
b <- barplot(Pk, names.arg = k
             , xlab = "number of successes"
             , ylab = "probability")
lines(x = b, y = Pk, col = "red")

Weldon_df <- as.data.frame(WeldonDice)
# collapse last three cols of simulated set
Pk <- c(Pk[1:10], sum(Pk[11:13]))
Exp <- round(26306 * Pk, 5) # expected
Diff <- Weldon_df$Freq - Exp
Chisq <- Diff^2/Exp
data.frame(Weldon_df, prob = round(Pk, 5), Exp, Diff, Chisq)

b2 <- barplot(Weldon_df$Freq)
lines(x = b2, y = Exp, col = "red")

p <- c(1/6, 1/3, 1/2, 2/3)
k <- 0:12
Prob <- outer(k, p, function(k,p) { dbinom(k, 12, p) })
str(Prob)

col <- palette()[2:5]
matplot(k, Prob, type = "o", pch = 15:17, col = col
        , xlab = "number of Successes", ylab = "Probability")
legend("topright", legend = as.character(p), pch = 15:17
       , lty = 1, col = col, title = "Pr(Success")

data("UKSoccer", package = "vcd")

soccer.df <- as.data.frame(UKSoccer, stringsAsFactors = FALSE)
soccer.df <- within(soccer.df, {
  Home <- as.numeric(Home)       # make numeric
  Away <- as.numeric(Away)       # make numeric
  Total <- Home + Away           # total goals
})
str(soccer.df)

soccer.df <- expand.dft(soccer.df)   # expand to ungrouped form
apply(soccer.df, 2, FUN = function(x) c(mean = mean(x), var = var(x)))


with(CyclingDeaths, c(mean = mean(deaths),
                      var = var(deaths),
                      ratio = mean(deaths) / var(deaths)))

mean.deaths <- mean(CyclingDeaths$deaths)
ppois(5, mean.deaths, lower.tail = FALSE)

KL <- expand.grid(k = 0 : 20, lambda = c(1, 3, 7, 10, 15, 20))
pois_df <- data.frame(KL, prob = dpois(KL$k, KL$lambda))
pois_df$lambda = factor(pois_df$lambda)
str(pois_df)

library(lattice)
library(directlabels)
xyplot(prob ~ k | lambda, data = pois_df,
       type = c("h", "p"), pch = 16, lwd = 2, cex = 1.25, layout = c(3, 2),
       xlab = list("Number of events (k)", cex = 1.25),
       ylab = list("Probability", cex = 1.25))

# use direct labels as an alternative way to legend a plot (lattice)
mycol <- palette()[2:7]
plt <- xyplot(prob ~ k, data = pois_df, groups = lambda,
              type = "b", pch = 15 : 17, lwd = 2, cex = 1.25, col = mycol,
              xlab = list("Number of events (k)", cex = 1.25),
              ylab = list("Probability",  cex = 1.25),
              ylim = c(0, 0.4))

library(directlabels)
direct.label(plt, list("top.points", cex = 1.5, dl.trans(y = y + 0.1)))

# ggplot alternative
library(ggplot2)
gplt <- ggplot(pois_df,
               aes(x = k, y = prob, colour = lambda, shape = lambda)) +
  geom_line(size = 1) + geom_point(size = 3) +
  xlab("Number of events (k)") +
  ylab("Probability")

gplt + theme(legend.position = c(0.9, 0.7)) +  # manually move legend
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

# negative binom
k <- 2
n <- 2 : 4
p <- 0.2
dnbinom(k, n, p)

(mu <- n * (1 - p) / p)
dnbinom(k, n, mu = mu)

XN <- expand.grid(k = 0 : 20, n = c(2, 4, 6), p = c(0.2, 0.3, 0.4))
nbin_df <- data.frame(XN, prob = dnbinom(XN$k, XN$n, XN$p))
nbin_df$n <- factor(nbin_df$n)
nbin_df$p <- factor(nbin_df$p)
str(nbin_df)

xyplot(prob ~ k | n + p, data = nbin_df,
       xlab = list("Number of failures (k)", cex = 1.25),
       ylab = list("Probability",  cex = 1.25),
       type = c("h", "p"), pch = 16, lwd = 2,
       strip = strip.custom(strip.names = TRUE)
)
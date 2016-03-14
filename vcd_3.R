library(vcd)
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

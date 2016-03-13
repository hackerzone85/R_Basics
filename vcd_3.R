library(vcd)
# distributions
k <- 0:12
Pk <- dbinom(k, 12, 1/3)
b <- barplot(Pk, names.arg = k
             , xlab = "number of successes"
             , ylab = "probability")
lines(x = b, y = Pk, col = "red")
x1 <- rnorm(100, mean = 10, sd = 5)
x2 <- rpois(100, 1) + 1.2 * x1
x3 <- runif(100, -0.1, 0.5) + rnorm(100, mean = -1:1, sd = 10)
noise <- rnorm(100, sd = 25)
y <- 10 + 1.5 * x1 + (-1.2) * x2 + 0.1 * x3 + noise

df <- data.frame(y, x1, x2, x3)

xyplot(y ~ x1, data = df, cex = x2/3
       , panel = function(x,y, ...) {
         panel.xyplot(x,y, ...)
         panel.lmline(x,y)
         
       })

summary(lm(y ~ x1 * x2 + x3, data = df))
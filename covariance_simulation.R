n <- 100
plot(c(1, n), 0:1, type = "n"
      , frame = FALSE
      , xlab = "p"
      , ylab = "R ^ 2") 
y <- rnorm(n); 
x <- NULL; 
r <- NULL 
for (i in 1:n){
  x <- cbind(x,rnorm(n)) 
  r <- c(r, summary(lm( y ~ x))$r.squared)
}
lines( 1 : n, r, lwd = 3) 
abline(h = 1)

n <- 100
nosim <- 1000 
x1 <- rnorm(n)
# x2 <- rnorm(n)
# x3 <- rnorm(n)
x2 <- x1/ sqrt( 2) + rnorm( n) /sqrt( 2)
x3 <- x1 * 0.95 + rnorm( n) * sqrt( 1 - 0.95 ^ 2)
betas <- sapply(1:nosim
                , function(i) {
                  y <- x1 + rnorm( n, sd = .3)
                  c(coef(lm(y ~ x1))[2]
                    , coef(lm(y ~ x1 + x2))[2]
                    , coef(lm(y ~ x1 + x2 + x3))[2])
                  })
round(apply(betas, 1, var), 5)


y <- x1 + rnorm(n, sd = .3)
a <- summary(lm(y ~ x1))$cov.unscaled[2,2]
c(summary(lm(y ~ x1 + x2))$cov.unscaled[2,2]
  , summary(lm(y ~ x1 + x2 + x3))$cov.unscaled[2,2])/a
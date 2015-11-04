icecream <- data.frame(
  temp=c(11.9, 14.2, 15.2, 16.4, 17.2, 18.1, 
         18.5, 19.4, 22.1, 22.6, 23.4, 25.1),
  units=c(185L, 215L, 332L, 325L, 408L, 421L, 
          406L, 412L, 522L, 445L, 544L, 614L)
)

basicPlot <- function(...){
  plot(units ~ temp, data=icecream, bty="n", lwd=2,
       main="Number of ice creams sold", col="#00526D", 
       xlab="Temperature (Celsius)", 
       ylab="Units sold", ...)
  axis(side = 1, col="grey")
  axis(side = 2, col="grey")
}

par(mfrow = c(1,1))
basicPlot()

# least square fit, linear regression model
lsq.mod <- lsfit(icecream$temp, icecream$units)
basicPlot()
abline(lsq.mod, col="orange", lwd=2)
legend(x="topleft", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "linear least square"),
       col=c("#00526D","orange"),  pch=c(1,NA))

fit <- lm(icecream$units ~ icecream$temp) # equivalent to lsfit above
plot(fit)


icecream$dev <- 1:6
icecream$origin <- rep(c("Wall's", "B&J"), each=6)

library(lattice)
xyplot(units/100 + log(temp) ~ dev, groups=origin, data=icecream, 
       t="b", par.settings = 
         list(simpleTheme(pch = 16), strip.background = list(col="pink")),
       auto.key = list(space="right",
                       title="Brand", cex.title=0.8,
                       points=FALSE, lines=TRUE, type="b"),
       xlab="Imaginary Units", ylab="Amount",
       main="Ice cream sales by imaginary units",
       scales= "free"
)

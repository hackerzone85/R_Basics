newmt<- seq(min(mtcars$hp), max(mtcars$hp), length.out = 100)
prc <- data.frame(predict(lm3, newdata = data.frame(hp = newmt), interval = "confidence"))
prc2 <- data.frame(predict(lm3, newdata = data.frame(hp = newmt), interval = "prediction"))
prc$interval <- "confidence"
prc2$interval <- "prediction"
prr <- rbind(prc, prc2)
prr$x <- newmt
g <- ggplot(data = prr, aes(x = x, y = fit))
g <- g + geom_line()
g <- g + geom_point(data = mtcars, aes(x = hp, y = mpg))
g + geom_ribbon(aes(ymin = lwr
                    , ymax = upr
                    , fill = interval), alpha = 0.2)
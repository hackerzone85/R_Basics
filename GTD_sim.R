set.seed(102)
n <- 46
yr <- 0:45
intext <- c("No", "Yes")
sim.grid <- expand.grid(yr, intext)
ninc <- rpois(n, round(seq(200, 1, length.out = n),0))
yinc <- rpois(n, c(round(seq(4, 20, length.out = n/2),0)
                   , rev(round(seq(0, 20, length.out = n/2),0))))

sim.data <- cbind (sim.grid, inc = c(ninc, yinc))
sim.data <- expand.dft(sim.data, freq = "inc")

nincs <- nrow(sim.data)
fats <- round(runif(nincs
                    , 0
                    , c(round(rexp(sum(ninc)
                    , 1/seq(50,1
                    , length.out = sum(ninc)))
                            ,0)
                    , round(rexp(sum(yinc)
                    , 1/seq(1,50
                    , length.out = sum(yinc)))
                            ,0))
              ))
sim.data <- cbind(sim.data, fats)
names(sim.data) <- c("Year", "intext", "fats")
sim.data <- within(sim.data, {
  intext <- factor(intext, levels = c("No", "Yes"))
  intCols <- ifelse(intext == "No", "violetred1", "steelblue")
})

sim.glm <- glm(fats~poly(Year,4)*intext, data = sim.data
               , family = poisson)
anova(sim.glm, test = "Chisq")

names(sim.grid) <- c("Year", "intext")
sim.pred <- cbind(sim.grid
                  , pred = predict(sim.glm, sim.grid
                                   , type = "response"
                                   , se = TRUE))
sim.pred <- within(sim.pred, {
  ci.lwr <- pred.fit - 1.96 * pred.se.fit
  ci.upr <- pred.fit + 1.96 * pred.se.fit
  intext <- factor(intext, levels = c("No", "Yes"))
  intCols <- ifelse(intext == "No", "violetred1", "steelblue")
})

plot(fats~Year, data = sim.data, col = intCols, pch = "."
     , ylim = c(0, 50))
with(sim.pred[sim.pred$intext == "No",]
     , polygon(c(Year, rev(Year))
               , c(ci.upr, rev(ci.lwr))
        , col=rgb(1,0,0,0.7), border=NA))
lines(pred.fit~Year, data = sim.pred
      , col = intCols, subset = (intext == "No"))
with(sim.pred[sim.pred$intext == "Yes",]
     , polygon(c(Year, rev(Year))
               , c(ci.upr, rev(ci.lwr))
               , col=rgb(0,0,1,0.2), border=NA))
lines(pred.fit~Year, data = sim.pred
      , col = intCols, subset = (intext == "Yes"))


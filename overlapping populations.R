library(lattice)


# density plots
chinese <- rnorm(1000, 105, 25)
caucasian <- rnorm(1000, 100, 25)
labels <- rep(c("chinese", "caucasian"), 1000)

dt <- data.frame(race = labels
                 , iq = c(chinese, caucasian))

densityplot(~iq, data = dt, groups = race, auto.key = TRUE)

n <- 1000000
chinese <- rnorm(n, 105, 10)
caucasian <- rnorm(n, 100, 10)

chin <- sample(chinese
               , size = n
               , replace = TRUE)
cauc <- sample(caucasian
               , size = n
               , replace = TRUE)
  
sims <- ifelse(chin > cauc, 1, 0)
mean(sims)

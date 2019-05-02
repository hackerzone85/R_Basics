library(dplyr)
# create some simulation data
x <- rnorm(100, mean = rep(1:5, each = 20))
z <- factor(rep(c("M", "F"), each = 50), levels = c("M", "F"))
y <- x * (as.numeric(z) + 0.1) * rnorm(100, mean = rep(1:5, each = 20), sd = seq(1, 10, by = 2))
dataM <- data.frame(x=x, y=y) #%>% data.matrix

dm <- dataM[z == "M", 1:2]
df <- dataM[z == "F", 1:2]

plot(x,y, type = "n")
points(dm$x, dm$y, col = "blue", pch = 19)
points(df$x, df$y, col = "red", pch = 19)
abline(lm(dm$y~dm$x), col = "blue")
abline(lm(df$y~df$x), col = "red")

dataM <- data.frame(x=x, y=y) %>% data.matrix
image(dataM)
heatmap(dataM)

set.seed(12345)
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(dataMatrix)
heatmap(dataMatrix)
set.seed(678910)
for(i in 1:40) {
  coinflip <- rbinom(1, size = 1, prob = 0.5)
  if(coinflip) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,3), each = 5)
  }
}
image(dataMatrix)
heatmap(dataMatrix)



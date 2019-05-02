library(tidyverse)
library(vcd)
library(vcdExtra)

Test <- tribble(
  ~Species, ~Value, ~Habitat,
  "Giraffes", 20, "Wildlife",
  "Orangutans", 14, "Wildlife",
  "Monkeys", 23, "Wildlife",
  "Giraffes", 15, "ZOO",
  "Orangutans", 9, "ZOO",
  "Monkeys", 18, "ZOO")

glm1 <- glm(Freq~., data = Test, family="poisson")
summary(glm1)
glm2 <- glm(Freq~Habitat, data = Test, family="poisson")
summary(glm2)

Test <- table(expand.dft(as.data.frame(Test)))
chisq.test(Test)

plot(loddsratio(t(Test)))


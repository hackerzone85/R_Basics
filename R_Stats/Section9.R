library(corrplot)
library(car)
library(lmtest)
library(usd)
library(ggplot2)

stud <- read.csv("students.csv")
View(stud)
cor(stud[, -4])
corrplot(cor(stud[, -4]))

plot(score~iq, data = stud)
abline(lm(score~iq, data = stud))

plot(score~hours, data = stud)
abline(lm(score~hours, data = stud))

fit <- lm(score~iq+hours, data = stud)
summary(fit)

# look for outliers - there is only one on a big sample.
res <- fit$residuals
zres <- scale(res)
head(sort(zres))
tail(sort(zres))

# no autocorrelation - accept the null h
durbinWatsonTest(fit)
dwtest(fit)

# variance inflation should be lower than 10
vif(fit)

# homoskedastic?
plot(fit)

rezz <- data.frame(fitted = fit$fitted.values
                   , resids = fit$residuals)
ggplot(data = rezz, aes(x = fitted, y = resids)) +
  geom_point()

# resids are normal?
shapiro.test(rezz$resids)
ggplot(data = rezz, aes(x = resids)) +
  geom_density()

# male 0, female 1
fit <- lm(score~iq+hours+gender, data = stud)
summary(fit)

# look for outliers - there is only one on a big sample.
res <- fit$residuals
zres <- scale(res)
head(sort(zres))
tail(sort(zres))

# no autocorrelation - accept the null h
durbinWatsonTest(fit)
dwtest(fit)

# variance inflation should be lower than 10
vif(fit)

# homoskedastic?
plot(fit)

rezz <- data.frame(fitted = fit$fitted.values
                   , resids = fit$residuals)
ggplot(data = rezz, aes(x = fitted, y = resids)) +
  geom_point()

# resids are normal?
shapiro.test(rezz$resids)
ggplot(data = rezz, aes(x = resids)) +
  geom_density()

fit1 <- lm(score~iq, data = stud)
fit2 <- lm(score~iq+hours, data = stud)
fit3 <- lm(score~iq+hours+gender, data = stud)

summary(fit1)
summary(fit2)
summary(fit3)

anova(fit1, fit2, fit3)

veh <- read.csv("vehicles.csv")
# engine, horsepow, wheelbas, width
# length, weight  fuelcap, type
View(veh)
corrplot(cor(veh[complete.cases(veh), -2]))

plot(price~engine, data = veh)
abline(lm(price~engine, data = veh))
plot(price~horsepow, data = veh)
abline(lm(price~horsepow, data = veh))
plot(price~wheelbas, data = veh)
abline(lm(price~wheelbas, data = veh))
plot(price~width, data = veh)
abline(lm(price~width, data = veh))
plot(price~length, data = veh)
abline(lm(price~length, data = veh))
plot(price~width, data = veh)
abline(lm(price~width, data = veh))
plot(price~fuelcap, data = veh)
abline(lm(price~fuelcap, data = veh))

fit <- lm(price~engine+horsepow+wheelbas+width+
          length+weight+fuelcap+type, data = veh)
summary(fit)

# look for outliers - there is only one on a big sample.
res <- fit$residuals
zres <- scale(res)
head(sort(zres))
tail(sort(zres)) # problem with outliers

# no autocorrelation - accept the null h
durbinWatsonTest(fit)
dwtest(fit)

# variance inflation should be lower than 10
vif(fit)

# homoskedastic?
plot(fit)

rezz <- data.frame(fitted = fit$fitted.values
                   , resids = fit$residuals)
ggplot(data = rezz, aes(x = fitted, y = resids)) +
  geom_point()

# resids are normal?
shapiro.test(rezz$resids)
ggplot(data = rezz, aes(x = resids)) +
  geom_density()

veh <- veh[complete.cases(veh), ]
fit1 <- lm(price~engine+horsepow+type, data = veh)
fit2 <- lm(price~engine+horsepow+wheelbas+width+
             length+weight+fuelcap+type, data = veh)

summary(fit1)
summary(fit2)

anova(fit1, fit2)

loan <- read.csv("bankloan.csv")
fit1 <- lm(balance~age, data = loan)
summary(fit1)
fit2 <- lm(balance~age+loan, data = loan)
summary(fit2)
anova(fit1, fit2)

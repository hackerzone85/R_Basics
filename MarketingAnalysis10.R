cbc.df <- read.csv("http://goo.gl/5xQObB"
                   , colClasses = c(seat = "factor", price = "factor"))
summary(cbc.df)
# building the sim data by hand
attrib <- list(seat = c("6", "7", "8")
               , cargo = c("2ft", "3ft")
               , eng = c("gas", "hyb", "elec")
               , price = c("30", "35", "40"))
# creating part worths
coef.names <- NULL
for (a in seq_along(attrib)) {
  coef.names <- c(coef.names
                  , paste(names(attrib)[a]
                          , attrib[[a]][-1], sep=""))
}
coef.names
mu <- c(-1, -1, 0.5, -1, -2, -1, -2) 
names(mu) <- coef.names
mu
Sigma <- diag(c(0.3, 1, 0.1, 0.3, 1, 0.2, 0.3))
dimnames(Sigma) <- list(coef.names, coef.names)
Sigma["enghyb", "engelec"] <- Sigma["engelec", "enghyb"] <- 0.3

set.seed(33040)
resp.id <- 1:200 # respondent ids
carpool <- sample(c("yes", "no")
                  , size=length(resp.id)
                  , replace=TRUE
                  , prob=c(0.3, 0.7))
library(MASS)
coefs <- mvrnorm(length(resp.id)
                 , mu=mu, Sigma=Sigma)
colnames(coefs) <- coef.names
coefs[carpool=="yes", "seat8"] <- coefs[carpool=="yes", "seat8"] + 2
coefs[carpool=="yes", "seat7"] <- coefs[carpool=="yes", "seat7"] + 1.5
head(cbind(carpool, coefs))
# the above creates the coefs (model) of the respondents' preferences
# it's not the final model

nques <- 15
nalt <- 3
profiles <- expand.grid(attrib)
nrow(profiles)
head(profiles)
# dummy var scheme for the attribs
profiles.coded <- model.matrix(~seat + cargo + eng + price
                               , data=profiles)[ , -1]
head(profiles.coded)

cbc.df <- data.frame(NULL)
for (i in seq_along(resp.id)) {
  profiles.i <- sample(1:nrow(profiles), size=nques*nalt)
  utility <- profiles.coded[profiles.i, ] %*% coefs[i, ]
  wide.util <- matrix(data=utility, ncol=nalt, byrow=TRUE)
  probs <- exp(wide.util) / rowSums(exp(wide.util))
  choice <- apply(probs, 1, function(x) sample(1:nalt, size=1, prob=x))
  choice <- rep(choice, each=nalt)==rep(1:nalt, nques)
  conjoint.i <- data.frame(resp.id=rep(i, nques)
                           , ques = rep(1:nques, each=nalt)
                           , alt = rep(1:nalt, nques)
                           , carpool = rep(carpool[i], nques)
                           , profiles[profiles.i, ]
                           , choice = as.numeric(choice))
  cbc.df <- rbind(cbc.df, conjoint.i)
}
# Tidy up, keeping only cbc.df and attrib
rm(a, i, resp.id, carpool, mu, Sigma, coefs, coef.names
 , conjoint.i, profiles, profiles.i, profiles.coded, utility
 , wide.util, probs, choice, nalt, nques)
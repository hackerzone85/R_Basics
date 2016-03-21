library(lavaan)
library(semTools)
library(semPlot)
piesSimData <- read.csv("http://goo.gl/yT0XwJ")
summary(piesSimData)

piesModel <- "General =~ i1 + i2 + i3
Feature =~ i4 + i5 + i6 + i7
Image =~ i8 + i9 + i10 + i11
PIES =~ General + Feature + Image"

piesDataModel <- "General =~ 0.9*i1 + 0.7*i2 + 0.5*i3
Feature =~ 0.3*i3 + 0.7*i4 + 0.9*i5 + 0.5*i6 + 0.9*i7
Image =~ 0.2*i3 + 0.8*i8 + 0.9*i9 + 0.8*i10 + 0.7*i11
PIES =~ 0.7*General + 0.8*Feature + 0.8*Image"

set.seed(10001)
piesSimData.norm <- simulateData(piesDataModel, sample.nobs=3600)
print(head(piesSimData.norm), digits=2)
# convert the continuous data points to discrete.
piesSimData <- data.frame(
  lapply(
    piesSimData.norm
    , function(x) { cut(x, breaks=7, labels=FALSE) }
    )
  )

library(psych)
describe(piesSimData)
library(car)
library(RColorBrewer)
scatterplotMatrix(piesSimData[, c(1, 2, 4, 5, 8, 9)], diag="histogram"
                  , col=brewer.pal(3, "Paired"), ellipse=TRUE )

pies.fit <- cfa(piesModel, data=piesSimData)
summary(pies.fit, fit.measures=TRUE)

semPaths(pies.fit, what="est", fade=FALSE, residuals=FALSE
         , edge.label.cex=0.75)

piesModelNH1 <- " PIES =~ i1 + i2 + i3 + i4 + i5 + i6 +
i7 + i8 + i9 + i10 + i11 "
pies.fit.NH1 <- cfa(piesModelNH1, data=piesSimData)

piesModelNH3 <- "General =~ i1 + i2 + i3
Feature =~ i4 + i5 + i6 + i7
Image =~ i8 + i9 + i10 + i11
General ~~ 0.1*Feature
General ~~ 0.1*Image
Feature ~~ 0.1*Image"
pies.fit.NH3 <- cfa(piesModelNH3, data=piesSimData)

compareFit(pies.fit.NH1, pies.fit.NH3, pies.fit)

# a more complicated example
satSimData <- read.csv("http://goo.gl/MhghRq")
summary(satSimData)

# or simulate
satModel <- "Quality =~ CSat + Value + q1 + q2 + q3 + 0*Cost
Cost =~ Value + Repeat + c1 + c2 + c3
Value =~ CSat + v1 + v2 + v3
CSat =~ Repeat + cs1 + cs2 + cs3
Repeat =~ r1 + r2 + r3"

satDataModel <- "Quality =~ 0.59*CSat + 0.56*Value +
0.9*q1 + 0.9*q2 + 0.9*q3 + 0*Cost
Cost =~ -0.5*Value + -0.29*Repeat +
0.9*c1 + 0.9*c2 + 0.9*c3
Value =~ 0.06*CSat + 0.9*v1 + 0.9*v2 + 0.9*v3
CSat =~ 0.48*Repeat + 0.9*cs1 + 0.9*cs2 + 0.9*cs3
Repeat =~ 0.9*r1 + 0.9*r2 + 0.9*r3"

set.seed(33706)
satData.norm <- simulateData(satDataModel, sample.nobs=200)
satSimData <- data.frame(lapply(satData.norm
                                , function(x) { as.numeric(cut(x, breaks=7)) } ))

sat.fit <- sem(satModel, data= satSimData, std.lv=TRUE)
summary(sat.fit, fit.measures=TRUE)

semPaths(sat.fit, what="est", fade=FALSE, residuals=FALSE
         , layout="tree", structural=TRUE, nCharNodes=7, edge.label.cex=1)

satAltModel <- "Quality =~ CSat + q1 + q2 + q3 + 0*Cost
Cost =~ Value + c1 + c2 + c3
Value =~ CSat + v1 + v2 + v3
CSat =~ Repeat + cs1 + cs2 + cs3
Repeat =~ r1 + r2 + r3"

satAlt.fit <- sem(satAltModel, data=satSimData, std.lv=TRUE)
compareFit(sat.fit, satAlt.fit, nested=TRUE)
library(lattice)
library(ggplot2)
library(plyr)

# coursera plotting example
# load Source Classification Code (SCC) Table
SCC <- readRDS("Source_Classification_Code.rds")
# load National Emissions Inventory (NEI) data; slow!
NEI <- readRDS("summarySCC_PM25.rds")


totalNEI<-tapply(NEI$Emissions, INDEX=NEI$year, sum)     #Sum emissions per year
barplot(totalNEI, main = "Total Emissions by Year", xlab="Year", ylab="Emissions")

baltimore<-subset(NEI, NEI$fips==24510)           #Subset Baltimore area
totalBaltimore<-tapply(baltimore$Emissions, INDEX=baltimore$year, sum)   #Sum emissions per year
barplot(totalBaltimore, main="Total Emissions in Baltimore, MD by Year", xlab="Year", ylab="Emissions")

library(ggplot2)
ggplot(data=baltimore, aes(x=year, y=Emissions, fill=type)) +    
  geom_bar(stat="identity", position="dodge") +
  ggtitle("Baltimore, MD Emission by Type: 1999-2008")


# abalone examples
aburl = 'http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'
abnames = c('sex','length','diameter','height','weight.w','weight.s','weight.v','weight.sh','rings')
abalone = read.table(aburl, header = F , sep = ',', col.names = abnames)

# or

abalone <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", 
                    header = FALSE)
names(abalone) <- c("sex", "length", "diameter", "height", "weight.whole", "weight.shucked", 
                    "weight.viscera", "weight.shell", "rings")

# for a discriminated colour plot
# create a colour gradient with rampPalette
color1 <- colorRampPalette(c("wheat", "blue"))
grad1 <- color1(10) # 10 colours

# cut the range to deciles, 10 bands
range(abalone$length)
cutpoints <- seq(0.075, 0.815, length.out = 11)
length.dq <- cut(abalone$length, cutpoints)

# map the colour gradient to the deciles and plot
length.color <- grad1[length.dq]
plot(abalone$length, abalone$diameter, pch = 19, col = length.color)


# for a smooth colour plot
# create a colour gradient with ramp, and a ramping function to output the hex colours correctly
rmp <- colorRamp(c("wheat", "blue"))

makeGrad <- function(a) {
  a1 <- a[1]
  a2 <- a[2]
  a3 <- a[3]
  rgb(a1, a2, a3, maxColorValue = 255)
}

# calculate the precise colour for each point and plot
length.color2 <- c()
mx <- max(abalone$length) # scale the domain to fit ramp (zero to one)
for (i in 1:length(abalone$length)) {
  length.color2[i] <- makeGrad(rmp(abalone$length[i]/mx))
}
plot(abalone$length, abalone$diameter, pch = 19, col = length.color2)


# list of gender indices, using which()
grps <- list()
for (gen in c("M", "F", "I")) grps[[gen]] <- which(abalone$sex == gen)

abam <- abalone[grps$M,]
abaf <- abalone[grps$F,]
abai <- abalone[grps$I,]

plot(abam$length, abam$diameter)
points(abaf$length, abaf$diameter, pch = "x")

xyplot(diameter~length | sex, data = abalone)

xyplot(diameter~length | sex, data = abalone
      , layout = c(3, 1)
      , index.cond=list(c(3, 1, 2))
      , fill.color = abalone$sex
      , panel = function(x, y, fill.color, subscripts) {
        fill = fill.color [subscripts]
        panel.xyplot(x, y ,pch=19, col=fill)
        lm1 <- lm(y ~ x)
        panel.abline(a = lm1$coefficients[1], 
                     b = lm1$coefficients[2])
      })


ggplot(abalone) + aes(rings, color = sex) + geom_density()

ggplot(abalone) + aes(rings, fill = sex) + geom_density(position = "stack")

ggplot(abalone) + 
  aes(length, rings, color = sex) + 
  geom_point() + 
  labs(x = "Shell Length", y = "Number of Rings"
       , title = "Number of Rings vs Length"
       , color = "Sex of Abalone") + 
  theme(legend.position = c(0, 1)
        , legend.justification = c(0, 1)
        , legend.background = element_rect(fill = "white", color = "black")) + 
  scale_color_hue(labels = c("Female", "Infant", "Male"))


ggplot(abalone) + 
  aes(length, rings, color = sex) + 
  geom_point() + 
  labs(x = "Shell Length", y = "Number of Rings"
       , title = "Number of Rings vs Length") + 
  facet_grid(.~sex, labeller = label_both) + 
  stat_smooth(method = "lm", se = FALSE) + 
  theme(legend.position = "none")

# analysis example workthrough
library(MASS)
library(rms)

summary(abalone)
# note that some records have min(height) = 0 which is not possible
abalone[abalone$height==0,]
abalone$height[abalone$height==0] <- NA

# The minimum weights are also a bit low compared to other measurements
# We should take a look at them.

abalone[abalone$weight.w < .01,]
# It seems these abalone are legitimately really small
# so this is probably not a data entry error. Thus we cannot exclude it. 

as.matrix(cor(na.omit(abalone[,-1])))

abfit1 = lm(rings ~ sex + length + diameter + height + weight.whole +
              weight.shucked + weight.viscera + weight.shell, data = abalone)
abfit2 = stepAIC(abfit1)
summary(abfit2)

abalone$sex = as.character(abalone$sex)
abalone$sex[abalone$sex != 'I'] = 'K'
abalone$sex = as.factor(abalone$sex)


abalone$weight.diff = abalone$weight.whole - 
  (abalone$weight.viscera + abalone$weight.shucked + abalone$weight.shell)
par(mfrow=c(1,1))
hist(abalone$weight.diff,breaks=50)
# It would appear that there are some instances
# where the whole weight is less than the sum of the components
# which does not stand up to logic. 
# We will examine the worst offenders of these.
abalone[abalone$weight.diff < -.1,]

# Now let's examine the best fit model to try and identify any potential outliers.
# The best model uses the shucked weight as a predictor.
par(mfrow=c(2,2))
plot(abfit2.2)

# This outlier - height looks like a data entry/decimal place error.
# Change 1.13 to 0.13
abalone[2052,]
summary(abalone[abalone$sex=='K',])
abalone$height[2052] = 0.130



# colour examples

library(MASS) 
library(lattice)
## Plot the claims frequency against age group by engine size and district
barchart(Claims/Holders ~ Age | Group, groups=District,
         data=Insurance, origin=0, auto.key=TRUE)

barchart(Claims/Holders ~ Age | Group, groups=District,
         data=Insurance, main="Claims frequency", 
         auto.key=list(space="top", columns=4, 
                       title="District", cex.title=1))

xyplot(Claims/Holders ~ Age | Group, groups=District,
       data=Insurance, t="l", main="Claims frequency",
       auto.key=list(space="top", columns=4, 
                     title="District", cex.title=1,
                     lines=TRUE, points=FALSE))
show.settings()

library(RColorBrewer)
display.brewer.all()

myColours <- brewer.pal(6,"Blues")

my.settings <- list(
  superpose.polygon=list(col=myColours[2:5], border="transparent"),
  strip.background=list(col=myColours[6]),
  strip.border=list(col="black")
)

barchart(Claims/Holders*100 ~ Age | Group, groups=District,
         data=Insurance, origin=0, 
         main="Motor insurance claims frequency", 
         xlab="Age", ylab="Claims frequency %",
         scales=list(alternating=1),                  
         auto.key=list(space="top", columns=4, 
                       points=FALSE, rectangles=TRUE,
                       title="District", cex.title=1),
         par.settings = my.settings,
         par.strip.text=list(col="white", font=2),
         panel=function(x,y,...){
           panel.grid(h=-1, v=0); 
           panel.barchart(x,y,...)
         }
)

## data import from URL
gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderDataFiveYear.txt"
gDat <- read.delim(file = gdURL)
## drop Oceania
jDat <- droplevels(subset(gDat, continent != "Oceania"))
str(jDat)

xyplot(lifeExp ~ gdpPercap, jDat,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       group = continent, auto.key = TRUE)

xyplot(lifeExp ~ gdpPercap | continent, jDat,
       group = country, subset = year == 2007,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       par.settings = list(superpose.symbol = list(pch = 19, cex = 1.5,
                                                   col = c("orange", "blue"))))

# import the colours to go with this example
gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderCountryColors.txt"
countryColors <- read.delim(file = gdURL, as.is = 3) # protect color
countryColors <-
  countryColors[match(levels(jDat$country), countryColors$country), ]

xyplot(lifeExp ~ gdpPercap | continent, jDat,
       group = country, subset = year == 2007,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       par.settings = list(superpose.symbol = list(pch = 19, cex = 1,
                                                   col = countryColors$color)))

xyplot(lifeExp ~ year | continent, jDat,
       group = country, type = "l",
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       par.settings = list(superpose.line = list(col = countryColors$color,
                                                 lwd = 2)))

gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderContinentColors.txt"
continentColors <- read.delim(file = gdURL, as.is = 3) # protect color
continentColors <-
  continentColors[match(levels(jDat$continent), continentColors$continent), ]

coolNewPars <- 
  list(superpose.symbol = list(pch = 21, cex = 2, col = "gray20",
                               fill = continentColors$color))

tp <- trellis.par.get() # store the original theme
# plot the original theme
myPlot <- xyplot(lifeExp ~ gdpPercap | continent, jDat,
                 group = country, subset = year == 2007,
                 scales = list(x = list(log = 10, equispaced.log = FALSE)))
myPlot

# plot the custom theme
trellis.par.set(superpose.symbol = list(pch = 19, cex = 1,
                                        col = countryColors$color))
myPlot

# plot the original theme
trellis.par.set(tp)
myPlot

# more settings and features to customise
# look at Lattice returning objects
myAwesomePlot <-
  xyplot(lifeExp ~ gdpPercap | continent, jDat,
         scales = list(x = list(log = 10, equispaced.log = FALSE)),
         type = c("p", "smooth"), grid = TRUE, col.line = "darkorange", lwd = 4)

print(myAwesomePlot)

myOtherPlot <- stripplot(lifeExp ~ reorder(continent, lifeExp),
                         subset(jDat, subset = year %in% c(1952, 1977, 2007)),
                         groups = year, auto.key = list(reverse.rows = TRUE),
                         jitter.data = TRUE, type = c("p", "a"), fun = median)

print(myAwesomePlot, pos = c(0, 0, 0.52, 1), more = TRUE)
print(myOtherPlot, pos = c(0.48, 0, 1, 1))

lifeExpSpread <- ddply(jDat, ~ continent + year, summarize,
                       sdGdpPercap = sd(gdpPercap), iqrGdpPercap = IQR(gdpPercap),
                       madGdpPercap = mad(gdpPercap))

## cheap trick? using lattice's extended formula interface to avoiding reshaping
xyplot(sdGdpPercap + iqrGdpPercap + madGdpPercap ~ year, lifeExpSpread,
       subset = continent == "Africa", type = "b", ylab = "measure of spread",
       auto.key = list(x = 0.07, y = 0.85, corner = c(0, 1)))
xyplot(sdGdpPercap + iqrGdpPercap + madGdpPercap ~ year, lifeExpSpread,
       group = reorder(continent, sdGdpPercap), layout = c(3, 1),
       type = "b", ylab = "measure of spread",
       auto.key = list(x = 0.35, y = 0.85, corner = c(0, 1),
                       reverse.rows = TRUE))

xyplot(sdGdpPercap + iqrGdpPercap + madGdpPercap ~ year, lifeExpSpread,
       subset = continent == "Africa", type = "b", ylab = "measure of spread",
       outer = TRUE, layout = c(3, 1), aspect = 1)

gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderWithColorsAndSorted.txt"
kDat <- read.delim(file = gdURL, as.is = 7) # protect color
jYear <- c(1952, 2007)
yDat <- subset(kDat, year %in% jYear)

# short cut to plot
xyplot(lifeExp ~ gdpPercap | factor(year), yDat, aspect = 2/3,
       grid = TRUE, scales = list(x = list(log = 10, equispaced.log = FALSE)))

# calling to the panel function with the default () gives the same plot
xyplot(lifeExp ~ gdpPercap | factor(year), yDat, aspect = 2/3,
       grid = TRUE, scales = list(x = list(log = 10, equispaced.log = FALSE)),
       panel = panel.xyplot)

# create an on the fly function, again empty for the same plot
xyplot(lifeExp ~ gdpPercap | factor(year), yDat, aspect = 2/3,
       grid = TRUE, scales = list(x = list(log = 10, equispaced.log = FALSE)),
       panel = function(...) {
         panel.xyplot(...)
       })

# explicitly pass the x and y variables
xyplot(lifeExp ~ gdpPercap | factor(year), yDat, aspect = 2/3,
       grid = TRUE, scales = list(x = list(log = 10, equispaced.log = FALSE)),
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
       })

# modify the character size based on the population.
# cex, character expansion. if the population is to be the circle area,
# then the radius can be discovered from formula area = pi * r squared.
# radius = sqrt(area/pi)
# create a vector for each point and pass it to cex - radius of the print character
# pass this to the panel function. 
# the subscripts argument is telling the panel function which points are being printed

jCexDivisor <- 1500                     # arbitrary scaling constant
jPch <- 21
xyplot(lifeExp ~ gdpPercap | factor(year), yDat, aspect = 2/3,
       grid = TRUE, scales = list(x = list(log = 10, equispaced.log = FALSE)),
       cex = sqrt(yDat$pop/pi)/jCexDivisor, 
       panel = function(x, y, ..., cex, subscripts) {
         panel.xyplot(x, y, cex = cex[subscripts], pch = jPch, ...)
       })

# follow the same logic for circles colour
# note that the data is organised such that the larger circles are behind
jDarkGray <- 'grey20'
xyplot(lifeExp ~ gdpPercap | factor(year), yDat, aspect = 2/3,
       grid = TRUE, scales = list(x = list(log = 10, equispaced.log = FALSE)),
       cex = sqrt(yDat$pop/pi)/jCexDivisor, fill.color = yDat$color,
       col = jDarkGray,
       panel = function(x, y, ..., cex, fill.color, subscripts) {
         panel.xyplot(x, y, cex = cex[subscripts],
                      pch = jPch, fill = fill.color[subscripts], ...)
       })

# Adding a legend
gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderContinentColors.txt"
continentColors <- read.delim(file = gdURL, as.is = 3) # protect color
continentKey <-
  with(continentColors,
       list(x = 0.95, y = 0.05, corner = c(1, 0),
            text = list(as.character(continent)),
            points = list(pch = jPch, col = jDarkGray, fill = color)))
xyplot(lifeExp ~ gdpPercap | factor(year), yDat, aspect = 2/3,
       grid = TRUE, scales = list(x = list(log = 10, equispaced.log = FALSE)),
       cex = sqrt(yDat$pop/pi)/jCexDivisor, fill.color = yDat$color,
       col = jDarkGray, key = continentKey,
       panel = function(x, y, ..., cex, fill.color, subscripts) {
         panel.xyplot(x, y, cex = cex[subscripts],
                      pch = jPch, fill = fill.color[subscripts], ...)
       })

library(dplyr)
# create some simulation data
x <- rnorm(100, mean = rep(1:5, each = 20))
z <- factor(rep(c("M", "F"), each = 50), levels = c("M", "F"))
y <- x * (as.numeric(z) + 0.1) * rnorm(100, mean = rep(1:5, each = 20), sd = seq(1, 10, by = 2))
dataM <- data.frame(x=x, y=y) %>% data.matrix

df <- dataM[z == "M", 1:2]
df <- dataM[z == "F", 1:2]

plot(x,y, type = "n")
points(dm$x, dm$y, col = "blue", pch = 19)
points(df$x, df$y, col = "red", pch = 19)
abline(lm(dm$y~dm$x), col = "blue")
abline(lm(df$y~df$x), col = "red")

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

  }
}




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

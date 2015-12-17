myPanel <- function(x, y, subscripts, ...) {
  panel.text(x - 1, y + 1, letters[subscripts], cex=1)
  panel.curve(x^2/y)
  #panel.segments(0, 1, 10, 0.5, 15, 0, 15, 5, 0, 1)
  #panel.segments(x, y, c(0, 1, 10, 0.5, 15, 0, 15, 5, 0, 1), c(1, 2, 2, 5, 17, 20, 1, 7, 2, 9),...)
}

strip.pink <- function(which.given, which.panel, factor.levels, ...) {
    if (as.logical(which.panel %% 2)) {
      panel.rect(0, 0, 1, 1, col = "pink", border = "black")
      panel.text(x = 0, y = 0.5, pos = 4,
                 lab = factor.levels[which.panel[which.given]])
    }
  if (!(as.logical(which.panel %% 2))) {
      panel.polygon(x= c(0, 1, 1, 0.5, 0), y = c(0, 0, 1, 0, 1)
                    , col = "light blue", border = "black")
      panel.text(x = 1, y = 0.5, pos = 2,
                 lab = factor.levels[which.panel[which.given]])
    }
  }

myPrePanel <- function(x, y, ...) {
  list(xlim=c(min(x) - 1, max(x) + 1),
       ylim=c(min(y) - 1, max(y) + 1))
}

x <- 1:20
y <- 1:20
g <- factor(rep(c("a", "b"), each = 10))

xyplot(x ~ y | g, aspect=1, layout=c(1, 2)
      , panel=myPanel
      , strip=strip.combined
      , prepanel=myPrePanel)


strip.pinkblue <- function(which.given, which.panel, factor.levels, ...) {
  if (as.logical(which.panel %% 2)) {
    panel.polygon(x= c(0, 1, 0), y = c(5, 0, 12)
                  , col = "pink", border = "black")
    panel.text(x = 0, y = 0.5, pos = 4,
               lab = factor.levels[which.panel])
  }
  if (!(as.logical(which.panel %% 2))) {
    panel.polygon(x= c(0, 1, 1), y = c(0, 5, 12)
                  , col = "light blue", border = "black")
    panel.text(x = 1, y = 0.5, pos = 2,
               lab = factor.levels[which.panel])
  }
}

parallelplot(~Auto[, c(1,2,3,4,5,6,8,9)] | as.factor(year)
             , data = Auto, subset = (year >= 77)
             , strip = strip.pinkblue
             , scales = list(draw = FALSE)
             , between = list(x = -20,y = -10)
             , layout = c(2, 3))


for (v in dimnames(Boston)[[2]]) {assign(paste0("mn", v), mean(Boston[,v]))}
mnBoston <- data.frame(crim = mncrim, zn = mnzn, indus = mnindus, chas = mnchas, nox = mnnox, rm = mnrm, age = mnage, dis = mndis, rad = mnrad, tax = mntax, ptratio = mnptratio, black = mnblack, lstat = mnlstat, medv = mnmedv)

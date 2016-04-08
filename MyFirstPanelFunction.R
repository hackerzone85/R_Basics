panel.eb <- function(x, y, ...) {
  x <- as.numeric(x); y <- as.numeric(y)
  ux <- sort(unique(x))
  mlist <- tapply(y, factor(x, levels = ux), mean)
  slist <- tapply(y, factor(x, levels = ux), sd)
  
  coords <- rbind(mlist + 1.96 * slist
        , mlist
        , mlist - 1.96 * slist)
  
  panel.points(y = coords[2, ], x = ux, pch = 16, ...)
  panel.segments(x0 = ux,
                 y0 = coords[2, ],
                 x1 = rep(ux, 2),
                 y1 = c(coords[1, ], coords[3, ])
                 , ...)
}


bwplot(Petal.Width~Species, iris
       , panel = panel.eb)

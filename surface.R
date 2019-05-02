library(lattice)
library(mvtnorm)
myPal <- colorRampPalette(c("#44AADD"
                            , "#44DDAA"
                            , "#AADD44"
                            , "#DDAA44"
                            , "#DD44AA"
                            , "#AA44DD"))
edge <- seq(-3, 3, length.out = 25)
surface <- expand.grid(x = edge, y = edge)
surface$normal <- dmvnorm(as.matrix(surface, ncol = 2))
wireframe(normal ~ x * y, data = surface
          , drape = TRUE
          , col.regions = myPal(200))
levelplot(normal ~ x * y, data = surface
          , col.regions = myPal(200))

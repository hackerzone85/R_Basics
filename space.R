library(lattice)

grd <- expand.grid(x = seq(-5, 5, 0.1)
                  , y = seq(-5, 5, 0.1))

grd$cls <- ifelse(grd$x^2 + grd$y^2 > 4, 2, 1)

xyplot(y~x, data = grd, pch = ".", col = cls)

grd$x2 <- grd$x^2
grd$y2 <- grd$y^2
grd$z <- sqrt(2) * grd$x * grd$y

cloud(z~x+y, data = grd, pch = ".", col = grd$cls)
cloud(z~x2+y2, data = grd, pch = ".", col = grd$cls)

grd1 <- grd[grd$cls == 1, ]
grd2 <- grd[grd$cls == 2, ]
cloud(z~x2+y2, data = grd1
      , pch = ".", col = grd1$cls
      , xlim = c(-5, 5)
      , ylim = c(-5, 5)
      , zlim = c(0, max(sqrt(2) * grd$x * grd$y)))
cloud(z~x2+y2, data = grd2
      , pch = ".", col = grd2$cls)

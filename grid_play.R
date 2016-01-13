grid.newpage()

x <- seq(0.1,0.9,length=100)
y <- 0.5+0.4*sin(seq(0,2*pi, length=100))
r <- abs(0.1*cos(seq(0,2*pi,length=100)))
gp = gpar(col = "pink"
          , fill = c("blue","green")
          , lwd = 1:10
          , alpha = c(0,0.3,0.5,0.8,1))
grid.circle(x,y,r, gp = gp)


angle <- seq(0,2*pi, length=50)
x <- seq(0.1,0.5, length=50)
y <- 0.5+0.3*sin(angle)
grid.lines(x,y)

grid.newpage()

x <- seq(0.1,0.9,length=100)
y <- 0.5+0.4*sin(seq(0,2*pi, length=100))
r <- abs(0.1*cos(seq(0,2*pi,length=100)))
gp = gpar(col = "pink"
          , fill = c("blue","green")
          , lwd = 1:10
          , alpha = c(0,0.3,0.5,0.8,1))
grid.circle(x,y,r, gp = gp)


angle <- seq(0,2*pi, length=50)
x <- seq(0.1,0.5, length=50)
y <- 0.5+0.3*sin(angle)
grid.lines(x,y)

pushViewport(viewport(h=.5, w=.7, clip = "on"))
grid.rect()
grid.circle(r=.7, gp = gpar(lwd=20))
pushViewport(viewport(clip="inherit"))
grid.circle(r=.7, gp = gpar(lwd=10, col="grey"))
pushViewport(viewport(clip = "off"))
grid.circle(r=.7)
popViewport()


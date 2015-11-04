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

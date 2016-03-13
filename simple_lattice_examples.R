bwplot(cut ~ price, data = diamonds,
       panel = function(...) {
         panel.bwplot(...)
       })

with(diamonds, bwplot(cut ~ price | color, data = diamonds,
       panel = function(x,y) {
         panel.bwplot(x,y)
         panel.average(x,y)
       }))

bwplot(voice.part ~ height, data = singer,
       xlab = "Height (inches)",
       panel = function(...) {
         panel.grid(v = -1, h = 0)
         panel.bwplot(...)
       },
       par.settings = list(plot.symbol = list(pch = 4)))
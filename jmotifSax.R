library(jmotif)
set.seed(1314)
x = seq(0, pi*4, 0.02)
y = sin(x) * 5 + rnorm(length(x))

plot(x, y, type="l", col="blue", main="A scaled sine wave with a random noise and its z-normalization")

lines(x, znorm(y, 0.01), type="l", col="red")
abline(h=c(1,-1), lty=2, col="gray50")
legend(0, -4, c("scaled sine wave","z-normalized wave"), lty=c(1,1), lwd=c(1,1), 
       col=c("blue","red"), cex=0.8)



y = c(-1, -2, -1, 0, 2, 1, 1, 0)
plot(y, type="l", col="blue"
     , main="8-points time series\nit PAA transform into 3 points"
     , xlim = c(0, 8))

points(y, pch=16, lwd=5, col="blue")

abline(v=c(1,1+7/3,1+7/3*2,8), lty=3, lwd=2, col="gray50")

y_paa3 = paa(y, 3)

segments(1,y_paa3[1],1+7/3,y_paa3[1],lwd=1,col="red")
points(x=1+7/3/2,y=y_paa3[1],col="red",pch=23,lwd=5)

segments(1+7/3,y_paa3[2],1+7/3*2,y_paa3[2],lwd=1,col="red")
points(x=1+7/3+7/3/2,y=y_paa3[2],col="red",pch=23,lwd=5)

segments(1+7/3*2,y_paa3[3],8,y_paa3[3],lwd=1,col="red")
points(x=1+7/3*2+7/3/2,y=y_paa3[3],col="red",pch=23,lwd=5)


y <- seq(-2,2, length=100)
x <- dnorm(y, mean=0, sd=1)
lines(x,y, type="l", lwd=5, col="magenta")
abline(h = alphabet_to_cuts(3)[2:3], lty=2, lwd=2, col="magenta")
text(0.7,-1,"a",cex=2,col="magenta")
text(0.7, 0,"b",cex=2,col="magenta")
text(0.7, 1,"c",cex=2,col="magenta")

data("CBF")
str(CBF)


w <- 60 # the sliding window size
p <- 10  # the PAA size
a <- 6  # the SAX alphabet size

cylinder <- manyseries_to_wordbag(CBF[["data_train"]][CBF[["labels_train"]] == 1,]
                                  , w
                                  , p
                                  , a
                                  , "exact"
                                  , 0.01)
bell <- manyseries_to_wordbag(CBF[["data_train"]][CBF[["labels_train"]] == 2,], w, p, a, "exact", 0.01)
funnel <- manyseries_to_wordbag(CBF[["data_train"]][CBF[["labels_train"]] == 3,], w, p, a, "exact", 0.01)

str(cylinder)
head(cylinder)
str(bell)
head(bell)
str(funnel)
head(funnel)

# compute tf*idf weights for three bags
#
tfidf = bags_to_tfidf(list("cylinder" = cylinder
                           , "bell" = bell
                           , "funnel" = funnel) )
head(tfidf)
tail(tfidf)
head(tfidf[order(tfidf$cylinder, decreasing = TRUE), ])
head(tfidf[order(tfidf$bell, decreasing = TRUE), ])
head(tfidf[order(tfidf$funnel, decreasing = TRUE), ])

library(ggplot2)
library(scales)

example1 <- CBF$data_train[1, ]
plot(example1, type = "l")


w <- 12 # the sliding window size
p <- 6  # the PAA size
a <- 6  # the SAX alphabet size


example1_sax <- sax_via_window(example1
                               , w
                               , p
                               , a
                               , "exact"
                               , 0.01)
example1_df <- data.frame(
  index = as.numeric(names(example1_sax))
  , words = unlist(example1_sax))

example1_df$specificity <- sapply(example1_df$words, function(x) {
  word <- match(as.character(x), tfidf$words)
  spec <- tfidf[word, ]$cylinder - tfidf[word, ]$bell - tfidf[word, ]$funnel
  ifelse(is.na(spec)
         , -Inf
         , spec)
})

example1_specifity <- rep(0, length(example1))
for(i in 1:w) {
  example1_specifity[i:(w+i-1)] <- 
    example1_specifity[i:(w+i-1)] + example1_df$specificity[i]
}


ggplot(data=example1_df
       , aes(x=index,y=y)) +
  geom_line(size=1.2) + theme_bw() +
  ggtitle("Example 1 - cylinder") +
  scale_colour_gradientn(name = "Class specificity:  ",limits=c(0,1),
                         colours=c("red","yellow","green","lightblue","darkblue"),
                         breaks=c(0,0.5,1),labels=c("negative","neutral","high"),
                         guide = guide_colorbar(title.theme=element_text(size=14, angle=0),title.vjust=1,
                                                barheight=0.6, barwidth=6, label.theme=element_text(size=10, angle=0))) +
  theme(legend.position="bottom",plot.title=element_text(size=18),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x=element_text(size=12),axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank())
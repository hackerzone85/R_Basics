seg.raw <- read.csv("http://goo.gl/qw303p")
seg.df <- seg.raw[ , -7] # remove the known segment assignments
summary(seg.df)

# quick sanity check function
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

seg.summ(seg.df, seg.raw$Segment) # could be used to check results of a clustering

# clustering
# calculating distance
c(1,2,3) - c(2,3,2) # vector of differences
sum((c(1,2,3) - c(2,3,2))^2) # the sum of squared differences
sqrt(sum((c(1,2,3) - c(2,3,2))^2)) # root sum of squares
dist(rbind(c(1,2,3), c(2,3,2)))
d <- dist(seg.df[, c("age", "income", "kids")])
as.matrix(d)[1:5, 1:5]

library(cluster) # daisy works with mixed data types
seg.dist <- daisy(seg.df)
as.matrix(seg.dist)[1:5, 1:5]
seg.hc <- hclust(seg.dist, method="complete")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[1]])
seg.df[c(101, 107), ] # similar
seg.df[c(278, 294), ] # similar
seg.df[c(173, 141), ] # disimilar
cor(cophenetic(seg.hc), seg.dist) # a goodness of fit test for dendro
plot(seg.hc)
rect.hclust(seg.hc, k=4, border="red") # choosing a set of 4 groups
seg.hc.segment <- cutree(seg.hc, k=4) # membership vector for 4 groups
table(seg.hc.segment)
# reusing the sanity check function
seg.summ(seg.df, seg.hc.segment)
# "Our advanced hierarchical analysis in R examined
# consumers who don't yet subscribe and found two segments to target! The segments
# are known as 'Men' and 'Women."'

# visualising the results
plot(jitter(as.numeric(seg.df$gender)) ~
       jitter(as.numeric(seg.df$subscribe)),
     col=seg.hc.segment, yaxt="n", xaxt="n", ylab="", xlab="")
axis(1, at=c(1, 2), labels=c("Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(1, 2), labels=levels(seg.df$gender))

# prepare to numeric data for k-means
seg.df.num <- seg.df
seg.df.num$gender <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)
summary(seg.df.num)

# k means
set.seed(96743)
seg.k <- kmeans(seg.df.num, centers=4)
seg.summ(seg.df, seg.k$cluster)
boxplot(seg.df.num$income ~ seg.k$cluster, ylab="Income", xlab="Cluster")

library(cluster)
clusplot(seg.df, seg.k$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, main="K-means cluster plot")
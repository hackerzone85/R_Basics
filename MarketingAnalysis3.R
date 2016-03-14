seg.df <- read.csv("http://goo.gl/qw303p")
mean(seg.df$income[seg.df$Segment == "Moving up"])
mean(seg.df$income[seg.df$Segment == "Moving up" & seg.df$subscribe=="subNo"])
by(seg.df$income, seg.df$Segment, mean)
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)
aggregate(seg.df$income, list(seg.df$Segment), mean)

seg.income.mean <- aggregate(seg.df$income, list(seg.df$Segment), mean)
seg.df$segIncome <- seg.income.mean[seg.df$Segment, 2]
aggregate(income ~ Segment + ownHome, data=seg.df, mean)
aggregate(income ~ Segment + ownHome + subscribe, data=seg.df, mean)
xtabs(kids ~ Segment, data=seg.df)
aggregate(kids ~ Segment, data=seg.df, sum)

seg.tab <- with(seg.df, table(kids, Segment))
apply(seg.tab*0:7, 2, sum)

library(lattice)
histogram(~subscribe | Segment, data=seg.df
          , layout=c(2,2))
histogram(~subscribe | Segment, data=seg.df, type="count",
           layout=c(4,1), col=c("burlywood", "darkolivegreen"))
histogram(~subscribe | Segment + ownHome, data=seg.df)

prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)
p.tab <- prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)
barchart(p.tab[2, ], xlim=c(0, p.tab[2, ]+0.05)
         , xlab="Subscriber proportion by Segment", col="darkolivegreen")

seg.mean <- aggregate(income~Segment, data=seg.df, mean)
barchart(income~Segment, data=seg.mean, col="grey")

seg.income.agg <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
barchart(income ~ Segment, data=seg.income.agg, groups=ownHome
         , auto.key=TRUE, par.settings = simpleTheme(col=terrain.colors(2)))

boxplot(income ~ Segment, data=seg.df, yaxt="n", ylab="Income ($k)")
ax.seq <- seq(from=0, to=120000, by=20000)
axis(side=2, at=ax.seq, labels=paste(ax.seq/1000, "k", sep=""), las=1)

bwplot(Segment ~ income, data=seg.df, horizontal=TRUE, xlab = "Income")
bwplot(income ~ Segment | ownHome, data=seg.df, horizontal=FALSE, xlab="Income")

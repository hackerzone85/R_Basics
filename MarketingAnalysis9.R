library(car)
library(arules)
library(arulesViz)
data("Groceries")
summary(Groceries)
inspect(head(Groceries, 3))

groc.rules <- apriori(Groceries
                      , parameter=list(supp=0.01
                                       , conf=0.3
                                       , target="rules"))

inspect(subset(groc.rules, lift > 3))

retail.raw <- readLines("http://fimi.ua.ac.be/data/retail.dat")
# retail.raw <- readLines("http://goo.gl/FfjDAO") # alternative link
head(retail.raw)
tail(retail.raw)
summary(retail.raw)
retail.list <- strsplit(retail.raw, " ")
rm(retail.raw)
names(retail.list) <- paste("Trans", 1:length(retail.list), sep="")
some(retail.list)
str(retail.list)
retail.trans <- as(retail.list, "transactions")
# takes a few seconds
summary(retail.trans)

retail.rules <- apriori(retail.trans
                        , parameter=list(supp=0.001
                                         , conf=0.4))

plot(retail.rules)
plot(retail.rules, interactive=TRUE) # oh my goodness

retail.hi <- head(sort(retail.rules, by="lift"), 50)
inspect(retail.hi)

plot(retail.hi
     , method="graph"
     , control=list(type="items"))

# simulated margin data
retail.itemnames <- sort(unique(
  unlist(as(retail.trans, "list"))))
head(retail.itemnames); tail(retail.itemnames)
set.seed(03870)
retail.margin <- data.frame(margin=rnorm(
  length(retail.itemnames)
  , mean=0.30, sd=0.30))
quantile(retail.margin$margin)

rownames(retail.margin) <- retail.itemnames
head(retail.margin); tail(retail.margin)
retail.margin[c("39", "48"), ]
sum(retail.margin[c("39", "48"), ])

(basket.items <- as(retail.trans[3], "list")[[1]])
retail.margin[basket.items, ]
sum(retail.margin[basket.items, ])
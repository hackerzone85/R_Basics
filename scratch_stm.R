library(stm)
# vignette
# https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf

# all the steps are very time consuming
# download file: http://goo.gl/tsprNO
data <- read.csv("poliblogs2008.csv")
set.seed(12021)
data <- data[sample(1000),]

processed <- textProcessor(data$documents, metadata = data)
# shows how many non-stopword terms would be removed by preprocessing
plotRemoved(processed$documents, lower.thresh = seq(1, 15, by = 1))

out <- prepDocuments(processed$documents, processed$vocab, 
                     processed$meta, lower.thresh = 3)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# some ready made examples
# load(url("http://goo.gl/VPdxlS"))

# this takes a long time to run
# it calculates the corpus having 20 distinct topics

# Additionally users can include more flexible functional forms of
# continuous covariates, including standard transforms like log(), as well as ns() or bs() from
# the splines package. The stm package also includes a convenience function s(), which selects
# a fairly flexible b-spline basis.
poliblogPrevFit <- stm(out$documents
                       , out$vocab, K = 20,
                       prevalence =~ rating + s(day), max.em.its = 75,
                       data = out$meta, init.type = "Spectral")
labelTopics(poliblogPrevFit, 3)

# this creates a number of models, discarding those with low likelihood 
poliblogSelect <- selectModel(out$documents, out$vocab, K = 20,
                              prevalence =~ rating + s(day), max.em.its = 75,
                              data = out$meta, runs = 20, seed = 8458159)
plotModels(poliblogSelect)

selectedmodel <- poliblogSelect$runout[[2]]
plot(selectedmodel)

# this can be used to generate stats to find what is the best value of K
storage <- searchK(out$documents, out$vocab, K = c(7, 10),
                   prevalence =~ rating + s(day), data = meta)

# extracting docs that are strongly associated with a topic
thoughts3 <- findThoughts(poliblogPrevFit, n = 3, topics = 3, thresh = 0.5)
plotQuote(substr(data$documents[thoughts3$index[[1]]],1,100))

out$meta$rating <- as.factor(out$meta$rating)
prep <- estimateEffect(1:20 ~ rating + s(day), poliblogPrevFit,
                      meta = out$meta, uncertainty = "Global")
prep2 <- estimateEffect(1:20 ~ rating + s(day), selectedmodel,
                       meta = out$meta, uncertainty = "Global")

plot.estimateEffect(prep, covariate = "rating", topics = c(3, 7, 20),
                    model = poliblogPrevFit, method = "difference",
                    cov.value1 = "Liberal", cov.value2 = "Conservative",
                    xlab = "More Conservative ... More Liberal",
                    main = "Effect of Liberal vs. Conservative",
                    #xlim = c(-1, 1),
                    labeltype = "custom",
                    custom.labels = c(3, 7, 20))

# looking at topic variation with publication dates
plot.estimateEffect(prep, "day", method = "continuous"
                    , topics = 7, model = z
                    , printlegend = FALSE, xaxt = "n"
                    , xlab = "Time (2008)")
# add axis with month names
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
    labels = monthnames)

cloud(poliblogPrevFit, topic = 7, scale = c(2,.25))

mod.out.corr <- topicCorr(poliblogPrevFit)
plot.topicCorr(mod.out.corr)

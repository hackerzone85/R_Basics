library(FactoMineR)
data("USArrests")
res.pca <- PCA(USArrests, graph = FALSE)
eig.val <- res.pca$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by PCs (%)",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")

plot(res.pca, choix = "ind", autoLab = "yes")
plot(res.pca, choix = "var", autoLab = "yes")

# Eigenvalues
res.pca$eig

# Results for Variables
res.var <- res.pca$var
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- res.pca$var
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

data("decathlon")
summary(decathlon)
View(decathlon)
res <- PCA(decathlon[, 1:10])
summary(res)
res <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
summary(res, nbelements = Inf)
dimdesc(res)
dimdesc(res, proba = 0.2)
plot(res, cex = 0.8
     , habillage = "Competition"
     , invisible = "quali"
     , title = "Individuals PCA Plot")
plotellipses(res, cex = 0.8
     , habillage = "Competition"
     , invisible = "quali"
     , title = "Individuals PCA Plot")
plotellipses(res, cex = 0.8
             , habillage = "Competition"
             , invisible = "quali"
             , title = "Individuals PCA Plot"
             , axes = 3:4)
plotellipses(res, cex = 0.8
             , habillage = "Competition"
             , invisible = "quali"
             , title = "Individuals PCA Plot"
             , select = "cos2 0.7")
plotellipses(res, cex = 0.8
             , habillage = "Competition"
             , invisible = "quali"
             , title = "Individuals PCA Plot"
             , select = "contrib 5")
plotellipses(res, cex = 0.8
             , habillage = "Competition"
             , invisible = "quali"
             , title = "Individuals PCA Plot"
             , select = c("Clay", "Karpov"))
plotellipses(res, cex = 0.8
             , habillage = "Competition"
             , invisible = "quali"
             , title = "Variables PCA Plot"
             , select = "contrib 5"
             , choix = "var")

library(FactoMineR)
library(missMDA)
data("orange")
res.pca <- PCA(orange)
nb <- estim_ncpPCA(orange, scale = TRUE)
comp <- imputePCA(orange, ncp = 2, scale = TRUE)
res.pca <- PCA(comp$completeObs)

mi <- MIPCA(orange, ncp = 2, scale = TRUE)
plot(mi)

library("Factoshiny")
outPCA <- PCAshiny(decathlon)

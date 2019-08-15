library(dplyr)
library(lattice)
library(latticeExtra)
library(corrgram)
library(psych)
library(car) # bcpower
library(Hmisc) # impute with mean/mode etc.
library(VIM) # impute with KNN
library(gmodels)
source("C:\\Dev\\Study\\R\\R_Themes\\MarketingTheme.R")

densWithOuts <- function(var, varLabel, marks) { 
  densityplot(~var
              , plot.points = FALSE
              , panel = function(x, ...) {
                panel.densityplot(x, ...)
                jitter.amount = 0.01 * 
                  diff(current.panel.limits()$ylim)
                markings <- rep(2, length(x))
                markings[marks] <- 5
                panel.xyplot(x = x
                             , y = jitter(rep(0, length(x))
                                          , amount = jitter.amount)
                             , col = myPal[markings]
                             , cex = markings/4)
              }
              , xlab = varLabel
              , par.settings = MyLatticeTheme
              , key = simpleKey(
                  text = c("Inside IQR * 1.5"
                         , "Outside IQR * 1.5")
                #, space = "top"
                , points = FALSE
                , col = myPal[c(2, 5)]
                , columns = 2)
              )
}

heart <- read.csv("Heart_ds2.csv") %>%
  dplyr::select(-X) %>%
  mutate(Ca = factor(Ca)
         , Slope = factor(Slope)
         , ExAng = factor(ExAng)
         , RestECG = factor(RestECG)
         , Fbs = factor(Fbs)
         , Sex = factor(Sex)
  )

classes <- sapply(heart, class)
facs <- names(classes[classes == "factor"])
heart.facs <- sapply(facs, function(x) {
  length(levels(heart[[x]]))
})
tabs <- sapply(facs, function(x) {
  table(heart[[x]])
})
nums <- names(classes[classes != "factor"])

summary(heart[, nums])
dens <- list()
shap <- list()
outs <- list()

for (num in nums) {
  shap[[num]] <- shapiro.test(heart[[num]])
  Q1Q3 <- quantile(heart[[num]])[c(2, 4)]
  IQR <- Q1Q3[2] - Q1Q3[1]
  outs[[num]] <- which(heart[[num]] < Q1Q3[1] - 1.5 * IQR |
    heart[[num]] > Q1Q3[2] + 1.5 * IQR)
}

d <- densWithOuts(heart[["Age"]]
             , varLabel = "Age"
             , marks = outs[["Age"]])
d + layer(panel.text(x = max(x)
                     , y = (current.panel.limits()$ylim[2] -
                       current.panel.limits()$ylim[1])/2
                     , paste("Shapiro Wilk\nnormality test\nStatistic"
                             , round(shap$Age$statistic, 2)
                             , "\np-value"
                             , round(shap$Age$`p.value`, 2))
                     , font = MyLatticeFont$font
                     , col = MyLatticeFont$col
                     , cex = MyLatticeFont$cex
                    ))
  
d <- densWithOuts(heart[["RestBP"]]
             , varLabel = "RestBP"
             , marks = outs[["RestBP"]])
d + layer(panel.text(x = max(x)
                     , y = (current.panel.limits()$ylim[2] -
                              current.panel.limits()$ylim[1])/2
                     , paste("Shapiro Wilk\nnormality test\nStatistic"
                             , round(shap$RestBP$statistic, 2)
                             , "\np-value"
                             , round(shap$RestBP$`p.value`, 2))
                     , font = MyLatticeFont$font
                     , col = MyLatticeFont$col
                     , cex = MyLatticeFont$cex
                     
))

d <- densWithOuts(heart[["Chol"]]
             , varLabel = "Chol"
             , marks = outs[["Chol"]])
d + layer(panel.text(x = max(x)
                     , y = (current.panel.limits()$ylim[2] -
                              current.panel.limits()$ylim[1])/2
                     , paste("Shapiro Wilk\nnormality test\nStatistic"
                             , round(shap$Chol$statistic, 2)
                             , "\np-value"
                             , round(shap$Chol$`p.value`, 2))
                     , font = MyLatticeFont$font
                     , col = MyLatticeFont$col
                     , cex = MyLatticeFont$cex
                     
))

d <- densWithOuts(heart[["MaxHR"]]
             , varLabel = "MaxHR"
             , marks = outs[["MaxHR"]])
d + layer(panel.text(x = max(x)
                     , y = (current.panel.limits()$ylim[2] -
                              current.panel.limits()$ylim[1])/2
                     , paste("Shapiro Wilk\nnormality test\nStatistic"
                             , round(shap$MaxHR$statistic, 2)
                             , "\np-value"
                             , round(shap$MaxHR$`p.value`, 2))
                     , font = MyLatticeFont$font
                     , col = MyLatticeFont$col
                     , cex = MyLatticeFont$cex
                     
))

d <- densWithOuts(heart[["Oldpeak"]]
             , varLabel = "Oldpeak"
             , marks = outs[["Oldpeak"]])
d + layer(panel.text(x = max(x)
                     , y = (current.panel.limits()$ylim[2] -
                              current.panel.limits()$ylim[1])/2
                     , paste("Shapiro Wilk\nnormality test\nStatistic"
                             , round(shap$Oldpeak$statistic, 2)
                             , "\np-value"
                             , round(shap$Oldpeak$`p.value`, 2))
                     , font = MyLatticeFont$font
                     , col = MyLatticeFont$col
                     , cex = MyLatticeFont$cex
))

# impute missing values Thal
heart$Thal <- impute(heart$Thal, mode) # mode is "normal"

# impute missing values in Ca
heart.imputn <- heart[!is.na(heart$Ca),]
imputn.rows <- nrow(heart.imputn)
heart.NA <- heart[is.na(heart$Ca),]
heart.imp.NA <- rbind(heart.imputn, heart.NA)

hearttemp <-kNN(heart.imp.NA, "Ca")[,-15]

toputback <- hearttemp[-c(1:imputn.rows), ]
heart <- heart[!is.na(heart$Ca),]
heart <- rbind(heart, toputback)

# remove the Chol outlier
cholmax <- which.max(heart$Chol)
heart <- heart[-cholmax,]

# getting a box cox parameter
Oldpeak_lambda <- coef(powerTransform(heart$Oldpeak + 10e-6)) # car package
# returning a bc transformed variable
heart$Oldpeak_t <- bcPower(heart$Oldpeak + 10e-6, Oldpeak_lambda)

nums <- c(nums, "Oldpeak_t")
dens <- list()
shap <- list()
outs <- list()

for (num in nums) {
  shap[[num]] <- shapiro.test(heart[[num]])
  Q1Q3 <- quantile(heart[[num]])[c(2, 4)]
  IQR <- Q1Q3[2] - Q1Q3[1]
  outs[[num]] <- which(heart[[num]] < Q1Q3[1] - 1.5 * IQR |
                         heart[[num]] > Q1Q3[2] + 1.5 * IQR)
}

d <- densWithOuts(heart[["Age"]]
                  , varLabel = "Age"
                  , marks = outs[["Age"]])
d + layer(panel.text(x = max(x)
                     , y = (current.panel.limits()$ylim[2] -
                              current.panel.limits()$ylim[1])/2
                     , paste("Shapiro Wilk\nnormality test\nStatistic"
                             , round(shap$Age$statistic, 2)
                             , "\np-value"
                             , round(shap$Age$`p.value`, 2))
                     , font = MyLatticeFont$font
                     , col = MyLatticeFont$col
                     , cex = MyLatticeFont$cex
))

d <- densWithOuts(heart[["RestBP"]]
                  , varLabel = "RestBP"
                  , marks = outs[["RestBP"]])
d + layer(panel.text(x = max(x)
                     , y = (current.panel.limits()$ylim[2] -
                              current.panel.limits()$ylim[1])/2
                     , paste("Shapiro Wilk\nnormality test\nStatistic"
                             , round(shap$RestBP$statistic, 2)
                             , "\np-value"
                             , round(shap$RestBP$`p.value`, 2))
                     , font = MyLatticeFont$font
                     , col = MyLatticeFont$col
                     , cex = MyLatticeFont$cex
                     
))

d <- densWithOuts(heart[["Chol"]]
                  , varLabel = "Chol"
                  , marks = outs[["Chol"]])
d + layer(panel.text(x = max(x)
                     , y = (current.panel.limits()$ylim[2] -
                              current.panel.limits()$ylim[1])/2
                     , paste("Shapiro Wilk\nnormality test\nStatistic"
                             , round(shap$Chol$statistic, 2)
                             , "\np-value"
                             , round(shap$Chol$`p.value`, 2))
                     , font = MyLatticeFont$font
                     , col = MyLatticeFont$col
                     , cex = MyLatticeFont$cex
                     
))

d <- densWithOuts(heart[["MaxHR"]]
                  , varLabel = "MaxHR"
                  , marks = outs[["MaxHR"]])
d + layer(panel.text(x = max(x)
                     , y = (current.panel.limits()$ylim[2] -
                              current.panel.limits()$ylim[1])/2
                     , paste("Shapiro Wilk\nnormality test\nStatistic"
                             , round(shap$MaxHR$statistic, 2)
                             , "\np-value"
                             , round(shap$MaxHR$`p.value`, 2))
                     , font = MyLatticeFont$font
                     , col = MyLatticeFont$col
                     , cex = MyLatticeFont$cex
                     
))

d <- densWithOuts(heart[["Oldpeak_t"]]
                  , varLabel = "Oldpeak_t"
                  , marks = outs[["Oldpeak_t"]])
d + layer(panel.text(x = max(x)
                     , y = (current.panel.limits()$ylim[2] -
                              current.panel.limits()$ylim[1])/2
                     , paste("Shapiro Wilk\nnormality test\nStatistic"
                             , round(shap$Oldpeak_t$statistic, 2)
                             , "\np-value"
                             , round(shap$Oldpeak_t$`p.value`, 2))
                     , font = MyLatticeFont$font
                     , col = MyLatticeFont$col
                     , cex = MyLatticeFont$cex
))

heart$Oldpeak_nz <- factor(ifelse(heart$Oldpeak == 0
                                  , 0
                                  , 1))

corrgram(heart
         , diag.panel = panel.density
         , lower.panel = panel.ellipse
         , upper.panel = panel.pie
         , order = "HC")

cor.test(heart$MaxHR, heart$Oldpeak
         , method = "pearson")
cor.test(heart$Oldpeak, heart$Chol
         , method = "pearson")
cor.test(heart$Chol, heart$Age
         , method = "pearson")
cor.test(heart$Age, heart$RestBP
         , method = "pearson")
cor.test(heart$Age, heart$MaxHR
         , method = "pearson")
cor.test(heart$RestBP, heart$MaxHR
         , method = "pearson")

CPSex <- with(heart, table(ChestPain, Sex))
mosaic(CPSex, shade = TRUE
       , legend = TRUE
       , gp = shading_Friendly
       , labeling_args =
         list(set_labels = list(
                Sex = c("Female", "Male"))
              , rot_labels = c(0, 0, 0, 0)
              , offset_labels = c(0, 0, 0, -12)
              , offset_varnames = c(0, 0, 0, 0))
       )

CPSex <- with(heart, table(ChestPain, Sex))
mosaic(CPSex, shade = TRUE
       , legend = TRUE
       , gp = shading_Friendly
       , labeling_args =
         list(set_labels = list(
           Sex = c("Female", "Male"))
           , rot_labels = c(0, 0, 0, 0)
           , offset_labels = c(0, 0, 0, -12)
           , offset_varnames = c(0, 0, 0, 0))
)

ThalSex <- with(heart, table(Thal, Sex))
mosaic(ThalSex, shade = TRUE
       , legend = TRUE
       , gp = shading_Friendly
       , labeling = labeling_values
       , labeling_args =
         list(set_labels = list(
           Sex = c("Female", "Male"))
           , rot_labels = c(0, 0, 0, 0)
           , offset_labels = c(0, 0, 0, -16)
           , offset_varnames = c(0, 0, 0, 0))
)

mosaic(HDSex, shade = TRUE
       , legend = TRUE
       , gp = shading_Friendly
       , labeling = labeling_values
       , labeling_args =
         list(set_labels = list(
           Sex = c("Female", "Male"))
           , rot_labels = c(0, 0, 0, 0)
           , offset_labels = c(0, 0, 0, 0.5)
           , just_labels = "left"
           , offset_varnames = c(0, 0, 0, 0))
)

HDFBS <- with(heart, table(HDisease, Fbs))
mosaic(HDFBS, shade = TRUE
       , legend = TRUE
       , gp = shading_Friendly
       , labeling = labeling_values
       , labeling_args =
         list(set_labels = list(
           Fbs = c("Low", "High"))
           , set_varnames = list(
             Fbs = "Diabetes Risk"
             , HDisease = "Heart Disease"
           )
           , rot_labels = c(0, 0, 0, 0)
           , offset_labels = c(0, 0, 0, 0.5)
           , just_labels = "left"
           , offset_varnames = c(0, 0, 0, 0))
)
with(heart, chisq.test(HDisease, Sex))


tAgeSexF <- with(heart
                 , t.test(Age[Sex == "0"]))
tAgeSexM <- with(heart
                 , t.test(Age[Sex == "1"]))

tAgeSex <- t.test(Age~Sex
              , data = heart
              , paired = FALSE
              , var.equal = TRUE)
bwp <- bwplot(Age~Sex, data = heart
        , subscripts = TRUE
        , par.settings = MyLatticeTheme
        , main = "Age by Gender"
        , sub = paste("Results of t test on"
                       , tAgeSex$parameter, "df, p ="
                       , round(tAgeSex$`p.value`, 2))
        , scales = 
            list(x = 
            list(labels = c("Female", "Male"))))
bwp + layer(panel.segments(x0 = c(0.9, 1, 0.9)
                           , x1 = c(1.1, 1, 1.1)
                           , y0 = c(tAgeSexF$conf.int[1]
                                    , tAgeSexF$conf.int[1]
                                    , tAgeSexF$conf.int[2])
                           , y1 = c(tAgeSexF$conf.int[1]
                                    , tAgeSexF$conf.int[2]
                                    , tAgeSexF$conf.int[2])
                           , lty = 3
                           , lwd = 1
                           , col = myPal[1])
  ) + layer(panel.segments(x0 = c(1.9, 2, 1.9)
                        , x1 = c(2.1, 2, 2.1)
                        , y0 = c(tAgeSexM$conf.int[1]
                                 , tAgeSexM$conf.int[1]
                                 , tAgeSexM$conf.int[2])
                        , y1 = c(tAgeSexM$conf.int[1]
                                 , tAgeSexM$conf.int[2]
                                 , tAgeSexM$conf.int[2])
                        , lty = 3
                        , lwd = 1
                        , col = myPal[1])
            
  )


tRestAngY <- with(heart
                 , t.test(RestBP[ExAng == "Yes"]))
tRestAngN <- with(heart
                 , t.test(Age[Sex == "No"]))

tRestAng <- t.test(RestBP~ExAng
                  , data = heart
                  , paired = FALSE
                  , var.equal = TRUE)

xyplot(MaxHR~Age, data = heart
       , par.settings = MyLatticeTheme) +
  latticeExtra::layer(panel.lmline(x, y
                                   , lty = 1
                                   , lwd = 2
                                   , col = myPal[4]
                                   , alpha = 1))

xyplot(RestBP~Age, data = heart
       , par.settings = MyLatticeTheme) +
  latticeExtra::layer(panel.lmline(x, y
                                   , lty = 1
                                   , lwd = 2
                                   , col = myPal[4]
                                   , alpha = 1))

xyplot(RestBP~MaxHR, data = heart
       , sub = "both are correlated with Age but not each other"
       , par.settings = MyLatticeTheme) +
  latticeExtra::layer(panel.lmline(x, y
                                     , lty = 1
                                     , lwd = 2
                                     , col = myPal[4]
                                     , alpha = 1))

ggplot(data = heart
       , aes(x = Age, fill = NumVessels)) +
  geom_histogram() +
  facet_grid(NumVessels~.) +
  myGgFillScale +
  myGgTheme

with(heart, leveneTest(Age, NumVessels))
oneway.test(Age~NumVessels
            , data = heart
            , var.equal = FALSE)

aov1 <- aov(Age~NumVessels, data = heart)
TukeyHSD(aov1)

ggplot(data = heart
       , aes(x = RestBP
             , fill = RestECG2
             , colour = ExAng)) +
  geom_histogram() +
  facet_grid(RestECG2+ExAng~.) +
  myGgFillScale +
  myGgColourScale +
  myGgTheme

with(heart
     , leveneTest(
       RestBP
       , factor(paste(RestECG2, ExAng))))
aov1 <- aov(RestBP~RestECG2*ExAng, data = heart)
summary(aov1)
t.test(RestBP~RestECG2, data = heart)


bwp <- bwplot(Age~NumVessels, data = heart
              , subscripts = TRUE
              , par.settings = MyLatticeTheme
              , main = "Age by Number of Vessels"
              , sub = paste("Results of t test on"
                            , tAgeSex$parameter, "df, p ="
                            , round(tAgeSex$`p.value`, 2)))
bwp + layer(panel.segments(x0 = c(0.9, 1, 0.9)
                           , x1 = c(1.1, 1, 1.1)
                           , y0 = c(tAgeSexF$conf.int[1]
                                    , tAgeSexF$conf.int[1]
                                    , tAgeSexF$conf.int[2])
                           , y1 = c(tAgeSexF$conf.int[1]
                                    , tAgeSexF$conf.int[2]
                                    , tAgeSexF$conf.int[2])
                           , lty = 3
                           , lwd = 1
                           , col = myPal[1])
) + layer(panel.segments(x0 = c(1.9, 2, 1.9)
                         , x1 = c(2.1, 2, 2.1)
                         , y0 = c(tAgeSexM$conf.int[1]
                                  , tAgeSexM$conf.int[1]
                                  , tAgeSexM$conf.int[2])
                         , y1 = c(tAgeSexM$conf.int[1]
                                  , tAgeSexM$conf.int[2]
                                  , tAgeSexM$conf.int[2])
                         , lty = 3
                         , lwd = 1
                         , col = myPal[1])
          
)

tRestRestN <- with(heart, t.test(RestBP[RestECG2 == "Normal"]))
tRestRestA <- with(heart, t.test(RestBP[RestECG2 == "Abnormal"]))
tRestRest <- t.test(RestBP~RestECG2, data = heart)

bwp <- bwplot(RestBP~RestECG2, data = heart
              , subscripts = TRUE
              , par.settings = MyLatticeTheme
              , main = "Resting Blood Pressure by Resting ECG"
              , sub = paste("Results of t test on"
                            , round(tRestRest$parameter, 2), "df, p ="
                            , round(tRestRest$`p.value`, 2)))
bwp + latticeExtra::layer(panel.segments(x0 = c(0.9, 1, 0.9)
                           , x1 = c(1.1, 1, 1.1)
                           , y0 = c(tRestRestN$conf.int[1]
                                    , tRestRestN$conf.int[1]
                                    , tRestRestN$conf.int[2])
                           , y1 = c(tRestRestN$conf.int[1]
                                    , tRestRestN$conf.int[2]
                                    , tRestRestN$conf.int[2])
                           , lty = 3
                           , lwd = 1
                           , col = myPal[1])
) + latticeExtra::layer(panel.segments(x0 = c(1.9, 2, 1.9)
                         , x1 = c(2.1, 2, 2.1)
                         , y0 = c(tRestRestA$conf.int[1]
                                  , tRestRestA$conf.int[1]
                                  , tRestRestA$conf.int[2])
                         , y1 = c(tRestRestA$conf.int[1]
                                  , tRestRestA$conf.int[2]
                                  , tRestRestA$conf.int[2])
                         , lty = 3
                         , lwd = 1
                         , col = myPal[1])
          
)

with(heart, table(Sex, Oldpeak_nz))
with(heart, table(Sex, Slope))
with(heart, table(Sex, NumVessels))
with(heart, table(Slope, NumVessels))

with(heart, table(HDisease, Oldpeak_nz))
with(heart, table(ChestPain, Oldpeak_nz))
with(heart, table(ChestPain, Slope))
with(heart, table(ChestPain, NumVessels))

CrossTable(CPSex, prop.chisq = TRUE
           , prop.c = FALSE
           , format = "SPSS")

HDFBS_Sex <- with(heart, table(HDisease, Fbs, Sex))

(LOR <- loddsratio(HDFBS_Sex))
(OR <- loddsratio(HDSex, log = FALSE))
summary(LOR)

loddsThalSex <- loddsratio(ThalSex)
loCPSex <- loddsratio(CPSex)
loCPSex
plot(loCPSex)
confint(LOR)
confint(OR)
fisher.test(HDSex)

fourfold(HDSex, color = myPalFourFold)

fourfold(Hdisease~Sex, data = heart)

HDSex_Slope <- with(heart, table(HDisease, Sex, Slope))


ThalSex <- with(heart, table(Thal, Sex))
mosaic(ThalSex)                      
ThalCP <- with(heart, table(ChestPain, Thal, Sex))
mosaic(ThalCP)

ThalHD <- with(heart, table(HDisease, Sex, Thal))
fourfold(aperm(ThalHD, c(1,3,2)))
LOR <- loddsratio(ThalHD)
fourfold(ThalHD)

HDRestSlop <- with(heart, table(HDisease, RestECG2, Slope))
fourfold(HDRestSlop)
LOR <- loddsratio(HDRestSlop)
plot(LOR)
mosaic(HDRestSlop, gp = shading_hcl
, gp_args = list(h = c(130, 350)
                 , c = 200
                 , l = c(95, 60)))

mosaic(HDSex, shade = TRUE
       , legend = TRUE
       , gp = shading_hcl
       , gp_args = hcl_div1
       , labeling = labeling_values
       , labeling_args =
         list(set_labels = list(
           Sex = c("Female", "Male"))
           , rot_labels = c(0, 0, 0, 0)
           , offset_labels = c(0, 0, 0, 0.5)
           , just_labels = "left"
           , offset_varnames = c(0, 0, 0, 0))
)

#### Heart Disease in relation to Thal Defects and Gender
```{r Thal_sex, opts.label='fig.centre'}
ThalSex <- with(heart, table(Thal, Sex))

CrossTable(ThalSex, prop.chisq = TRUE
           , prop.c = FALSE
           , format = "SPSS")

mosaic(ThalSex, shade = TRUE
       , legend = TRUE
       , gp = shading_Friendly
       , labeling = labeling_values
       , labeling_args =
         list(set_labels = list(
           Sex = c("Female", "Male"))
           , rot_labels = c(0, 0, 0, 0)
           , offset_labels = c(0, 0, 0, -15.1)
           , just_labels = "left"
           , offset_varnames = c(0, 0, 0, 0))
)
with(heart, chisq.test(Thal, Sex))
```

```{r Thal_sex2, opts.label='fig.wide'}
ThalHD <- with(heart, table(HDisease, Sex, Thal))

LOR <- loddsratio(ThalHD)
confint(LOR)

fourfold(ThalHD, color = myPalFourFold)
plot(LOR, col = myPal[2]
     , bars = FALSE, whiskers = 0.05
     , gp_main = gpar(fontface = MyLatticeFont$font
                      , col = MyLatticeFont$col))
```
```{r Thal_sex3, opts.label='fig.centre'}
mosaic(ThalHD, gp = shading_Friendly)

```

ggplot(data = heart
       , aes(x = HDisease, y = MaxHR, fill = Slope)) +
  geom_boxplot() +
  myGgFillScale +
  myGgTheme
ggplot(data = heart
       , aes(x = MaxHR
             , fill = HDisease
             , colour = Slope)) +
  geom_histogram() +
  facet_grid(HDisease+Slope~.) +
  myGgFillScale +
  myGgColourScale +
  myGgTheme


grps <- factor(with(heart, paste(HDisease, Slope)))
tapply(heart$MaxHR, grps, FUN = shapiro.test)
aovAge <- aov(MaxHR~HDisease+RestECG2+Slope+
                HDisease*RestECG2+
                HDisease*Slope+
                RestECG2*Slope+
                HDisease*RestECG2*Slope
                , data = heart)
summary(aovAge)
aovAge2 <- aov(MaxHR~HDisease+Slope+
                HDisease*Slope
              , data = heart)
summary(aovAge2)

HDSlop <- with(heart, table(HDisease, Slope))

mosaic(HDSlop, shade = TRUE
       , legend = TRUE
       , gp = shading_hcl
       , gp_args = hcl_div1
       , labeling = labeling_values
       , labeling_args =
         list(rot_labels = c(0, 0, 0, 0)
              , offset_labels = c(0, 1, 0, 0)
              , just_labels = "right"
              , offset_varnames = c(0, 1, 0, 0))
)


data("Arthritis", package = "vcd")
Art <- xtabs(~ Treatment + Improved, data = Arthritis)
Art
round(100 * prop.table(Art, margin = 1), 2)
assocstats(Art)

Art2 <- xtabs(~ Treatment + Improved + Sex, data = Arthritis)
Art2
assocstats(Art2)
CMHtest(Art2)
# the test was more significant for female 
# because of the greater number of female in the data
apply(Art2, 3, sum)

data("Mental", package = "vcdExtra")
mental <- xtabs(Freq ~ ses + mental, data = Mental)
assocstats(mental)    # standard chisq tests
CMHtest(mental)       # CMH tests

# general association
cmhdemo1 <- read.table(header=TRUE, sep="", text="
                       b1  b2   b3  b4  b5
                       a1    0  15   25  15   0
                       a2    5  20    5  20   5
                       a3   20   5    5   5  20
                       ")
cmhdemo1 <- as.matrix(cmhdemo1)

# linear association
cmhdemo2 <- read.table(header=TRUE, sep="", text="
                       b1  b2   b3  b4  b5
                       a1    2   5    8   8   8
                       a2    2   8    8   8   5
                       a3    5   8    8   8   2
                       a4    8   8    8   5   2
                       ")

cmhdemo2 <- as.matrix(cmhdemo2)

CMHtest(cmhdemo1)
CMHtest(cmhdemo2)

sieve(cmhdemo1, shade=TRUE, main="General association",
      gp = shading_sieve(interpolate = 0, lty = c("solid", "longdash")))
sieve(cmhdemo2, shade=TRUE, main="Linear association",
      gp = shading_sieve(interpolate = 0, lty = c("solid", "longdash")))

# for 2x2xk tables, determine if the odds ratios differ across the strata
woolf_test(UCBAdmissions)
woolf_test(Art2)

# the loglinear model agrees with the woolf test on the art data
loglm(~ (Treatment + Improved + Sex)^2, data = Art2)
loglm(~ (Gender + Admit + Dept)^2, data = UCBAdmissions)

fourfold(Berkeley, std = "ind.max")   # unstandardized
fourfold(Berkeley, margin = 1)        # equating gender
fourfold(Berkeley)  # standardize both margins

summary(loddsratio(Berkeley))
exp(.6103 + c(-1, 1) * qnorm(.975) * 0.06398)
confint(loddsratio(Berkeley, log = FALSE))

UCB <- aperm(UCBAdmissions, c(2, 1, 3))
fourfold(UCB, mfrow = c(2, 3))

data("CoalMiners", package = "vcd")
CM <- CoalMiners[, , 2 : 9]
structable(. ~ Age, data = CM)
fourfold(CM, mfcol = c(2, 4))

loddsratio(CM)
loddsratio(CM, log = FALSE)

lor_CM <- loddsratio(CM)
plot(lor_CM, bars=FALSE, baseline=FALSE, whiskers=.2)

lor_CM_df <- as.data.frame(lor_CM)
age <- seq(25, 60, by = 5) + 2
lmod <- lm(LOR ~ poly(age, 2), weights = 1 / ASE^2, data = lor_CM_df)
grid.lines(seq_along(age), fitted(lmod), 
           gp = gpar(col = "red", lwd = 2), default.units = "native")

summary(lmod)

haireye <- margin.table(HairEyeColor, 1:2)
expected = independence_table(haireye)
round(expected, 1)

sieve(haireye, shade=TRUE, sievetype="expected",
      main="Expected frequencies")
sieve(haireye, shade=TRUE,
      main="Observed frequencies")

sieve(haireye, sievetype = "expected", shade = TRUE,
      main="Expected frequencies",
      labeling = labeling_values, value_type = "expected",
      gp_text = gpar(fontface = 2), gp = shading_sieve(interpolate = 0, line_col="darkgrey",eps=Inf,lty="dashed"))

sieve(haireye, shade = TRUE, main="Observed frequencies",
      labeling = labeling_values, value_type = "observed",
      gp_text = gpar(fontface = 2))

data("VisualAcuity", package = "vcd")
VA <- xtabs(Freq ~ right + left + gender, data = VisualAcuity)
dimnames(VA)[1:2] <- list(c("high", 2, 3, "low"))
names(dimnames(VA))[1:2] <- paste(c("Right", "Left"), "eye grade")
structable(aperm(VA))

sieve(VA[, , "female"], shade = TRUE)

sieve(Freq ~ right + left | gender, data = VisualAcuity, 
      shade = TRUE, set_varnames = c(right = "Right eye grade", 
                                     left = "Left eye grade"))

cotabplot(VA, cond = "gender", panel = cotab_sieve, shade = TRUE)

sieve(UCBAdmissions, shade = TRUE, condvar = 'Gender')

sieve(~ Dept + Admit + Gender, data = UCBAdmissions, 
      shade = TRUE, labeling = labeling_values, 
      gp_text = gpar(fontface = 2), abbreviate_labs = c(Gender = TRUE))

cotabplot(UCBAdmissions, cond = "Gender", panel = cotab_sieve, 
          shade = TRUE)

cotabplot(UCBAdmissions, cond = "Dept", panel = cotab_sieve, 
          shade = TRUE, labeling = labeling_values, 
          gp_text = gpar(fontface = "bold"))

UCB2 <- aperm(UCBAdmissions, c(3, 2, 1))
sieve(UCB2, shade = TRUE
      , expected = ~ Admit * Gender + Dept
      , split_vertical = c(FALSE, TRUE, TRUE))

assoc(~ Hair + Eye, data = HairEyeColor, shade = TRUE, gp_axis = gpar(lty = 5))
assoc(HairEyeColor, shade = TRUE, gp_axis = gpar(lty = 5))

library(ggplot2)
library(lattice)
library(car)
sp <- read.csv("spanish.csv")

shapiro.test(sp$score[sp$course == "no"])
shapiro.test(sp$score[sp$course == "yes"])

ggplot(data = sp, aes(x = score, fill = course)) +
  geom_histogram() + facet_grid(course~.)

ggplot(data = sp, aes(x = course, y = score, fill = course)) +
  geom_boxplot()

no_scaled <- scale(sp$score[sp$course == "no"], scale = TRUE)
tail(sort(no_scaled))

# to see if group variances are the same
leveneTest(sp$score, sp$course)

t.test(sp$score~sp$course, var.equal = TRUE)

# if vars were not equal
t.test(sp$score~sp$course, var.equal = FALSE)

mat <- read.csv("math.csv")
mat$diff <-mat$grade2 - mat$grade1
shapiro.test(mat$diff)
mat_scaled <- scale(mat$diff, scale = TRUE)
head(mat_scaled)
tail(mat_scaled)

ggplot(data = mat, aes(x = diff, fill = diff)) +
  geom_histogram()

t.test(mat$grade2, mat$grade1, paired = TRUE)

vit <- read.csv("vitamin1.csv")
shapiro.test(vit$effort[vit$dose == "placebo"])
shapiro.test(vit$effort[vit$dose == "low dose"])
shapiro.test(vit$effort[vit$dose == "high dose"])

vit_scale <- scale(vit$effort, scale = TRUE)
head(vit_scale[vit$dose == "placebo"])
tail(vit_scale[vit$dose == "placebo"])
head(vit_scale[vit$dose == "low dose"])
tail(vit_scale[vit$dose == "low dose"])
head(vit_scale[vit$dose == "high dose"]) # bit of outlier here
tail(vit_scale[vit$dose == "high dose"])

ggplot(data = vit, aes(x = effort, fill = dose)) +
  geom_histogram() + facet_grid(dose~.)

leveneTest(vit$effort, vit$dose)

aov1 <- aov(effort~dose, data = vit)
summary(aov1)

#if not equal var
oneway.test(effort~dose, data = vit, var.equal = FALSE)


# now post hoc, to see where are the difference
# Tukey HSD test for equ var
TukeyHSD(aov1)

pairwise.t.test(vit$effort
                , vit$dose
                , p.adjust.method = "bonferroni")

# Two way ANOVA
vit <- read.csv("vitamin2.csv")
View(vit)
vit$two_way <- factor(paste(vit$gender, vit$dose))
shapiro.test(vit$effort[vit$two_way == "male placebo"])
shapiro.test(vit$effort[vit$two_way == "male low dose"])
shapiro.test(vit$effort[vit$two_way == "male high dose"])
shapiro.test(vit$effort[vit$two_way == "female placebo"])
shapiro.test(vit$effort[vit$two_way == "female low dose"])
shapiro.test(vit$effort[vit$two_way == "female high dose"])

vit_scale <- scale(vit$effort, scale = TRUE)
head(vit_scale[vit$two_way == "male placebo"])
tail(vit_scale[vit$two_way == "male placebo"])
# etc
leveneTest(vit$effort, vit$two_way)
aov2 <- aov(effort~dose+gender+dose*gender, data = vit)
summary(aov2)

# because dose:gender is signif. we have to consider the simple main effects
vitp <- vit[vit$dose == "placebo", ]
View(vitp)

aov1 <- aov(effort ~ gender, data = vitp)
summary(aov1)
# no signif dif between genders for placebo group according to F test

TukeyHSD(aov1)
# again, no difference

vitld <- vit[vit$dose == "low dose", ]
View(vitld)

aov2 <- aov(effort ~ gender, data = vitld)
summary(aov2) # ave difference is significant according to F test

TukeyHSD(aov2)
# ave male greater by 2.81 points

vithd <- vit[vit$dose == "high dose", ]
View(vithd)

aov3 <- aov(effort ~ gender, data = vithd)
summary(aov3) # ave difference is significant according to F test

TukeyHSD(aov3)
# ave male greater by 2.81 points

vitm <- vit[vit$gender == "male", ]
View(vitm)

aov4 <- aov(effort ~ dose, data = vitm)
summary(aov4) # ave difference is significant according to F test

TukeyHSD(aov4)
# ave male greater by 2.81 points

vitf <- vit[vit$gender == "female", ]
View(vitf)

aov5 <- aov(effort ~ dose, data = vitf)
summary(aov5) # ave difference is significant according to F test

TukeyHSD(aov5)
# ave male greater by 2.81 points

vit <- read.csv("vitamin3.csv")
vit <- within(vit
  , group <- factor(paste(dose, gender, type))
)

leveneTest(vit$effort, vit$group)

groups <- unique(vit$group)
sh.w <- list()
ht <- list()
hists <- list()
for (g in groups) {
  
  sh.w[[g]] <- shapiro.test(vit$effort[vit$group == g])
  
  g_scaled <- scale(vit$effort[vit$group == g]
                       , scale = TRUE)
  ht[[paste("head", g)]] <- head(sort(g_scaled))
  ht[[paste("tail", g)]] <- tail(sort(g_scaled))
  
  }
sh.w.p <- sapply(groups, function(g) {
  sh.w[[g]]$p.value
})

any(sh.w.p < 0.05)
which(sh.w.p < 0.05)
any(as.data.frame(ht) > 3)

ggplot(data = vit, aes(x = effort, fill = group)) +
  geom_histogram() +
  facet_grid(group~.) +
  theme(strip.text.y = element_blank()) +
  theme(strip.background = element_blank())

ggplot(data = vit, aes(x = group
                       , y = effort
                       , fill = group)) +
  geom_boxplot() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

aov1 <- aov(effort~dose*gender*type+
              dose*gender+
              dose*type+
              gender*type+
              dose+
              gender+
              type, data = vit)
summary(aov1)

# 3rd order is signif
# so should compute each second order
# interaction effects over the third

vitm <- vit[vit$gender == "male", ]
aov2 <- aov(effort~dose*type, data = vitm)
summary(aov2)

vitf <- vit[vit$gender == "female", ]
aov3 <- aov(effort~dose*type, data = vitf)
summary(aov3)

vitmb <- vit[vit$gender == "male" &
               vit$type == "blue collar", ]

aov1 <- aov(effort ~ dose, data = vitmb)
summary(aov1)
TukeyHSD(aov1)

vitfb <- vit[vit$gender == "female" &
               vit$type == "blue collar", ]

aov1 <- aov(effort ~ dose, data = vitmb)
summary(aov1)
TukeyHSD(aov1)

vitmw <- vit[vit$gender == "male" &
               vit$type == "white collar", ]

aov1 <- aov(effort ~ dose, data = vitmb)
summary(aov1)
TukeyHSD(aov1)

vitfw <- vit[vit$gender == "female" &
               vit$type == "white collar", ]

aov1 <- aov(effort ~ dose, data = vitmb)
summary(aov1)
TukeyHSD(aov1)

stu <- read.csv("students.csv")
View(stu)
stu$g <- as.factor(stu$gender)

ggplot(data = stu, aes(x = g
                       , y = score
                       , fill = g)) +
  geom_boxplot() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(data = stu, aes(x = score, fill = g)) +
  geom_histogram() +
  facet_grid(g~.) +
  theme(strip.text.y = element_blank()) +
  theme(strip.background = element_blank())

shapiro.test(stu$score[stu$gender == 0])
shapiro.test(stu$score[stu$gender == 1])

f_scale <- scale(stu$score[stu$gender == 0]
                 , scale = TRUE)
which(abs(f_scale) > 3)
m_scale <- scale(stu$score[stu$gender == 1]
                 , scale = TRUE)
which(abs(m_scale) > 3)

# to see if group variances are the same
leveneTest(stu$score, stu$gender)

# if vars were not equal
t.test(stu$score~stu$gender, var.equal = FALSE)
wilcox.test(stu$score~stu$gender, paired = FALSE)

loan <- read.csv("bankloan.csv")
View(loan)

loanadj <- loan[loan$balance > 0 ,]

ggplot(data = loanadj, aes(x = loan
                       , y = balance
                       , fill = loan)) +
  geom_boxplot() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(data = loanadj, aes(x = loan
                        , y = log(balance)
                        , fill = loan)) +
  geom_boxplot() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(data = loanadj, aes(x = balance, fill = loan)) +
  geom_histogram() +
  facet_grid(loan~.) +
  theme(strip.text.y = element_blank()) +
  theme(strip.background = element_blank())

ggplot(data = loanadj, aes(x = log(balance ), fill = loan)) +
  geom_histogram() +
  facet_grid(loan~.) +
  theme(strip.text.y = element_blank()) +
  theme(strip.background = element_blank())

loanadj$logbalance <- log(loanadj$balance)

shapiro.test(loanadj$logbalance[loanadj$loan == "yes"])
shapiro.test(loanadj$logbalance[loanadj$loan == "no"])

y_scale <- scale(loanadj$logbalance[loanadj$loan == "yes"]
                 , scale = TRUE)
which(abs(y_scale) > 3)
n_scale <- scale(loanadj$logbalance[loanadj$loan == "yes"]
                 , scale = TRUE)
which(abs(n_scale) > 3)

# to see if group variances are the same
leveneTest(loanadj$logbalance, loanadj$loan)

# if vars were not equal
t.test(loanadj$logbalance~loanadj$loan, var.equal = TRUE)
wilcox.test(loanadj$logbalance~loanadj$loan
            , paired = FALSE)

gym <- read.csv("gym.csv")
View(gym)
shapiro.test(gym$romania)
shapiro.test(gym$unitedstates)

gym2 <- data.frame(score = c(gym$romania, gym$unitedstates)
                  , judg = factor(rep(c("rom", "uni")
                          , each = nrow(gym))))

leveneTest(gym2$score, gym2$judg)

r_scale <- scale(gym2$score[gym2$judg == "rom"]
                 , scale = TRUE)
which(abs(r_scale) > 3)
u_scale <- scale(gym2$score[gym2$judg == "uni"]
                 , scale = TRUE)
which(abs(u_scale) > 3)

ggplot(data = gym2, aes(x = score, fill = judg)) +
  geom_histogram() +
  facet_grid(judg~.) +
  theme(strip.text.y = element_blank()) +
  theme(strip.background = element_blank())

ggplot(data = gym2, aes(x = judg
                           , y = score
                           , fill = judg)) +
  geom_boxplot() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

t.test(gym$romania, gym$unitedstates
       , paired = FALSE
       , var.equal = FALSE)

wilcox.test(gym$romania
            , gym$unitedstates
            , paired = FALSE)

inc <- read.csv("incomedata.csv")
View(inc)

inc <- within(inc
       , group <- factor(paste(educ, agecat, gender))
)

inc$loginc <- log(inc$income)

groups <- unique(inc$group)
sh.w <- list()
ht <- list()
hists <- list()
validg <- character()
for (g in groups) {
  
  if (sum(inc$group == g) < 3) next
  validg <- c(validg, g)
  sh.w[[g]] <- shapiro.test(inc$loginc[inc$group == g])
  
  g_scaled <- scale(inc$loginc[inc$group == g]
                    , scale = TRUE)
  ht[[paste("head", g)]] <- head(sort(g_scaled))
  ht[[paste("tail", g)]] <- tail(sort(g_scaled))
  
}
sh.w.p <- sapply(validg, function(g) {
  sh.w[[g]]$p.value
})

any(sh.w.p < 0.05)
which(sh.w.p < 0.05)
lapply(ht, function(x) {any(x > 3)})

ggplot(data = inc, aes(x = loginc, fill = group)) +
  geom_histogram() +
  facet_grid(group~.) +
  theme(strip.background = element_blank()) +
  theme(legend.position="none")

ggplot(data = inc, aes(x = group
                       , y = loginc
                       , fill = group)) +
  geom_boxplot() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
          legend.position="none")

shapiro.test(inc$loginc[as.numeric(inc$group) == 1])
shapiro.test(inc$loginc[as.numeric(inc$group) == 2])
shapiro.test(inc$loginc[as.numeric(inc$group) == 3])
# and so on

aov1 <- aov(loginc~educ, data = inc)
summary(aov1)
TukeyHSD(aov1)

aov1 <- aov(loginc~educ+agecat+educ*agecat, data = inc)
summary(aov1)
TukeyHSD(aov1)

incage1 <- inc[inc$agecat == "Over 44", ]
aov1 <- aov(loginc~educ, data = incage1)
summary(aov1)

incage2 <- inc[inc$agecat == "31-44", ]
aov1 <- aov(loginc~educ, data = incage2)
summary(aov1)
TukeyHSD(aov1)

incage3 <- inc[inc$agecat == "Up to 30", ]
aov1 <- aov(loginc~educ, data = incage3)
summary(aov1)
TukeyHSD(aov1)

aov1 <- aov(loginc~educ+agecat+gender+
              educ*agecat+
              agecat*gender+
              educ*gender+
              educ*agecat*gender
              , data = inc)
summary(aov1)
TukeyHSD(aov1)

incf <- inc[inc$gender == "Female", ]
incm <- inc[inc$gender == "Male", ]

aov1 <- aov(loginc~educ, data = incf)
summary(aov1)
TukeyHSD(aov1)
aov1 <- aov(loginc~educ, data = incm)
summary(aov1)
TukeyHSD(aov1)

# install.packages("Biograph")
library(Biograph)
library(mvna) 
library(etm)
library (msm)
library(Epi)
library(TraMineR)
library(lattice)
library(ggplot2)
data(GLHS)
d <- Remove.intrastate(GLHS) 
dd <- ChangeObservationWindow.e(Bdata=d, entrystate="J", exitstate=NA) 
d3.a <- date_b(Bdata=dd, selectday=1, format.out="age")

data <- date_b(Bdata=dd, selectday=1, format.out="age"
               , covs=c("marriage","LMentry"))

# parametric
# Select 10 respondents and create Biograph object
idd <- c(1, 2, 67, 76, 82, 96, 99, 180, 200, 208) 
d.10 <- d3.a[d3.a$ID %in% idd,] 
D <- Biograph.mvna(d.10) 
tra <- matrix(ncol=2,nrow=2,FALSE) 
tra[1, 2] <- TRUE 
tra[2, 1] <- TRUE 
na <- mvna(data=D$D,c("J","N"),tra,"cens") 
etm.0 <- etm(data=D$D,c("J","N"),tra,"cens",s=0) 

gg.1 <- data.frame(round(na$"J N"$time,4), na$n.risk[, 1]
                   , unname(aperm(na$n.event,c(3,2,1))[,2,1])
                   , na$n.cens[,1], round(na$"J N"$na,4)
                   , round(na$"J N"$var.aalen,3)
                   , round(aperm(etm.0$delta.na,c(3,2,1))[,2,1],4))
dimnames(gg.1) <- list(1:37, 
                       c("age","nrisk","nevent","ncens","cumhaz","var","delta ")) 

gg.2 <- data.frame(round(na$"N J"$time,4)
                   , na$n.risk[,2][na$time %in% na$"N J"$time]
                   , unname(aperm(na$n.event,c(3,2,1))[,1,2])[na$time %in% na$"N J"$time]
                   , na$n.cens[,2][na$time %in% na$"N J"$time]
                   , round(na$"N J"$na,4), round(na$"N J"$var.aalen,3)
                   , round(aperm (etm.0$delta.na,c(3,2,1))[,1,2][na$time %in% na$"N J"$time],4))
dimnames(gg.2) <- list(1:nrow(gg.2), 
                       c("age","nrisk","nevent","ncens","cumhaz","var ","delta"))


# non-parametric
Dmsm <- Biograph.msm(data)
twoway2.q <- rbind(c(-0.025, 0.025),c(0.2,-0.2))
crudeinits.msm(state ~ date, ID, data=Dmsm, qmatrix=twoway2.q) 
GLHS.msm.y <- msm(state ~ date, subject=ID, data = Dmsm
                  , use.deriv=TRUE, exacttimes=TRUE
                  , qmatrix = twoway2.q, obstype=2
                  , control=list(trace=2,REPORT=1, abstol=0.0000005)
                  , method="BFGS")
GLHS.msm.y

tra2 <- attr(D$D, "param")$trans_possible
etm.2 <- etm(data=D$D, c("J","N"), tra2, "cens", s=0)
summary(etm.2)$"J N" 
summary(etm.2)$"N J"

age.points <- c(18,25,30,35)
landmark.etm <- lapply(age.points, function (reference.age) {
  etm(data=D$D, state.names=c("J","N"), tra=tra,"cens", s=reference.age) 
  })

# extracting parameters
pams.GLHS <- Parameters(GLHS)
pams.GLHS$nntrans
pams.GLHS$trans_possible
pams.GLHS$nsample
# etc

stsps.GLHS <- StateSpace(GLHS)
stsps.GLHS

# ID with longest path
GLHS$ID[nchar(GLHS$path)== max(nchar(GLHS$path))]
which(nchar(GLHS$path)== max(nchar(GLHS$path)))

locpat<- locpath(GLHS)
GLHS[,(locpat+1):ncol(GLHS)]

# Age operations
agetrans <- AgeTrans(Bdata=GLHS)
agetrans$ages[rownames(agetrans$ages)%in% c(3,208),]
namage <- c(pams.GLHS$iagelow:pams.GLHS$iagehigh) 
censored_by_age <- table(cut(agetrans$agecens
                             , breaks=namage
                             , include.lowest=TRUE
                             , right=FALSE))
table(cut(agetrans$agecens
          , breaks=namage
          , include.lowest=TRUE
          , right=FALSE)
      , GLHS$sex)

zmean <- apply(agetrans$ages,2,function(x) mean(x,na.rm=T))

occup <- Occup(Bdata=GLHS)
plot(x=occup$state_occup
      , namstates.desired=c("N","J","Censored")
      , colours=c("red","green","lightgrey")
      , title="States occupancies. GLHS"
      , area=TRUE, xmin=10, xmax=55)


z <- TransitionAB(GLHS,"NJ")
zzz <- as.data.frame(cbind(ID=GLHS$ID
                           , cohort=GLHS$cohort
                           , sex=GLHS$sex
                           , Entry=z$age))
zzz$cohort <- factor(zzz$cohort
                     , labels=c("1929-31","1939-41","1949-51"))
zzz$sex <- factor(zzz$sex
                  , labels =c("Males","Females"))
densityplot(~Entry|sex,data=zzz, groups=cohort
            , plot.points="rug"
            , main="Age at labour market entry"
            , sub= paste("Total number of entries with known covariates is "
                         , length(na.omit(zzz$Entry)),sep="")
            , xlab="Age"
            , scale=list(x=list(alternating=FALSE))
            , ref=TRUE
            , auto.key=TRUE)


z <- TransitionAB(Bdata=GLHS,"NJ")
GLHS$age_at_LMentry <- z$age
qplot(ID, age_at_LMentry
      , data=GLHS, colour=cohort
      , shape=sex)

p <- ggplot(GLHS,aes(x=ID,y=age_at_LMentry
                     , colour=cohort,shape=sex)) 
p + geom_point()

colours=c("1929-31"="red"
          , "1939-41"="black"
          ,"1949-51"="purple")
p + geom_point (aes(colour=cohort)) +
  scale_colour_manual(values=colours)

GLHS.e <- GLHS 
GLHS.e$edu2<- factor(ifelse(GLHS$edu<=11,1,2)
                     , labels=c("-LowerSec","Middle+"))
qplot(ID,age_at_LMentry
      , data=GLHS.e
      , colour=cohort
      , shape=sex
      , facets=edu2~.)

p <- ggplot(GLHS.e
            , aes(x=ID
                  ,y=age_at_LMentry
                  ,colour=cohort
                  ,shape=sex))
p + geom_point() + facet_grid(edu2~.)

qplot(age_at_LMentry
      , data=GLHS
      , geom="histogram"
      , binwidth=1
      , fill=cohort)

qplot(age_at_LMentry
      , data=GLHS
      , geom="histogram"
      , binwidth=1
      , fill=cohort
      , facets=sex~.)


D <- Biograph.long(GLHS)
DE <- D$Depisode
DE$id <- 1:nrow(DE)
DE$Duration <- DE$Tstop-DE$Tstart 
DE$StateOccupied <- DE$OR 
DE$Status <- factor(DE$status
                    , labels=c("Open","Closed"))

p.e <- ggplot(DE
              , aes(x=id
                    ,y=Duration
                    ,shape=Status
                    , colour=StateOccupied))
p.e + geom_point(
  aes(shape=Status
      , colour=StateOccupied)) + 
  scale_shape_manual(values=c(1,19)) + 
  scale_colour_manual(values=c("N"="red","J"="blue")) + 
  theme_bw() +
  labs(title="Durations of episodes in months")  +
  theme(plot.title=element_text(colour="red"
                                , size="12"
                                , face="bold"
                                , hjust=0)
        , plot.background=element_rect(fill="lightskyblue1"
                                     , colour="black"
                                     , size=5)
        )

z <- Lexispoints(Bdata=GLHS, transition="NJ"
                 , title="Calendar time and age at labour market entry"
                 , cov="sex", legend="topleft")

z <- Lexispoints(Bdata=GLHS, transition="JN"
                 , title="Calendar time and age at exit from first job"
                 , cov="sex", legend="topleft")

z <- Lexis.points(Bdata=GLHS, transition="NJ"
                  , title="Labour market entry by sex and cohort"
                  , cov="sex", group="cohort"
                  , legend.pos=c(0.9,0.95), pdf=FALSE)

subjectsID <- c(1,19,46,208) 
title1 <- "Lifelines for selection of respondents." 
z <- Lexislines.episodes(GLHS
                         , D$Depisode
                         , subjectsID
                         , title1)

GLHS.yr <- date_b(Bdata=GLHS
                  , selectday=1
                  , format.out="year")
D <- Biograph.long(GLHS.yr) 
subjects <- c(1,78,120,208) 
z <- Lexis.lines(Bdata=GLHS.yr
                 , Dlong=D$Depisode
                 , subjectsID = subjects
                 , title = " ")

w <- LexisOccExp(Bdata=GLHS,transition= "JN"
                 , nyear=5)

z<- plot(x=occup$state_occup
         , namstates.desired=c("N","J","Censored")
         , colours=c("red","green","lightgrey")
         , title="States occupancies. GLHS"
         , area=TRUE, xmin=10, xmax=55)

DTraMineR <- seqconc(occup$st_age_1,sep="-") 
namst <- c(Parameters(GLHS)$namstates, "Censored") 
D.seq <- seqdef(DTraMineR,states=namst)

seqplot(D.seq, type="d"
        , title="State distribution. GLHS"
        , ylab="Count"
        , xtlab=0:54
        , group=GLHS$sex)

seqiplot(D.seq,tlim=GLHS$ID%in%c(1,20,208))

n <- 10
seqfplot(D.seq
        , group=GLHS$sex
        , tlim=1:n
        , title="Sequence frequency plot.GLHS"
        , xtlab=c(0:54)
        , ltext=c("N","J","Censored")
        , las=1
        , ylab=paste(n
                     , " most frequent sequences (%)"
                     ,sep="") )

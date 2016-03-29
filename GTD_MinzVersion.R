library(dplyr)
library(reshape2)
library(lattice)
library(ggplot2)

gtd <- read.csv("gtd.csv")
gtdwe <- gtd[gtd$region_txt == "Western Europe" &
               gtd$country_txt %in% c("Italy"
                                        , "France"
                                        , "Germany"
                                        , "United Kingdom"
                                        , "Spain") & gtd$nkill > 0
             
             , c("eventid", "iyear", "country_txt", "natlty1_txt"
                 , "natlty2_txt", "natlty3_txt", "nkill", "gname")]

countries <- as.vector(unique(gtdwe$country_txt))
tergroups <- as.vector(unique(gtdwe$gname))
gtdwe <- within(gtdwe, {
  country <- factor(as.vector(country_txt), levels = countries)
  tergroup <- factor(as.vector(gname), levels = tergroups)
})


gtdfatals <- tapply(gtdwe$nkill, INDEX = list(gtdwe$iyear, gtdwe$country), FUN = sum, na.rm = TRUE)
gtdfatals <- data.frame(year = rownames(gtdfatals), gtdfatals)
gtdfatals <- melt(id.vars = "year"
                  , variable.name = "country"
                  , value.name = "fatalities"
                  , data = gtdfatals)
gtdfatals <- gtdfatals[!is.na(gtdfatals$fatalities),]
gtdfatals <- within(gtdfatals, {
  year <- factor(year, rev(levels(year)))
})

barchart(year~fatalities, group = country, stack = TRUE
         , data = gtdfatals
         , horizontal = TRUE
         , auto.key = TRUE
)

gg <- ggplot(data = gtdfatals
             , aes(x = year, y = fatalities, fill = country))
gg + geom_bar(stat = "identity") + coord_flip() + theme_bw()

gtdgroups <- tapply(gtdwe$nkill, INDEX = list(gtdwe$iyear, gtdwe$gname), FUN = sum, na.rm = TRUE)
gtdgroups <- data.frame(year = rownames(gtdgroups), gtdgroups)
gtdgroups <- melt(id.vars = "year"
                  , variable.name = "tergroup"
                  , value.name = "fatalities"
                  , data = gtdgroups)
gtdgroups <- gtdgroups[!is.na(gtdgroups$fatalities),]
gtdgroups <- within(gtdgroups, {
  year <- factor(year, rev(levels(year)))
  tergroup <- factor(tergroup, levels = unique(tergroup))
  levels(tergroup) <- c(levels(tergroup), "Lockerbie")
})

gtdgroups[gtdgroups$year == 1988 &
            gtdgroups$tergroup == "Unknown"
          , "tergroup"] <- "Lockerbie"

tervars <- read.csv("tervars.csv")
tervars$X <- NULL

gtdgroups <- left_join(gtdgroups, tervars) %>%
  mutate(tergroup = factor(tergroup))

gtdgroups <- within(gtdgroups, {
  internationalextremism <- factor(intext, labels = c("No", "Yes"))
  adjustedlocalism <- fatalities * (1 - 0.9 * local)
  adjustedjckl <- adjustedlocalism * (1 - 0.5 * jckl)
  adjustedmemory <- fatalities  * (1 - as.numeric(gtdgroups$year)/45)
  adjustedall <- adjustedjckl * (1 - as.numeric(gtdgroups$year)/45)
})

gg <- ggplot(data = gtdgroups
             , aes(x = year, y = fatalities))
gg + geom_bar(stat = "identity") + coord_flip() + theme_bw()
gg + geom_bar(stat = "identity", aes(fill = internationalextremism)) +
  coord_flip() + theme_bw() + theme(legend.position="top")

gg + geom_bar(stat = "identity", aes(y = gtdgroups$adjustedjckl, fill = internationalextremism)) +
  coord_flip() + theme_bw() + theme(legend.position="top")

gg + geom_bar(stat = "identity", aes(y = gtdgroups$adjustedmemory, fill = internationalextremism)) +
  coord_flip() + theme_bw() + theme(legend.position="top")

gg + geom_bar(stat = "identity", aes(y = gtdgroups$adjustedall, fill = internationalextremism)) +
  coord_flip() + theme_bw() + theme(legend.position="top")
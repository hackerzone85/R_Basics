gtdgroups$yr <- as.numeric(as.character(gtdgroups$year)) - 1970
gtdgroups$Year <- as.numeric(as.character(gtdgroups$year))
gtdgroups$ieCols <- ifelse(gtdgroups$internationalextremism == "No"
                           , "violetred1", "darkorchid4")
gtd.grid <- expand.grid(0:50, c("No", "Yes"))
names(gtd.grid) <- c("yr", "internationalextremism")


gtd.glm1 <- glm(fatalities~yr * internationalextremism
               , data = gtdgroups, family = Gamma(link=log))

anova(gtd.glm1, test = "Chisq")

gtd.preds1 <- data.frame(fatalities = predict(gtd.glm1, gtd.grid
                                              , type = "response"
                                              , se = TRUE), gtd.grid)
gtd.preds1 <- within(gtd.preds1, {
  ci.lwr <- fatalities.fit - 1.96 * fatalities.se.fit
  ci.upr <- fatalities.fit + 1.96 * fatalities.se.fit
  Year <- yr + 1970
  ieCols <- ifelse(internationalextremism == "No"
                   , "violetred1", "darkorchid4")
})

plot(fatalities ~ Year, data = gtdgroups, col = ieCols, pch = ".")

with(gtd.preds1[gtd.preds1$internationalextremism == "No",]
     , polygon(c(Year, rev(Year))
               , c(ci.upr, rev(ci.lwr))
               , col=rgb(1,0,0,0.4), border=NA))
lines(fatalities.fit ~ Year, data = gtd.preds1, col = ieCols
      , subset = internationalextremism == "No")

with(gtd.preds1[gtd.preds1$internationalextremism == "Yes",]
     , polygon(c(Year, rev(Year))
               , c(ci.upr, rev(ci.lwr))
               , col=rgb(0,0,1,0.2), border=NA))
lines(fatalities.fit ~ Year, data = gtd.preds1, col = ieCols
      , subset = internationalextremism == "Yes")






gtd.gg <- ggplot(data = gtdgroups, aes(x = yr, y = fatalities
                                       , colour = internationalextremism)) + 
  #coord_flip() + 
  theme_bw() + theme(legend.position="top"
                                    , axis.text.y = element_text(size=6))
gtd.gg + geom_point(alpha = 0.4) +
  geom_smooth(method = glm, method.args = list(family = poisson)
              , size = 2)
gtd.gg + geom_path(data = gtd.preds, aes(x = year, y = fatalities, colour = internationalextremism))

gtd.preds2 <- data.frame(fatalities = predict(gtd.glm2, gtd.grid
                                              , type = "response"
                                              , se = TRUE), gtd.grid)
gtd.preds2 <- within(gtd.preds2, {
  ci.lwr <- fatalities.fit - 1.96 * fatalities.se.fit
  ci.upr <- fatalities.fit + 1.96 * fatalities.se.fit
  Year <- yr + 1970
  ieCols <- ifelse(internationalextremism == "No"
                   , "violetred1", "darkorchid4")
})
gtd.glm2 <- glm(fatalities~poly(yr,2) * internationalextremism
                , data = gtdgroups, family = poisson)
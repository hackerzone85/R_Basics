gtdgroups$yr <- as.numeric(as.character(gtdgroups$year)) - 1970
gtdgroups$Year <- as.numeric(as.character(gtdgroups$year))
gtdgroups$ieCols <- ifelse(gtdgroups$internationalextremism == "No"
                           , "violetred1", "darkorchid4")

gtd.glm1 <- glm(fatalities~yr * internationalextremism
               , data = gtdgroups, family = poisson)

gtd.glm2 <- glm(fatalities~poly(yr,2) * internationalextremism
               , data = gtdgroups, family = poisson)

anova(gtd.glm2, test = "Chisq")
exp(coef(gtd.glm2))

gtd.grid <- expand.grid(0:45, c("No", "Yes"))
names(gtd.grid) <- c("yr", "internationalextremism")

gtd.preds1 <- data.frame(fatalities = predict(gtd.glm1, gtd.grid), gtd.grid)
gtd.preds1 <- within(gtd.preds1, {
  Year <- yr + 1970
  ieCols <- ifelse(internationalextremism == "No"
                   , "violetred1", "darkorchid4")
})

gtd.preds2 <- data.frame(fatalities = predict(gtd.glm2, gtd.grid), gtd.grid)
gtd.preds2 <- within(gtd.preds2, {
  Year <- yr + 1970
  ieCols <- ifelse(internationalextremism == "No"
                   , "violetred1", "darkorchid4")
})

plot(fatalities ~ Year, data = gtdgroups, col = ieCols, type = "n")
lines(exp(fatalities) ~ Year, data = gtd.preds1, col = ieCols
      , subset = internationalextremism == "No")
lines(exp(fatalities) ~ Year, data = gtd.preds1, col = ieCols
      , subset = internationalextremism == "Yes")
lines(exp(fatalities) ~ Year, data = gtd.preds2, col = ieCols
      , subset = internationalextremism == "No")
lines(exp(fatalities) ~ Year, data = gtd.preds2, col = ieCols
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



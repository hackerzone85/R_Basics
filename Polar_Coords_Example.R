Gini <- ggplot(prr, aes(x=x,y=fit,fill=x))
Gini <- Gini + geom_bar(stat="identity",position="dodge")+coord_polar()+scale_fill_continuous(high="darkred",low="darkgreen") 
Gini <- Gini + theme(panel.background=element_rect(fill="white", colour = "white", size=0),axis.text=element_blank(),axis.title=element_blank(),legend.title=element_blank())
x <- c(1:dim(GiniData)[1]) 


Gini + geom_text(
  aes(
    x=x,
    label=paste(GiniData$GiniIndex,GiniData$Country),
    angle=270-x/134*360,
    hjust=1),
  y=GiniData$GiniIndex+3,
  size=3,
  vjust=0)
# Create some sample data
CV_1 <- 0.2
CV_2 <- 0.3
Mean <- 65
sigma_1 <- sqrt(log(1 + CV_1^2))
mu_1 <- log(Mean) - sigma_1^2 / 2
sigma_2 <- sqrt(log(1 + CV_2^2))
mu_2 <- log(Mean) - sigma_2^2 / 2
q <- c(0.25, 0.5, 0.75, 0.9, 0.95) 
SummaryTable <- data.frame(
  Quantile=paste0(100*q,"%ile"), 
  Loss_1=round(qlnorm(q, mu_1, sigma_1),1),
  Loss_2=round(qlnorm(q, mu_2, sigma_2),1)
)
# Create a plot 
library(ggplot2)
plt <- ggplot(data.frame(x=c(20, 150)), aes(x)) + 
  stat_function(fun=function(x) dlnorm(x, mu_1, sigma_1), 
                aes(colour="CV_1")) + 
  stat_function(fun=function(x) dlnorm(x, mu_2, sigma_2), 
                aes(colour="CV_2")) +
  scale_colour_discrete(name = "CV", 
                        labels=c(expression(CV[1]), expression(CV[2]))) +
  xlab("Loss") +  
  ylab("Density") +
  ggtitle(paste0("Two log-normal distributions with same mean of ",
                 Mean,", but different CVs")) 
# Create a table plot
library(gridExtra)
names(SummaryTable) <- c("Quantile", 
                         expression(Loss(CV[1])),
                         expression(Loss(CV[2])))
# Set theme to allow for plotmath expressions
tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
tbl <- tableGrob(SummaryTable, rows=NULL, theme=tt)
# Plot chart and table into one object
grid.arrange(plt, tbl,
             nrow=2,
             as.table=TRUE,
             heights=c(3,1))
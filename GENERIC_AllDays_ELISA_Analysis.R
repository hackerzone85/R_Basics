# using auc in MESS
require(MESS)

##### Input data for day 10 #####
# Protein file
data_10_prot=read.csv("HMBD-002_Day_10_ELISA_protein.csv")

# Peptide file
data_10_pept=read.csv("HMBD-002_Day_10_ELISA_peptide.csv")

##### Input data for day 23 #####
# Protein file
data_23_prot=read.csv("HMBD-002_Day_23_ELISA_protein.csv")

# Peptide file
data_23_pept=read.csv("HMBD-002_Day_23_ELISA_peptide.csv")


##### Input data for day 37 #####
# Protein file
data_37_prot=read.csv("HMBD-002_Day_37_ELISA_protein.csv")

# Peptide file 
data_37_pept=read.csv("HMBD-002_Day_37_ELISA_peptide.csv")

##### Input data for day 46 #####
# Protein file
data_46_prot=read.csv("HMBD-002_Day_46_ELISA_protein.csv")
data_46_pept=read.csv("HMBD-002_Day_46_ELISA_peptide.csv")

##### There are some excel files that contain information for the dead mouse and few others that do not. 
# Ideally you will like to unify them 

# Assign the dilution tags
x_labels <- data_37_prot[,1]
# and for each tag assign a x coordinate
dat=matrix(data=NA, nrow=40, ncol=16)
x = c(1:length(x_labels))

#### Output name and location of the pdf file 
somePDFPath = "HMBD-002_AllDays.pdf"

#### Open the pdf file where you want to add the graphs
pdf(file=somePDFPath)# This is the default size
		     # you could tune the dimensions by adding 
		     #,width=30,height=20)  

# margens in the plots
par(mar = c(5,2.5,2,1))

# Assign the mouse pair as an entity 
for(mouse_pair in seq(1,40))
{
j <- 2* mouse_pair

m <- matrix(c(1,1,2,2,3,3,4,4,5,5,5,5),nrow = 3,ncol = 4,byrow = TRUE)
layout(mat = m,heights = c(1.5,1.5))

# margens in the plots
par(mar = c(5,2.5,2,1))

###### Day 10

# Mouse A
y1 <- data_10_prot[,j]
y2 <- data_10_pept[,j]
# Mouse B
y3 <- data_10_prot[,j+1]
y4 <- data_10_pept[,j+1]

# Draw an empty plot
plot(0,0,type="n",xlim=c(0,8), ylim=c(0,1.5),ylab = "OD 450",xaxt="n",xlab="Diultion",main="Day 10")
axis(side=1, at=x, labels=x_labels)
# Plot Title
title(paste("Mouse Pair: ",mouse_pair), outer=TRUE,line=-1,col.main="blue")

# Estimate the AUC for each data set
m1_prot_10=round(auc(x,y1, type = 'spline'),3)
m1_pept_10=round(auc(x,y2, type = 'spline'),3)
m2_prot_10=round(auc(x,y3, type = 'spline'),3)
m2_pept_10=round(auc(x,y4, type = 'spline'),3)

lines(x,y1, type = "o",col="red",xaxt="n",lwd=2)
lines(x,y2, type = "o",col="red",xaxt="n",pch=22,lty=2,lwd=2)
lines(x,y3, type = "o",col="blue",xaxt="n",lwd=2)
lines(x,y4, type = "o",col="blue",xaxt="n",pch=22,lty=2,lwd=2)

legend("topright",title="AUC",legend=c(m1_prot_10,m1_pept_10,m2_prot_10,m2_pept_10),lty=c(1,2,1,2),col=c(2,2,4,4),horiz=FALSE,
       bty="n")

dat[mouse_pair,1]<-m1_prot_10
dat[mouse_pair,9]<-m1_pept_10
dat[mouse_pair,5]<-m2_prot_10
dat[mouse_pair,13]<-m2_pept_10

######### Day 23

# Mouse A
y1 <- data_23_prot[,j]
y2 <- data_23_pept[,j]
# Mouse B
y3 <- data_23_prot[,j+1]
y4 <- data_23_pept[,j+1]

plot(0,0,type="n",xlim=c(0,8), ylim=c(0,1.5),xlab="Dilution",ylab = "OD 450",xaxt="n",main="Day 23")
axis(side=1, at=x, labels=x_labels)

lines(x,y1, type = "o",col="red",xaxt="n",lwd=2)
lines(x,y2, type = "o",col="red",xaxt="n",pch=22,lty=2,lwd=2)
lines(x,y3, type = "o",col="blue",xaxt="n",lwd=2)
lines(x,y4, type = "o",col="blue",xaxt="n",pch=22,lty=2,lwd=2)

m1_prot_23=round(auc(x,y1, type = 'spline'),3)
m1_pept_23=round(auc(x,y2, type = 'spline'),3)
m2_prot_23=round(auc(x,y3, type = 'spline'),3)
m2_pept_23=round(auc(x,y4, type = 'spline'),3)


legend("topright",title="AUC",legend=c(m1_prot_23,m1_pept_23,m2_prot_23,m2_pept_23),lty=c(1,2,1,2),col=c(2,2,4,4),horiz=FALSE,
       bty="n")

dat[mouse_pair,2]<-m1_prot_23
dat[mouse_pair,10]<-m1_pept_23
dat[mouse_pair,6]<-m2_prot_23
dat[mouse_pair,14]<-m2_pept_23

####### Day 37

# Mouse A
y1 <- data_37_prot[,j]
y2 <- data_37_pept[,j]
# Mouse B
y3 <- data_37_prot[,j+1]
y4 <- data_37_pept[,j+1]

x_labels <- data_37_pept[,1]
plot(0,0,type="n",xlim=c(0,8), ylim=c(0,1.5),xlab="Dilution",ylab = "OD 450",xaxt="n",main="Day 37")
axis(side=1, at=x, labels=x_labels,las=2)

lines(x,y1, type = "o",col="red",xaxt="n",lwd=2)
lines(x,y2, type = "o",col="red",xaxt="n",pch=22,lty=2,lwd=2)
lines(x,y3, type = "o",col="blue",xaxt="n",lwd=2)
lines(x,y4, type = "o",col="blue",xaxt="n",pch=22,lty=2,lwd=2)

m1_prot_37=round(auc(x,y1, type = 'spline'),3)
m1_pept_37=round(auc(x,y2, type = 'spline'),3)
m2_prot_37=round(auc(x,y3, type = 'spline'),3)
m2_pept_37=round(auc(x,y4, type = 'spline'),3)

legend("topright",title="AUC",legend=c(m1_prot_37,m1_pept_37,m2_prot_37,m2_pept_37),lty=c(1,2,1,2),col=c(2,2,4,4),horiz=FALSE,
       bty="n")

dat[mouse_pair,3]<-m1_prot_37
dat[mouse_pair,11]<-m1_pept_37
dat[mouse_pair,7]<-m2_prot_37
dat[mouse_pair,15]<-m2_pept_37

####### Day 46

# Mouse A
y1 <- data_46_prot[,j]
y2 <- data_46_pept[,j]
# Mouse B
y3 <- data_46_prot[,j+1]
y4 <- data_46_pept[,j+1]

plot(0,0,type="n",xlim=c(0,8), xlab="Dilution",ylim=c(0,1.5),xaxt="n",main="Day 46")
axis(side=1, at=x, labels=x_labels,las=2)

lines(x,y1, type = "o",col="red",xaxt="n",lwd=2)
lines(x,y2, type = "o",col="red",xaxt="n",pch=22,lty=2,lwd=2)
lines(x,y3, type = "o",col="blue",xaxt="n",lwd=2)
lines(x,y4, type = "o",col="blue",xaxt="n",pch=22,lty=2,lwd=2)

m1_prot_46=round(auc(x,y1, type = 'spline'),3)
m1_pept_46=round(auc(x,y2, type = 'spline'),3)
m2_prot_46=round(auc(x,y3, type = 'spline'),3)
m2_pept_46=round(auc(x,y4, type = 'spline'),3)

legend("topright",title="AUC",legend=c(m1_prot_46,m1_pept_46,m2_prot_46,m2_pept_46),lty=c(1,2,1,2),col=c(2,2,4,4),horiz=FALSE,
bty="n")

dat[mouse_pair,4]<-m1_prot_46
dat[mouse_pair,12]<-m1_pept_46
dat[mouse_pair,8]<-m2_prot_46
dat[mouse_pair,16]<-m2_pept_46

# Plot Legend
# For the legend I need to read the title of the colunm not always M1 and M2
#colnames(data_10_prot)
plot(1, type = "n", axes=FALSE, xlab="", ylab="")

plot_colors <- c("red","red", "blue", "blue")
line_types <- c(1,2,1,2)
legend(x = "top",inset = 0,bty = 'n',
	legend = c(paste(colnames(data_10_prot)[j], "Protein"),paste(colnames(data_10_pept)[j],"Peptide"),paste(colnames(data_10_prot)[(j+1)],"Protein"),paste(colnames(data_10_pept)[(j+1)],"Peptide")),
       col=plot_colors, lwd=2, cex=1.2, horiz = TRUE,lty = line_types)

}

dev.off()


###### How to plot the matrix afterwards

labelsx<-c("m1-prot_10","m1-prot_23","m1-prot_37","m1-prot_46","m2-prot_10","m2-prot_23","m2-prot_37","m2-prot_46","m1-pept_10","m1-pept_23","m1-pept_37","m1-pept_46","m2-pept_10","m2-pept_23","m2-pept_37","m2-pept_46")
colnames(dat) <-labelsx 
rownames(dat) <-seq(1,40)


###### How to plot the matrix afterwards

dat<-read.table("mymatrix.txt")


pdf(file="matrix.pdf")# This is the default size

par(mfrow=c(6,3))
par(mar = rep(2, 4))


for(i in seq(1,40))
{
  lab<-paste("Mouse Pair: ",i)
  plot(seq(1,4),dat[i,1:4],typ="b",xlim=c(1,16),ylim=c(0,10),col="red",lwd=2,main=lab,xlab=NULL,ylab="",xaxt="n")
  points(seq(5,8),dat[i,9:12],typ="b",col="blue",lwd=2)
  points(seq(9,12),dat[i,5:8],typ="b",col="green",lwd=2,pch=22,lty=2)
  points(seq(13,16),dat[i,13:16],typ="b",col="orange",lwd=2,pch=22,lty=2)
}
dev.off()

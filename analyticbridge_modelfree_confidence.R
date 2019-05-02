set.seed(3.1416)
x <- rnorm(1000000,10,2)
x <- rpois(1000000,10)
x <- rgamma(1000000,10, 1)

analytic_teorem <- function (N,x,med,desv)
{
  meansamples <- rep(NA,N)
  for(i in 1:N){
    meansamples[i]  <- mean(sample(x, length(x)/N, replace = TRUE))
  }
  meansamples <- sort(meansamples)
  confidence <- rep(NA,N/2)
  p_lower <- rep(NA,N/2)
  p_upper <- rep(NA,N/2)
  
  for(k in 1:N/2){
    confidence[k] <- 2*k/(N+1)
    p_lower[k] <- meansamples[k]
    p_upper[k] <- meansamples[N-k+1]
  }
  
  mean_t <- (meansamples[k] + meansamples[N-k+1])/2
  
  x_a <- seq(1:(N/2))
  par(mar=c(5,4,4,5)+.1)
  plot(x_a, p_upper,type="l", ylim=range(c(med-1*desv,med+1*
                                             desv)),col="blue",xlab="K",ylab="Values")
  
  lines(x_a, p_lower,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
  
  par(new=TRUE)
  
  plot(confidence*(N/2),rep(mean_t,N/2),ylim=range(c(med-1*desv,med+1*desv)),type="l",
       
       col="black",xaxt="n",yaxt="n",ylab='',xlab='',lty=2)
  axis(3,seq(from=0,to=1,by=.25)*(N/2),las=0,at=seq(from=0,to=1,by=.25)*(N/2),labels=c(1,0.75,0.50,0.25,0))
  mtext("Confidence",side=3,line=3)
  legend("bottomright",col=c("red","blue"),lty=1,legend=c("Lower bound","Upper Bound"))
  legend("topright",legend=c(paste("N=",as.character(N))))
}

for(i in seq(1000,10000,100)) analytic_teorem(i,x,10,2)

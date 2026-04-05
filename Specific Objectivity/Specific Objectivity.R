#Specific Objectivity

icccal <- function(b, a, c)  {
  if (missing(c)) c <- 0  
  if (missing(a)) a <- 1  
  theta <- seq(-3, 3, .5)  
  P <- c + (1 - c)/ (1 + exp(-a * (theta - b))) 
  Logits <- log((P-c)/(1-P))
  data.frame(theta, P, Logits)  } 

#ICC function with vertical line at item difficulty location

icc <- function(b, a, c, col) {
  if (missing(c)) c <- 0  
  if (missing(a)) a <- 1 
  if (missing(col)) col <- "black" 
  par(lab=c(7,3,3)) 
  theta <- seq(-3, 3, .1)  
  P <- c + (1 - c)/ (1 + exp(-a * (theta - b)))  
  plot(theta, P, type="l", axes=FALSE, col=col,
       ylim=c(0,1),  xlab="Ability", ylab="Probability of Correct Response") 
  axis(side=1, at=c(-3:3))
  axis(side=2, at=seq(0, 1, by=.1))
  } 

prob <- function(b, a, c, theta) {
  if (missing(c)) c <- 0  
  if (missing(a)) a <- 1 
  P <- c + (1 - c)/ (1 + exp(-a*(theta - b)))  
  P  
} 

logits <-function(b, a, c, theta) {
  if (missing(c)) c <- 0  
  if (missing(a)) a <- 1 
  P <- c + (1 - c)/ (1 + exp(-a*(theta - b)))  
  Logits <- log((P-c)/(1-P))
  Logits
} 

# Example: Comparing difference in logits for two different people with using
# two different item that do not fit the Rasch model

logits(-.5,.6,.15,-.35)-logits(-.5,.6,.15,-.85)
logits(-.5,1,.25,-.35)-logits(-.5,1,.25,-.85)


##Compare Rasch vs 2PL ICCs

#2PL Example with crossing curves

icc(a=1,b=-.5, c=0,col="blue")
par(new=T)
icc(a=2,b=.0,c=0,col="magenta")

#Rasch Model Fits
 
icc(a=1,b=-.5, c=0,col="blue")
par(new=T)
icc(a=1,b=.0,c=0,col="magenta")

#Compare Rasch vs 2PL in terms of logits

plot(x1$theta,x1$Logits,type="l",col="blue",ylim=c(-3,3),
     ylab="logit of correct item response",xlab="Theta")
par(new=T)
plot(x2$theta,x2$Logits,type="l",col="magenta",ylab="",xlab="",ylim=c(-3,3))

plot(x1$theta,x1$Logits,type="l",col="blue",ylim=c(-3,3),
     ylab="logit of correct item response",xlab="Theta")
par(new=T)
plot(x2$theta,x2$Logits,type="l",col="magenta",ylab="",xlab="",ylim=c(-3,3))

##Answers to in-class activity on slides

#Calculate probs and logits for 2P
x1.2<-icccal(a=1,b=-.5,c=0)
x1.2
x2.2<-icccal(a=2,b=0,c=0)
x2.2

#Calculate probs and logits for Rasch
x1.r<-icccal(-.5,1,0)
x1.r
x2.r<-icccal(0,1,0)
x2.r

##Ideal Concept of trade-offs defining an interval scale

diff<-seq(from=-4,to=4, by = .5)

ldf<-data.frame(matrix(rep(0,117),nrow=13,ncol=9))

for (i in 1:9) {
  x<-icccal(diff[i])
  ldf[,i]<-x$Logits
  }

plot(x$theta,ldf[,1],type="l",col="blue",xlim=c(-3,3),ylim=c(-4,6),
     ylab="logit of correct item response",xlab="Theta")
par(new=T)
plot(x$theta,ldf[,2],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,3],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,4],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,5],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,6],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,7],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,8],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,9],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=F)
abline(h=c(1),lty=2)
abline(v=c(-1,0,1),lty=2)


##Reality of trade-offs defining an interval scale

diff<-c(-2,-1,-.5,0,.1,.2,.3,.5,.6,.7,1,1.5,1.7,2)

ldf<-data.frame(matrix(rep(0,13*length(diff)),nrow=13,ncol=length(diff)))

for (i in 1:length(diff)) {
  x<-icccal(diff[i])
  ldf[,i]<-x$Logits
}

plot(x$theta,ldf[,1],type="l",col="blue",xlim=c(-3,3),ylim=c(-4,6),
     ylab="logit of correct item response",xlab="Theta")
par(new=T)
plot(x$theta,ldf[,2],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,3],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,4],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,5],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,6],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,7],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,8],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=T)
plot(x$theta,ldf[,9],type="l",col="blue",ylab="",xlab="",xlim=c(-3,3),ylim=c(-4,6))
par(new=F)
abline(h=c(1),lty=2)
abline(v=c(-1,0,1),lty=2)



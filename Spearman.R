#Spearman-Brown Formula

rel.old<-seq(from=0, to=1,by=.05) 
rel.new2<-2*rel.old/(1+rel.old)
rel.new3<-3*rel.old/(1+(2*rel.old))
rel.new4<-4*rel.old/(1+(3*rel.old))

par(mfrow=c(1,1))
plot(rel.old,rel.new2,xlab="Old Reliability",ylab="New Reliability",main="Doubling Test Length",type="l")

plot(rel.old,rel.new3,xlab="Old Reliability",ylab="New Reliability",main="Tripling Test Length",type="l")
plot(rel.old,rel.new4,xlab="Old Reliability",ylab="New Reliability",main="Quadrupling Test Length",type="l")
par(mfrow=c(1,1))

#Estimating Spearman's g

a<-c(10,8,6,4,2,0)
b<-c(8,5,9,7,0,1)
c<-c(7,9,4,8,1,1)

mean(a)
mean(b)
mean(c)

sd(a)
sd(b)
sd(c)


z.a<-(a-mean(a))/sd(a)
z.b<-(b-mean(b))/sd(b)
z.c<-(c-mean(c))/sd(c)

r.ab<-cor(a,b)
r.ac<-cor(a,c)
r.bc<-cor(b,c)

r.ag<-sqrt(r.ab*r.ac/r.bc)
r.bg<-sqrt(r.ab*r.bc/r.ac)
r.cg<-sqrt(r.ac*r.bc/r.ab)

r.ag
r.bg
r.cg


g.a<-z.a*r.ag
g.b<-z.b*r.bg
g.c<-z.c*r.cg

dat<-cbind(g.a,g.b,g.c)

dat

sem.a<-.6745*sqrt(1-r.ag^2)
sem.b<-.6745*sqrt(1-r.bg^2)
sem.c<-.6745*sqrt(1-r.cg^2)

c(sem.a,sem.b,sem.c)

sem.sa<-.6745*sqrt(r.ag)
sem.sb<-.6745*sqrt(r.bg)
sem.sc<-.6745*sqrt(r.cg)

c(sem.sa,sem.sb,sem.sc)

# Example of attenuation

N<-100

rho <- 0.6
mu1 <- 69; s1 <- 3
mu2 <- 69; s2 <- 3

mu<-c(mu1,mu2)

sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),
                2) # Covariance matrix

library(MASS)
d <- mvrnorm(N, mu = mu, Sigma = sigma ) # from MASS package
colnames(d) <- c("Height1","Height2")

d<-as.data.frame(d)

#names(d)<-c("Height1","Height2")

library(ggplot2)
library(gridExtra)

#Assuming high reliability, r = .9
#SEM height = sqrt(1-.9)*3 = 1
me1h<-2 
#Assuming low reliability, r = .6
#SEM height = sqrt(1-.6)*3 = 2
#SEM weight =  sqrt(1-.6)*10 = 3
me2h<-4; 

p1<-ggplot(data = d,aes(x = Height1,y = Height2)) + 
  geom_point() +theme_bw() +
  xlim(60,85) + ylim(60,85)
  
p2<-ggplot(data = d,aes(x = Height1,y = Height2)) + 
  geom_errorbar(aes(ymin = Height2-me1h,ymax =Height2+me1h),colour="#56B4E9",linetype="dotted") + 
  geom_errorbarh(aes(xmin = Height1-me1h,xmax = Height1+me1h),colour="#56B4E9",linetype="dotted")+
  geom_point() + theme_bw() +
  xlim(60,85) + ylim(60,85)

p3<-ggplot(data = d,aes(x = Height1,y = Height2)) + 
  geom_errorbar(aes(ymin = Height2-me2h,ymax =Height2+me2h),colour="#56B4E9",linetype="dotted") + 
  geom_errorbarh(aes(xmin = Height1-me2h,xmax = Height1+me2h),colour="#56B4E9",linetype="dotted")+
  geom_point() + theme_bw() +  
  xlim(60,85) + ylim(60,85)

grid.arrange(p1, p2, p3, nrow = 1)

d$Height1.2<-d$Height1+rnorm(nrow(d),0,1)
d$Height2.2<-d$Height2+rnorm(nrow(d),0,1)

d$Height1.3<-d$Height1+rnorm(nrow(d),0,2)
d$Weight2.3<-d$Height2+rnorm(nrow(d),0,2)

cor(d$Height1,d$Height2)
cor(d$Height1.2,d$Height2.2)
cor(d$Height1.3,d$Weight2.3)

##1904 "General Intelligence" Data

pitch<-rank(c(8,15,14,13,5,25,10,10,18,14,60,20,40,45,33,25,90,17,24,18,70,17,28,90))
light<-rank(c(4,3,6,4,14,7,19,12,11,30,3,12,5,12,5,4,15,15,26,35,10,42,20,25))
weight<-rank(c(4,4,4,9,7,4,8,10,9,7,10,10,12,9,15,28,5,20,13,14,14,16,17,18))
sensea<-c(6,11,16,1,3,10,8,2,5,21,12,13,4,9,15,17,22,14,19,18,23,24,7,20)
senseb<-c(5,7,10,1,2,14,19,4,6,22,9,12,8,13,18,11,21,20,17,3,24,23,15,16)
clever<-c(2,22,7,1,3,9,12,6,11,19,4,18,8,14,10,17,5,15,24,6,20,23,13,21) 

vill1<-as.data.frame(cbind(pitch,light,weight,sensea,senseb,clever))
 
cor(vill1)

  age<-rank(c(9+7/12,8+7/12,7+3/12,8,8,8+4/12,9+9/12,8+2/12,9,7+11/12,8+2/12,7+2/12,9+11/12,10+2/12,7+1/12,
            9+6/12,7+10/12,8+2/12,7+11/12,7,7+3/12))
  pitch<-rank(c(6,6,16,24,26,35,35,38,42,48,48,67,67,74,77,80,96,96,112,120,120))
  intellect<-c(2,2,1,2,2,2,3,1,3,2,2,1,3,2,1,3,1,2,2,1,1)

  vill2<-as.data.frame(cbind(age,pitch,intellect))
  
  cor(vill2)

age<-c(12+6/12,12+4/12,9+8/12,13+7/12,10+4/12,10+7/12,13+6/12,11+10/12,10+1/12,
       11+1/12,13+4/12,10+6/12,12+3/12,13+1/12,11+1/12,9+9/12,10+4/12,13,10+2/12,
       13,12,12+11/12,13+1/12,10+4/12,10+1/12,12+6/12,10+8/12,12+8/12,9+5/12,11+2/12,
       10+9/12,10+11/12,13+7/12)
pitch<-rank(c(2,3,3,4,4,4,5,5,5,6,7,7,7,8,10,10,11,11,11,11,11,12,14,14,15,15,15,
         18,20,24,50,60,60))
classics1<-c(8,11,19,2,21,23,3,6,29,20,1,26,18,5,22,33,28,4,7,12,17,9,
             10,27,24,14,30,16,32,15,25,31,13)
classics2<-c(7,12,18,2,NA,23,NA,4,26,20,1,24,17,5,19,29,25,3,6,11,16,8,9,21,22,
             13,27,15,NA,14,NA,28,10)
classics3<-c(4,10,15,1,19,22,NA,3,24,18,NA,21,16,5,17,27,23,2,6,11,NA,7,8,14,20,
             12,NA,13,25,9,NA,26,NA)
french1<-c(5,13,21,2,22,26,3,7,23,20,1,27,17,4,19,33,30,6,12,11,16,8,10,24,18,15,
           29,25,31,14,28,32,9)
french2<-c(3,13,19,2,NA,23,NA,6,25,21,1,16,20,4,18,29,27,5,7,11,15,8,9,22,17,14,
           26,24,NA,12,NA,28,10)
french3<-c(3,10,16,1,23,22,NA,5,21,18,NA,13,19,2,17,27,24,4,6,12,NA,7,8,15,14,11,
           NA,20,25,9,NA,26,NA)
english1<-c(4,13,23,2,22,28,3,6,27,21,1,26,25,5,20,33,18,7,8,15,24,9,11,17,29,10,
            30,14,32,16,19,31,12)
english2<-c(3,13,21,2,NA,25,NA,6,26,20,1,19,23,8,17,27,18,4,5,16,22,7,10,11,24,9,
            29,14,NA,15,NA,28,12)
english3<-c(3,11,18,1,20,23,NA,2,22,19,NA,17,21,5,15,27,13,4,8,16,NA,7,9,10,24,6,
            NA,12,26,14,NA,25,NA)
math1<-c(4,12,21,7,21,29,3,9,25,17,1,22,19,5,23,32,30,2,11,6,24,14,10,26,18,8,28,
         20,33,13,15,31,16)
math2<-c(2,13,19,7,NA,25,NA,8,23,16,1,18,17,4,21,29,27,3,9,5,24,12,10,20,15,6,26,
         21,NA,11,NA,28,14)
math3<-c(3,11,17,7,24,23,NA,6,19,15,NA,16,14,1,21,27,22,4,8,2,NA,12,9,18,13,5,NA,20,
         26,10,NA,25,NA)
music<-c(8,9,6,3,16,1,21,NA,7,14,5,11,20,4,18,17,NA,NA,NA,12,15,NA,13,2,NA,10,NA,
         19,NA,NA,NA,22,NA)

prep.all<-as.data.frame(cbind(age,pitch,classics1,classics2,classics3,french1,
                  french2,french3,english1,english2,english3,math1,math2,math3,music))

prep<-subset(prep.all,!is.na(music)==1)

cor(prep,use="pairwise.complete.obs")

classics.avg<-apply(prep[,3:5],1,mean)
french.avg<-apply(prep[,6:8],1,mean)
english.avg<-apply(prep[,9:11],1,mean)
math.avg<-apply(prep[,12:14],1,mean)

avg<-cbind(prep$pitch,classics.avg,french.avg,english.avg,math.avg)

mod<-classics.avg-xx  #rank residualized to adjust for age
cor(mod,prep$pitch,use="pairwise.complete.obs")  #adjusted correlation
cor(classics.avg,prep$pitch,use="pairwise.complete.obs") #unadjusted correlation

par(mfrow=c(1,2))
plot(mod,prep$pitch)
plot(classics.avg,prep$pitch)


cor(avg,use="pairwise.complete.obs")

cor(cbind(prep$pitch,prep$classics1,prep$french1,prep$english1,prep$math1,prep$age),use="pairwise.complete.obs")

##Average intercorrelation among repeated occasions by subjects
  z<-cor(prep[,3:5],use="pairwise.complete.obs") 
  mean(z[lower.tri(z)])
  z<-cor(prep[,6:8],use="pairwise.complete.obs") 
  mean(z[lower.tri(z)])
  z<-cor(prep[,9:11],use="pairwise.complete.obs") 
  mean(z[lower.tri(z)])
  z<-cor(prep[,12:14],use="pairwise.complete.obs") 
  mean(z[lower.tri(z)])
  

##Spearman's Band-Shooting Example: Simulation
  
#The 15 numbers to be shot = truth
  
tbs<-c(42,60,79,39,69,23,60,65,58,52,61,54,66,35,65)


#Male Shooter: Chuck
set.seed(42)
e1<-round(rnorm(15,0,8.6))
e2<-round(rnorm(15,0,8.6))
man1<-tbs+e1
man2<-tbs+e2
cor(man1,man2)

#Female Shooter: Wanda
set.seed(22)
w1<-round(rnorm(15,0,15))
w2<-round(rnorm(15,0,15))
wom1<-tbs+w1
wom2<-tbs+w2
cor(wom1,wom2)

plot(man1,man2,xlab="1st Set of Shots",ylab="2nd Set of Shots",main="Chuck")
text(x=70,y=30,paste("r =",round(cor(man1,man2,use="complete.obs"),2)))
abline(a=0,b=1)

plot(man1,wom1,xlab="Chuck",ylab="Wanda",main="1st Set of Shots")
text(x=30,y=70,paste("r =",round(cor(man1,wom1,use="complete.obs"),2)))
abline(a=0,b=1)

plot(man2,wom2,xlab="Chuck",ylab="Wanda",main="2nd Set of Shots")
text(x=30,y=70,paste("r =",round(cor(man2,wom2,use="complete.obs"),2)))
abline(a=0,b=1)

avg.sch.pl<-apply(cbind(prep$classics1,prep$english1,prep$french1,prep$math1),1,mean)

cor(prep$pitch,avg.sch.pl)

xx<-rank(-prep$age)

cor(avg.sch.pl,xx)
cor(avg.sch.pl,-prep$ag)

#pairs(~pitch+light+weight+sensea+senseb+clever, data=study1)




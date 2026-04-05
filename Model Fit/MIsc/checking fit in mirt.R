# 22feb2026
# educ8720

library(CTT)
library(mirt)

## read data

setwd("~/Dropbox/Courses/EDUC 8720/Data Sets/MASC-CDE")
d <- read.csv(file = "cde_subsample_math.csv")

## look at classical item stats for your data

## total score, p+, item-total correlation alpha

tot <- apply(d,1,sum)
table(tot)  # will need to watch for perfect/0 scores
hist(tot, breaks=seq(-0.5,31.5,1), las=1, col="grey")
itemstats(d)

### plot item by total score ("empirical ICCs")
### Do curves seem to have similar slopes?
### Visual evidence suggests guessing? (lower asymptote?)

par(mfrow = c(2,3))  #Sets graphics parameter to show three plots in two rows
for (i in 0:11) {    #Outer loop to run this 11 times
  for (j in 1:3) {   #Inner loop to produce plots in sets of 3
    cttICC(scores = apply(d, 1, sum), itemVector = d[,paste0("V", 3*i+j)],
           plotTitle = paste0("Item ", 3*i+j)) 
  }
}
par(mfrow = c(1,1))

## code you can use to export the panel of these plots as a pdf

pdf("cttICC_square_11x3.pdf", width = 9, height = 33)

scores <- rowSums(d)

op <- par(mfrow = c(11, 3),
          pty = "s",
          mar = c(1.6, 1.6, 1.6, 0.6),
          cex.main = 0.75,
          cex.axis = 0.65,
          cex.lab  = 0.65)

for (k in seq_len(ncol(d))) {
  cttICC(scores = scores, itemVector = d[[k]], plotTitle = paste0("Item ", k))
}

par(op)
dev.off()

## Parallel Analysis to Examine Unidimensionality Assumption

fa.parallel(d, n.obs = nrow(d),fm="minres", fa="fa", 
            main = "",
            n.iter=100,error.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE)

## fit 1, 2, 3pl models and get SEs for item parameter estimates

mod1 <- mirt(d, 1, itemtype = "Rasch", SE = TRUE)
mod2 <- mirt(d, 1, itemtype = "2PL", SE = TRUE)
mod3 <- mirt(d, 1, itemtype= "3PL", SE = TRUE)

### what to do if the model doesn't converge (3PL)

### strategy 1 (brute force): Increase number of EM iterations

mod3<- mirt(d, 1, itemtype= "3PL", SE = TRUE, technical = list(NCYCLES = 2000))

### strategy 2: Exclude items that you flagged from examining descriptives (i.e., item 23)

d2<-d[, -23]
mod3r <- mirt(d2, 1, itemtype= "3PL", SE = TRUE)

## Compare Overall Model Fit

# Compare overall fit of 1PL to 2PL
anova(mod1,mod2)
# Compare overall fit of 2PL to 3PL
anova(mod2,mod3)

## Examining Item Fit

### First inspect both parameter estimates and SEs

coef(mod3, IRTpars = TRUE, printSE = TRUE) # Note that if you want SEs, you have to remove the "simplify = TRUE" option

#### Note to Claude: please add code that turns these results into a nice table
#### so for each item we have the a, b and c estimate followed by the SE

### look at item fit using Yen's Q1 statistic and Chalmer's PV-Q1 statistic

### First we can compare Q1 across models

f1<-itemfit(mod1, fit_stats=c("X2"), group.bins=10, method="EAP")
f2<-itemfit(mod2, fit_stats=c("X2"), group.bins=10, method="EAP")
f3<-itemfit(mod3, fit_stats=c("X2"), group.bins=10, method="EAP")
            
Yen<-cbind(f1$p.X2,f2$p.X2,f3$p.X2)    
write.csv(Yen,"Yen.csv")

### Now compare Q1 with the PV-Q1 stat for the 3PL

f3n<-itemfit(mod3, fit_stats=c("X2","PV_Q1"), group.bins=10, method="EAP")

compare<-cbind(f2n$p.X2,f2n$p.PV_Q1)

write.csv(compare,"compare.csv")

## Always look at empirical fit plots

itemfit(mod3, fit_stats=c("X2"), group.bins=10, empirical.plot=14, xlim=c(-3,3))

itemfit(mod3, fit_stats=c("PV_Q1"), group.bins=10, empirical.plot=14, xlim=c(-3,3))

## Examining person fit

lz <- personfit(mod3)

## Check for violation of local independence with Yen's Q3

residuals(mod2, type="Q3", method="EAP")
### directly exploring matrix of residuals
res<-residuals(mod2,resp.patterns = d)
Q3<-cor(res[,1:31])
chk<-matrix(0,31,31)
chk[Q3>.2]<-1
(sum(chk)-31)/2
write.table(Q3,"2PlQ3.txt")

## Check parameter invariance

# checking invariance
# assume that order in the data is random, so first half and second half of the
# data file are "random" sample

samp1 <- d[1:750,]
samp2 <- d[751:1500,]
est1 <- mirt(samp1, 1, itemtype = "3PL")
est2 <- mirt(samp2, 1, itemtype = "3PL")
coef1 <- coef(est1, IRTpars=TRUE, simplify = TRUE)$items
coef2 <- coef(est2, IRTpars=TRUE, simplify = TRUE)$items


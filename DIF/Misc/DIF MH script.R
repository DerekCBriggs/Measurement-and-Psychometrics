###########Checking for Item Bias via Differential Item Functioning
###########Using the Mantel-Haenzel Statistic

setwd("~/Dropbox/Courses/EDUC 8720/Data Sets/MASC-CDE")

## Once again we will use the CDE data, but this time I will also 
## include the column that indicates gender of test taker

fulldata<-read.fwf("CTBMathSci.rwo",widths=c(rep(1,76),3,1,6),skip=2)
 
# Note: "V78" in the CDE dataframe provides the gender for each student. 
# A value of 1 = "F", 2 = "M". The default when using maentel haenszel in R
# is for the lower alphanumeric value (females) to be designated the "focal" category 
# and the higher value (males) designated the "reference" category 

cde<-fulldata[,c(1:31,78)]
names(cde)[32]<-"gender"

# To check out unconditional crosstabs of item responses by gender
# Have a look at results in the list object "cross"
# Do we notice items with significant differences in proportion correct
# by gender? 

cross<-list(NULL) #This initializes a new list object

for (i in 1:31){
TAB<-table(cde[,i],cde$gender)
cross[[i]]<-prop.table(TAB,2)  # The function prop.table converts cell counts into proportions
}

MH[c(7,8,20,21,26,28,29)]

#I see differences in favor of males greater than 5% for 
#items 7, 8, 20, 21, 26, 28, 29
#Are these items biased in favor of males (i.e., biased against females)?

##Create "Ability" Grouping Variable

totscore<-apply(cde[,1:31],1,sum)

# Examine totscore to make sure we have 
# large enough bins (sample size per value of totscore)

table(totscore)

# To keep this example relatively simple
# I'll collapse scores for every 20 percentiles of distribution
# 0 to 19, 20 to 39, ..., 80 to 99
# This will leave me with 5 bins in the collapsed grouping
# variable "diftot"

x<-quantile(totscore,probs = seq(0, 1, 0.10))

diftot<-matrix(0,1500,1)

for(i in 1:length(x)-1) {
diftot[(totscore>=x[i] & totscore<x[i+1])]<-i
}
diftot[totscore==max(totscore)]<-length(x)-1

table(diftot) #Compare frequencies for diftot to the freqs for totscore

##Getting an MH statistic, p-value and 95% CI for a given item
   #Remember the the null hypothesis is that the odds of solving item
   #for focal group (females) is same as for reference group (males)
   # If this is true, the odds ratio should be 1
   # Alt Hypothesis is that it is not equal 1
   # If greater than 1, means females are more likely to solve correctly than a comparable male
   # If less than 1, means females are less likely to solve correctly than a comparable male

#Running MH for just the first item on the test

mh<-mantelhaen.test(cde$V1, y = cde$gender, z = diftot, alternative = "two.sided", conf.level = 0.95)

mh

##To create a list with all MH results for ALL items on the test

MH<-list(NULL)  #This initializes a new list object

for (i in 1:31) {
MH[[i]]<-mantelhaen.test(cde[,i], y = cde$gender, z = diftot, alternative = "two.sided", conf.level = 0.95)
}

MH

##To create a vector with just the MH odds ratio estimates

mh.or<-rep(0,31)

for (i in 1:31) {
temp<-mantelhaen.test(cde[,i], y = cde$gender, z = diftot, alternative = "two.sided", conf.level = 0.95)
mh.or[i]<-temp$estimate	
}

hist(mh.or)
abline(v=1,col="blue")

#To transform these into the ETS delta statistic
delta<--2.35*log(mh.or)
#Negative numbers mean reference group (males) found this item easy 
#relative to focal group(females), positive numbers vice-versa
hist(delta)

#ETS uses the following criteria to interpret DIF
# Level A: delta < 1 in absolute value [Feel free to use these.]
sum(abs(delta)<=1) 
# Level B: delta > 1 but < 1.5 in absolute value [Can use if you have to.]
sum(abs(delta)>1 & abs(delta)<=1.5)
# Level C: delta > 1.5 in absolute value [Don't use unless content experts say so.]
sum(abs(delta)>=1.5)


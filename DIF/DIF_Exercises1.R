# Load Required Packages

library(lordif)

#------------------------------------------------------------------------------------------
# Setup
#------------------------------------------------------------------------------------------

# Excercise 1. 
# ~~~~~~~~~~~~

# Set your working directory to the location of your data.
setwd("~/Desktop") 

# Read in your datafile by changing the file name to the name of your dataset. 
DIF.data <- read.csv("DIF.data1.csv") 

#Change the numeric variable "Male" into a Factor (makes crosstabs easier to read)
DIF.data$Male<-factor(DIF.data$Male,levels=c(0,1),labels=c("Female","Male"))

# Below, type some commands (e.g., head, tail) to review your datafile.
head(DIF.data)
tail(DIF.data)
nrow(DIF.data)
ncol(DIF.data)
# Notes: 
# The last column contains a variable for gender called "Male." 
# This is a dummy variable where 1=Male and 0=Female
# The data is pre-sorted. The first half contains all observations for males,
# and the second half contains all observations for females.  

#------------------------------------------------------------------------------------------
# DIF Using the Mantel-Haenszel Statistic
#------------------------------------------------------------------------------------------

# Excercise 2. 
# ~~~~~~~~~~~~
# Check out unconditional crosstabs of item responses by gender (1=Male, 0=Female)
# Look at results in the list object "cross."
# Do you notice items with significant differences in proportion correct by gender? 

cross <- list(NULL)  # This creates a new list object.
for (i in 1:28) {  
  TAB <- table(DIF.data[,i], DIF.data$Male)
  cross[[i]] <- prop.table(TAB, 2)
} 
cross   

# Create a new variable for total score for each respondent
# Add the total score as a new column in the data.
# Does there seem to be a large amount of respondents for each possible total score value?

DIF.data$total <- apply(DIF.data[,1:28], 1, sum)
table(DIF.data$total)

# Collapse scores for every 20 percentiles of distribution (0 to 19, 20 to 39, ..., 80 to 99)
# This will create 5 bins in the collapsed grouping variable "diftot"
# Compare the frequencies for diftot to the freqs for totscore.

x <- quantile(DIF.data$total, probs = seq(0, 1, 0.10))
diftot <- matrix(0, 1500, 1)
for(i in 1:length(x)-1) {
  diftot[(DIF.data$total >= x[i] & DIF.data$total < x[i+1])]<-i
}
diftot[DIF.data$total==max(DIF.data$total)]<-length(x)-1
table(diftot) 

# Generate the Mantel-Haenszel statistic for each item. 
# Extract the odds ratios. 
# Which items would you flag for DIF? 

MH <- list(NULL)  
for (i in 1:28) {
  MH[[i]] <- mantelhaen.test(as.factor(DIF.data[,i]), 
                             y = as.factor(DIF.data$Male), z = diftot, 
                             alternative = "two.sided", conf.level = 0.95)
  }  
MH 

mh.or <- rep(0,28)
for (i in 1:28) {
  temp <- mantelhaen.test(DIF.data[,i], 
                          y = DIF.data$Male, z = diftot, 
                          alternative = "two.sided", conf.level = 0.95)
  mh.or[i] <- temp$estimate	
}
mh.or

hist(mh.or)
abline(v=1,col="blue")

# Transform the Mantel-Haenszel odds ratios into the ETS delta statistic
# Based on the ETS statistic, which items would you flag for DIF?

delta <- -2.35*log(mh.or)
hist(delta)
delta

# Level A: delta < 1 in absolute value [Feel free to use these.]
sum(abs(delta)<=1) 
# Level B: delta > 1 but < 1.5 in absolute value [Can use if you have to.]
sum(abs(delta)>1 & abs(delta)<=1.5)
# Level C: delta > 1.5 in absolute value [Don't use unless content experts say so.]
sum(abs(delta)>=1.5)

#------------------------------------------------------------------------------------------
# DIF Using the lordif Package
#------------------------------------------------------------------------------------------

# Excercise 3. 
# ~~~~~~~~~~~~

DIF.data <- read.csv("DIF.data1.csv") 

dif.ld <- lordif(DIF.data[,1:28], DIF.data[,29], 
                 criterion = "Chisqr", alpha = 0.01, minCell = 5)
print(dif.ld)

plot(dif.ld, labels = c("Female", "Male"), cex=0.7)

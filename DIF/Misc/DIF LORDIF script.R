###########Checking for Item Bias via Differential Item Functioning
###########Using an IRT Approach with R Package lordif

#install.packages("lordif")
library(lordif)

setwd("~/Dropbox/Courses/EDUC 8720/Data Sets/MASC-CDE")

## Once again we will use the CDE data, but this time we will also 
## include the column that indicates gender of test taker

fulldata<-read.fwf("CTBMathSci.rwo",widths=c(rep(1,76),3,1,6),skip=2)
 
# Note: "V78" in the CDE dataframe provides the gender for each student. 
# A value of 1 = "F", 2 = "M". 

cde<-fulldata[,c(1:31,78)]
names(cde)[32]<-"gender"

# Data screening step

cross<-list(NULL) #This initializes a new list object

for (i in 1:31){
TAB<-table(cde[,i],cde$gender)
cross[[i]]<-prop.table(TAB,2)
}

cross   

#Running the lordif algorithm

response<-cde[,1:31]
gender<-as.factor(cde[,32])
#Female = 1, Male = 2
gender.num<-as.numeric(gender)

DIF <- lordif(response, gender.num, criterion = "Chisqr", alpha = 0.01,minCell = 5)

print(DIF)

plot(DIF, labels = c("Female", "Male"),cex=.7)

#To assess statistical significance using simulation
#This can be very time-consuming--up to 30 minutes or so

DIF.mc <- montecarlo(DIF, alpha = 0.01, nr = 1000)

print(DIF.mc)
plot(DIF.mc)




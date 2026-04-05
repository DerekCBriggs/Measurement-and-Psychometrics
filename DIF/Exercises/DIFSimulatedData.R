library(irtoys)

#------------------------------------------------------------------------------------------
# DIF DATA SET 1
# ~~~~~~~~~~~~~~
# Impact: Males perform better than females 
#   - female mean theta = 0.8; male mean theta = 1.2
# DIF: 5 Items contain DIF with the following specifications:
#   - 3 items favor males and 2 favor females
#        - item 5 is 1.0 logits more difficult for females
#        - item 10 is 0.7 logits more difficult for females 
#        - item 15 is 0.5 logits more difficult for females
#        - item 20 is 0.5 logits more difficult for males
#        - item 25 is 0.7 logits more difficult for males
#   - 4 items have uniform DIF and 1 has nonuniform DIF
#        - item 15 is 0.2 logits more discriminating for males
#------------------------------------------------------------------------------------------

# Simulate dataset for females

dsc.f <- c(0.54667, 0.70870, 1.21655, 0.74152, 0.27034, 
           1.44346, 0.43086, 0.58966, 0.73382, 0.82069, 
           1.19976, 0.58514, 0.71463, 1.43985, 0.80005, 
           1.21490, 0.92553, 0.93156, 1.23090, 1.42748, 
           1.44729, 1.12610, 1.41680, 0.80265, 0.82695, 
           0.73163, 1.34518, 0.98092)
dif.f <- c(0.2770, -0.2879, 0.4330, 0.5440, 2.8675, 
           0.9882, -1.1542, 1.3608, -1.5074, 1.5482, 
           -0.3881, 0.9395, 0.5619, 1.3371, 1.5060, 
           1.2315, 0.2625, 0.5734, 1.2647, 0.5633, 
           0.7119, 0.7563, 1.9647, -0.1522, 0.4315, 
           -0.3123, 0.7147, 0.8285)
gss <- c(0.1630, 0.0330, 0.0817, 0.0642, 0.2000, 
         0.2130, 0.1823, 0.1026, 0.2000, 0.2159,
         0.4051, 0.1969, 0.2944, 0.1790, 0.2898, 
         0.3112, 0.0503, 0.0901, 0.2780, 0.0989, 
         0.3056, 0.1852, 0.3472, 0.1984, 0.2820, 
         0.1128, 0.1544, 0.3306)

set.seed(3000)
f.pars <- cbind(dsc.f, dif.f, gss)  
f.theta <- rnorm(1000, mean = 0.8, sd = 1)

f.d <- sim(f.pars, f.theta)
raw.f <- as.data.frame(f.d)
raw.f$Male <- 0

# Simulate dataset for males

dsc.m <- c(0.54667, 0.70870, 1.21655, 0.74152, 0.27034, 
           1.44346, 0.43086, 0.58966, 0.73382, 0.82069, 
           1.19976, 0.58514, 0.71463, 1.43985, 1.00005, 
           1.21490, 0.92553, 0.93156, 1.23090, 1.42748, 
           1.44729, 1.12610, 1.41680, 0.80265, 0.82695, 
           0.73163, 1.34518, 0.98092)
dif.m <- c(0.2770, -0.2879, 0.4330, 0.5440, 1.8675, 
           0.9882, -1.1542, 1.3608, -1.5074, 0.8482, 
           -0.3881, 0.9395, 0.5619, 1.3371, 1.0060, 
           1.2315, 0.2625, 0.5734, 1.2647, 1.0633, 
           0.7119, 0.7563, 1.9647, -0.1522, 1.1315, 
           -0.3123, 0.7147, 0.8285)

set.seed(3000)
m.pars <- cbind(dsc.m, dif.m, gss)  
m.theta <- rnorm(1000, mean = 1.2, sd = 1.1)

m.d <- sim(m.pars, m.theta)
raw.m <- as.data.frame(m.d)
raw.m$Male <- 1

DIF.data1 <- rbind(raw.f, raw.m)
# setwd("~/Desktop")
# write.csv(DIF.data1, file="DIF.data1.csv", row.names=FALSE)

rm(f.d, f.pars, m.d, m.pars, raw.f, raw.m, dif.f, dif.m, dsc.f, dsc.m, f.theta, gss, m.theta)

#------------------------------------------------------------------------------------------
# DIF DATA SET 2
# ~~~~~~~~~~~~~~
# Impact: Females perform better than males 
#   - female mean theta = 1.15; male mean theta = 1.00
# DIF: 8 Items contain DIF with the following specifications:
#   - 7 items favor females and 1 favors males
#        - item 5 is 1.2 logits more difficult for males
#        - item 10 is 1.1 logits more difficult for males 
#        - item 15 is 1.0 logits more difficult for males
#        - item 20 is 0.9 logits more difficult for males
#        - item 25 is 0.8 logits more difficult for males
#        - item 26 is 0.7 logits more difficult for males
#        - item 27 is 0.6 logits more difficult for males
#        - item 28 is 1.0 logits more difficult for females
#   - 7 items have uniform DIF and 1 has nonuniform DIF
#        - item 15 is 0.5 logits more discriminating for males
#------------------------------------------------------------------------------------------

# Simulate dataset for females

dsc.f <- c(0.54667, 0.70870, 1.21655, 0.74152, 0.27034, 
           1.44346, 0.43086, 0.58966, 0.73382, 0.82069, 
           1.19976, 0.58514, 0.71463, 1.43985, 0.40005, 
           1.21490, 0.92553, 0.93156, 1.23090, 1.42748, 
           1.44729, 1.12610, 1.41680, 0.80265, 0.82695, 
           0.73163, 1.34518, 0.98092)
dif.f <- c(0.2770, -0.2879, 0.4330, 0.5440, 1.0675, 
           0.9882, -1.1542, 1.3608, -1.5074, 0.1482, 
           -0.3881, 0.9395, 0.5619, 1.3371, 0.5060, 
           1.2315, 0.2625, 0.5734, 1.2647, 0.1633, 
           0.7119, 0.7563, 1.9647, -0.1522, 0.3315, 
           -1.0123, 0.1147, 1.8285)
gss <- c(0.1630, 0.0330, 0.0817, 0.0642, 0.2000, 
         0.2130, 0.1823, 0.1026, 0.2000, 0.2159,
         0.4051, 0.1969, 0.2944, 0.1790, 0.2898, 
         0.3112, 0.0503, 0.0901, 0.2780, 0.0989, 
         0.3056, 0.1852, 0.3472, 0.1984, 0.2820, 
         0.1128, 0.1544, 0.3306)

set.seed(3000)
f.pars <- cbind(dsc.f, dif.f, gss)  
f.theta <- rnorm(1000, mean = 1.15, sd = 1)

f.d <- sim(f.pars, f.theta)
raw.f <- as.data.frame(f.d)
raw.f$Male <- 0

# Simulate dataset for males

dsc.m <- c(0.54667, 0.70870, 1.21655, 0.74152, 0.27034, 
           1.44346, 0.43086, 0.58966, 0.73382, 0.82069, 
           1.19976, 0.58514, 0.71463, 1.43985, 1.30005, 
           1.21490, 0.92553, 0.93156, 1.23090, 1.42748, 
           1.44729, 1.12610, 1.41680, 0.80265, 0.82695, 
           0.73163, 1.34518, 0.98092)
dif.m <- c(0.2770, -0.2879, 0.4330, 0.5440, 2.2675, 
           0.9882, -1.1542, 1.3608, -1.5074, 1.2482, 
           -0.3881, 0.9395, 0.5619, 1.3371, 1.0060, 
           1.2315, 0.2625, 0.5734, 1.2647, 1.0633, 
           0.7119, 0.7563, 1.9647, -0.1522, 1.1315, 
           -0.3123, 0.7147, 0.8285)

set.seed(3000)
m.pars <- cbind(dsc.m, dif.m, gss)  
m.theta <- rnorm(1000, mean = 1, sd = 1.1)

m.d <- sim(m.pars, m.theta)
raw.m <- as.data.frame(m.d)
raw.m$Male <- 1

DIF.data2 <- rbind(raw.f, raw.m)
# setwd("~/Desktop")
# write.csv(DIF.data2, file="DIF.data2.csv", row.names=FALSE)

rm(f.d, f.pars, m.d, m.pars, raw.f, raw.m, dif.f, dif.m, dsc.f, dsc.m, f.theta, gss, m.theta)




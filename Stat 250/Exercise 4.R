# Exercise 4

# 1. Construct a dataset where you extract the ACRES92, ACRES87 and FARMS92 from 
# the agpop.DAT dataset
agpop <- read.csv('agpop.dat', head = T, sep = ',')
agpop <- with(agpop, cbind.data.frame(ACRES92, ACRES87, FARMS92))

# 2. Eliminate all observations where any of the above variables are missing.
agpop <- subset(agpop, ACRES92 >= 0 & ACRES87 >= 0 & FARMS92 >= 0)

# 3. Draw a random sample of size 300 and estimate the population mean of
# ACRES92 using a ratio estimator with
agpop.srs <- agpop[sample(nrow(agpop), size = 300),]

ACRES92.mean <- mean(agpop.srs$ACRES92)
ACRES87.mean <- mean(agpop.srs$ACRES87)
FARMS92.mean <- mean(agpop.srs$FARMS92)

# 3.a. ACRES87 as auxiliary variable
B.ACRES87 <- ACRES92.mean / ACRES87.mean
y_ratio.ACRES87 <- B.ACRES87 * mean(agpop$ACRES87)

# 3.b. FARMS92 as auxiliary variable
B.FARMS92 <- ACRES92.mean / FARMS92.mean
y_ratio.FARMS92 <- B.FARMS92 * mean(agpop$FARMS92)

cor.test(agpop.srs$ACRES92, agpop.srs$ACRES87)
cor.test(agpop.srs$ACRES92, agpop.srs$FARMS92)

par(mfrow=c(1,2))
plot(agpop.srs$ACRES92, agpop.srs$ACRES87)
abline(lm(agpop.srs$ACRES92 ~ agpop.srs$ACRES87))

plot(agpop.srs$ACRES92, agpop.srs$FARMS92)
abline(lm(agpop.srs$ACRES92 ~ agpop.srs$FARMS92))
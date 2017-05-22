# Exercise 5

# 1. Delete all observations with missing values for the variable ACRES92.
agpop <- read.csv('agpop.dat', head = T, sep = ',')
agpop <- subset(agpop, ACRES92 >= 0 & ACRES87 >= 0)

# 2. Construct separate datasets according to the 4 REGIONS.
agpop_ne <- subset(agpop, REGION == 'NE')
agpop_nc <- subset(agpop, REGION == 'NC')
agpop_s <- subset(agpop, REGION == 'S')
agpop_w <- subset(agpop, REGION == 'W')

# 3. Using set.seet (last 5 digits of student number), draw a random sample of size 300 and estimate the population mean and total.
set.seed(91450)
agpop.srs <- agpop[sample(nrow(agpop), size = 300),]
ACRES92.mean <- mean(agpop.srs$ACRES92); ACRES92.mean
nrow(agpop) * ACRES92.mean

# 4. Using
# a. set.seed(last 5 digits of your std no + 10), obtain a sample of size 21 from the Northeast stratum
set.seed(91450 + 10); agpop_ne.strat <- agpop_ne[sample(nrow(agpop_ne), size = 21),]

# b. set.seed(last 5 digits of your std no + 11), obtain a sample of size 103 from the North Central stratum
set.seed(91450 + 11); agpop_nc.strat <- agpop_nc[sample(nrow(agpop_nc), size = 103),]

# c. set.seed(last 5 digits of your std no + 12), obtain a sample of size 135 from the South stratum
set.seed(91450 + 12); agpop_s.strat <- agpop_s[sample(nrow(agpop_s), size = 135),]

# d. set.seed(last 5 digits of your std no + 13), obtain a sample of size 41 from the South stratum
set.seed(91450 + 13); agpop_w.strat <- agpop_w[sample(nrow(agpop_w), size = 41),]

# 5. Estimate the population mean and total of ACRES92 using a ratio estiator with ACRES87 as auxiliary variable.
ACRES87.mean <- mean(agpop.srs$ACRES87)
B.ACRES87 <- ACRES92.mean / ACRES87.mean
y_ratio.ACRES87 <- B.ACRES87 * mean(agpop$ACRES87); y_ratio.ACRES87
nrow(agpop) * y_ratio.ACRES87

# 6. Estimate the population total using a combined ratio estimator (see page 144)
ACRES92.mean <- rbind(
  ne = mean(agpop_ne.strat$ACRES92),
  nc = mean(agpop_nc.strat$ACRES92),
  s = mean(agpop_s.strat$ACRES92),
  w = mean(agpop_w.strat$ACRES92))

ACRES87.mean <- rbind(
  ne = mean(agpop_ne.strat$ACRES87),
  nc = mean(agpop_nc.strat$ACRES87),
  s = mean(agpop_s.strat$ACRES87),
  w = mean(agpop_w.strat$ACRES87))

ACRES92.t_hat <- rbind(
  ne = nrow(agpop_ne) * ACRES92.mean[1,],
  nc = nrow(agpop_nc) * ACRES92.mean[2,],
  s = nrow(agpop_s) * ACRES92.mean[3,],
  w = nrow(agpop_w) * ACRES92.mean[4,])

ACRES87.t_hat <- rbind(
  ne = nrow(agpop_ne) * ACRES87.mean[1,],
  nc = nrow(agpop_nc) * ACRES87.mean[2,],
  s = nrow(agpop_s) * ACRES87.mean[3,],
  w = nrow(agpop_w) * ACRES87.mean[4,])

B_hat.strat <- sum(ACRES92.t_hat) / sum(ACRES87.t_hat)
t_x <- nrow(agpop) * mean(agpop$ACRES87)
B_hat.strat * t_x

# 7. Estimate the population total using a separate ratio estimator (see page 144)
sum(sum(agpop_ne$ACRES87) * (ACRES92.mean[1,] / ACRES87.mean[1,]),
    sum(agpop_nc$ACRES87) * (ACRES92.mean[2,] / ACRES87.mean[2,]), 
    sum(agpop_s$ACRES87) * (ACRES92.mean[3,] / ACRES87.mean[3,]),
    sum(agpop_w$ACRES87) * (ACRES92.mean[4,] / ACRES87.mean[4,]))
# Exercise 6
install.packages('dplyr')
require(dplyr)

# 1. Delete all observations with missing values for the variable ACRES92.
agpop <- read.csv('agpop.dat', head = T, sep = ',')
agpop <- subset(agpop, ACRES92 >= 0)

# 2. Using set.seet (last 5 digits of student number), draw a random sample of size 300 
set.seed(91450)
agpop.srs <- agpop[sample(nrow(agpop), size = 300),]
agpop.srs %>% group_by(REGION) %>% summarise(n())

# 3. Obtain t_hat of
agpop_ne <- subset(agpop, REGION == 'NE')
agpop_nc <- subset(agpop, REGION == 'NC')
agpop_s <- subset(agpop, REGION == 'S')
agpop_w <- subset(agpop, REGION == 'W')

# a. Northeast
agpop_ne.srs <- subset(agpop.srs, REGION == 'NE')
agpop_ne.t_hat <- nrow(agpop_ne) * mean(agpop_ne.srs$ACRES92)
agpop_ne.t_hat

# b. North Central
agpop_nc.srs <- subset(agpop.srs, REGION == 'NC')
agpop_nc.t_hat <- nrow(agpop_nc) * mean(agpop_nc.srs$ACRES92)
agpop_nc.t_hat

# c. South
agpop_s.srs <- subset(agpop.srs, REGION == 'S')
agpop_s.t_hat <- nrow(agpop_s) * mean(agpop_s.srs$ACRES92)
agpop_s.t_hat

# d. West
agpop_w.srs <- subset(agpop.srs, REGION == 'W')
agpop_w.t_hat <- nrow(agpop_w) * mean(agpop_w.srs$ACRES92)
agpop_w.t_hat

# 4. Obtain t_hat of ACRES92 post-stratification, y bar of ACRES92 post-stratification.
sum(agpop_ne.t_hat, agpop_nc.t_hat, agpop_s.t_hat, agpop_w.t_hat)

sum((nrow(agpop_ne)/nrow(agpop)) * mean(agpop_ne.srs$ACRES92),
(nrow(agpop_nc)/nrow(agpop)) * mean(agpop_nc.srs$ACRES92),
(nrow(agpop_s)/nrow(agpop)) * mean(agpop_s.srs$ACRES92),
(nrow(agpop_w)/nrow(agpop)) * mean(agpop_w.srs$ACRES92))

# 5. Approximate V_hat[y_bar of ACRES92 post-stratification].
sum((nrow(agpop_ne)/nrow(agpop)) * (var(agpop_ne.srs$ACRES92)/nrow(agpop_ne.srs)),
(nrow(agpop_nc)/nrow(agpop)) * (var(agpop_nc.srs$ACRES92)/nrow(agpop_nc.srs)),
(nrow(agpop_s)/nrow(agpop)) * (var(agpop_s.srs$ACRES92)/nrow(agpop_s.srs)),
(nrow(agpop_w)/nrow(agpop)) * (var(agpop_w.srs$ACRES92)/nrow(agpop_w.srs)))
# Exercise 3

# 1. Delete all observations with mising values for the variable ACRES92.
setwd('D://Stat 250//')
agpop <- read.csv('agpop.dat', head = T, sep = ',')
agpop <- subset(agpop, ACRES92 >= 0)

# 2. Construct separate datasets according to the 4 REGIONS.
agpop_ne <- subset(agpop, REGION == 'NE')
agpop_nc <- subset(agpop, REGION == 'NC')
agpop_s <- subset(agpop, REGION == 'S')
agpop_w <- subset(agpop, REGION == 'W')

# 3. Obtain 10 different SRS from dataset 1 using
# a. set.seed(last 5 digits of your std no)

# for(i in 0:9) {
#   label <- paste0('agpop', '_', i) 
#   assign(label, 0:i)
#   set.seed(91450+i)
#   agpop[sample(nrow(agpop), size = 300),]
# }

set.seed(91450); agpop_0 <- agpop[sample(nrow(agpop), size = 300),]
set.seed(91451); agpop_1 <- agpop[sample(nrow(agpop), size = 300),]
set.seed(91452); agpop_2 <- agpop[sample(nrow(agpop), size = 300),]
set.seed(91453); agpop_3 <- agpop[sample(nrow(agpop), size = 300),]
set.seed(91454); agpop_4 <- agpop[sample(nrow(agpop), size = 300),]
set.seed(91455); agpop_5 <- agpop[sample(nrow(agpop), size = 300),]
set.seed(91456); agpop_6 <- agpop[sample(nrow(agpop), size = 300),]
set.seed(91457); agpop_7 <- agpop[sample(nrow(agpop), size = 300),]
set.seed(91458); agpop_8 <- agpop[sample(nrow(agpop), size = 300),]
set.seed(91459); agpop_9 <- agpop[sample(nrow(agpop), size = 300),]

# Compute for mean for each SRS
agpop.srs_mean <- rbind(agpop_0 = mean(agpop_0$ACRES92),
                        agpop_1 = mean(agpop_1$ACRES92),
                        agpop_2 = mean(agpop_2$ACRES92),
                        agpop_3 = mean(agpop_3$ACRES92),
                        agpop_4 = mean(agpop_4$ACRES92),
                        agpop_5 = mean(agpop_5$ACRES92),
                        agpop_6 = mean(agpop_6$ACRES92),
                        agpop_7 = mean(agpop_7$ACRES92),
                        agpop_8 = mean(agpop_8$ACRES92),
                        agpop_9 = mean(agpop_9$ACRES92))
colnames(agpop.srs_mean) <- 'mean'

# and compute (and reflect in your paper) the sample variances.
# Variance estimation of SRS: (1-n/N) * (s^2/n)
fpc <- 1-300/nrow(agpop)
agpop_0.var <- fpc * var(agpop_0$ACRES92)/nrow(agpop_0)
agpop_1.var <- fpc * var(agpop_1$ACRES92)/nrow(agpop_1)
agpop_2.var <- fpc * var(agpop_2$ACRES92)/nrow(agpop_2)
agpop_3.var <- fpc * var(agpop_3$ACRES92)/nrow(agpop_3)
agpop_4.var <- fpc * var(agpop_4$ACRES92)/nrow(agpop_4)
agpop_5.var <- fpc * var(agpop_5$ACRES92)/nrow(agpop_5)
agpop_6.var <- fpc * var(agpop_6$ACRES92)/nrow(agpop_6)
agpop_7.var <- fpc * var(agpop_7$ACRES92)/nrow(agpop_7)
agpop_8.var <- fpc * var(agpop_8$ACRES92)/nrow(agpop_8)
agpop_9.var <- fpc * var(agpop_9$ACRES92)/nrow(agpop_9)

agpop.var <- rbind(agpop_0.var, agpop_1.var, agpop_2.var, agpop_3.var, agpop_4.var,
                   agpop_5.var, agpop_6.var, agpop_7.var, agpop_8.var, agpop_9.var)
colnames(agpop.var) <- 'sample_variances'
agpop.var

# 4. Construct the population ANOVA table from the stratification obtained in 2.
agpop_ne.mean <- mean(agpop_ne$ACRES92)
agpop_nc.mean <- mean(agpop_nc$ACRES92)
agpop_s.mean <- mean(agpop_s$ACRES92)
agpop_w.mean <- mean(agpop_w$ACRES92)
agpop.mean <- mean(agpop$ACRES92)

# Between strata
pop_ssb <- sum(nrow(agpop_ne) * (agpop_ne.mean - agpop.mean)^2,
               nrow(agpop_nc) * (agpop_nc.mean - agpop.mean)^2,
               nrow(agpop_s) * (agpop_s.mean - agpop.mean)^2,
               nrow(agpop_w) * (agpop_w.mean - agpop.mean)^2)
pop_ssb_df <- 3 # H-1

# Within strata
pop_ssh <- sum((nrow(agpop_ne) - 1) * var(agpop_ne$ACRES92),        
               (nrow(agpop_nc) - 1) * var(agpop_nc$ACRES92),
               (nrow(agpop_s) - 1) * var(agpop_s$ACRES92),
               (nrow(agpop_w) - 1) * var(agpop_w$ACRES92))
pop_ssh_df <- nrow(agpop) - 4 # N-H

# Total
pop_ssto <- (nrow(agpop) - 1) * var(agpop$ACRES92)
pop_ssto_df <- nrow(agpop) - 1 # N-1

pop_anova <- matrix(NA, nrow = 3, ncol = 2)
pop_anova[,1] <- rbind(pop_ssb_df, pop_ssh_df, pop_ssto_df)
pop_anova[,2] <- rbind(pop_ssb, pop_ssh, pop_ssto)
rownames(pop_anova) <- c('SSB', 'SSH', 'Total')
colnames(pop_anova) <- c('df', 'SumOfSquares')
pop_anova

# 5. Using:
# a. set.seed(last 5 digits of your std no + 10), obtain a sample of size 21 from the Northeast stratum
set.seed(91450 + 10); agpop_ne.strat <- agpop_ne[sample(nrow(agpop_ne), size = 21),]
agpop_ne.strat_mean <- mean(agpop_ne.strat$ACRES92)

# b. set.seed(last 5 digits of your std no + 11), obtain a sample of size 103 from the North Central stratum
set.seed(91450 + 11); agpop_nc.strat <- agpop_nc[sample(nrow(agpop_nc), size = 103),]
agpop_nc.strat_mean <- mean(agpop_nc.strat$ACRES92)

# c. set.seed(last 5 digits of your std no + 12), obtain a sample of size 135 from the South stratum
set.seed(91450 + 12); agpop_s.strat <- agpop_s[sample(nrow(agpop_s), size = 135),]
agpop_s.strat_mean <- mean(agpop_s.strat$ACRES92)

# d. set.seed(last 5 digits of your std no + 13), obtain a sample of size 41 from the South stratum
set.seed(91450 + 13); agpop_w.strat <- agpop_w[sample(nrow(agpop_w), size = 41),]
agpop_w.strat_mean <- mean(agpop_w.strat$ACRES92)

# 6. Construct the sample ANOVA table:
# a. using y_bar_SRS for y_bar_U

# Between strata
sample.srs_ssb <- sum(nrow(agpop_0) * (agpop.srs_mean[1,] - agpop.mean)^2,
                      nrow(agpop_1) * (agpop.srs_mean[2,] - agpop.mean)^2,
                      nrow(agpop_2) * (agpop.srs_mean[3,] - agpop.mean)^2,
                      nrow(agpop_3) * (agpop.srs_mean[4,] - agpop.mean)^2,
                      nrow(agpop_4) * (agpop.srs_mean[5,] - agpop.mean)^2,
                      nrow(agpop_5) * (agpop.srs_mean[6,] - agpop.mean)^2,
                      nrow(agpop_6) * (agpop.srs_mean[7,] - agpop.mean)^2,
                      nrow(agpop_7) * (agpop.srs_mean[8,] - agpop.mean)^2,
                      nrow(agpop_8) * (agpop.srs_mean[9,] - agpop.mean)^2,
                      nrow(agpop_9) * (agpop.srs_mean[10,] - agpop.mean)^2)
sample.srs_ssb_df <- 9 # H-1

# Within strata
sample.srs_ssh <- sum((nrow(agpop_0) - 1) * var(agpop_0$ACRES92),
                      (nrow(agpop_1) - 1) * var(agpop_1$ACRES92),
                      (nrow(agpop_2) - 1) * var(agpop_2$ACRES92),
                      (nrow(agpop_3) - 1) * var(agpop_3$ACRES92),
                      (nrow(agpop_4) - 1) * var(agpop_4$ACRES92),
                      (nrow(agpop_5) - 1) * var(agpop_5$ACRES92),
                      (nrow(agpop_6) - 1) * var(agpop_6$ACRES92),
                      (nrow(agpop_7) - 1) * var(agpop_7$ACRES92),
                      (nrow(agpop_8) - 1) * var(agpop_8$ACRES92),
                      (nrow(agpop_9) - 1) * var(agpop_9$ACRES92))
sample.srs_ssh_df <- nrow(agpop) - 10 # N-H

# Total
sample.srs_ssto <- (nrow(agpop) - 1) * var(agpop$ACRES92)
sample.srs_ssto_df <- nrow(agpop) - 1 # N-1

sample.srs_anova <- matrix(NA, nrow = 3, ncol = 2)
sample.srs_anova[,1] <- rbind(sample.srs_ssb_df, sample.srs_ssh_df, sample.srs_ssto_df)
sample.srs_anova[,2] <- rbind(sample.srs_ssb, sample.srs_ssh, sample.srs_ssto)
rownames(sample.srs_anova) <- c('SSB', 'SSH', 'Total')
colnames(sample.srs_anova) <- c('df', 'SumOfSquares')
sample.srs_anova

# b. using y_bar_strat for y_bar_U

# Between strata
sample.strat_ssb <- sum(nrow(agpop_ne.strat) * (agpop_ne.strat_mean - (nrow(agpop_ne.strat)/300) * agpop_ne.mean)^2,
                        nrow(agpop_nc.strat) * (agpop_nc.strat_mean - (nrow(agpop_ne.strat)/300) * agpop_nc.mean)^2,
                        nrow(agpop_s.strat) * (agpop_s.strat_mean - (nrow(agpop_ne.strat)/300) * agpop_s.mean)^2,
                        nrow(agpop_w.strat) * (agpop_w.strat_mean - (nrow(agpop_ne.strat)/300) * agpop_w.mean)^2)
sample.strat_ssb_df <- 3 # H-1

# Within strata
sample.strat_ssh <- sum((nrow(agpop_ne) - 1) * var(agpop_ne.strat$ACRES92),
                      (nrow(agpop_nc) - 1) * var(agpop_nc.strat$ACRES92),
                      (nrow(agpop_s) - 1) * var(agpop_s.strat$ACRES92),
                      (nrow(agpop_w) - 1) * var(agpop_w.strat$ACRES92))
sample.strat_ssh_df <- nrow(agpop) - 4 # N-H

# Total
sample.strat_ssto <- (nrow(agpop) - 1) * var(agpop$ACRES92)
sample.strat_ssto_df <- nrow(agpop) - 1 # N-1

sample.strat_anova <- matrix(NA, nrow = 3, ncol = 2)
sample.strat_anova[,1] <- rbind(sample.strat_ssb_df, sample.strat_ssh_df, sample.strat_ssto_df)
sample.strat_anova[,2] <- rbind(sample.strat_ssb, sample.strat_ssh, sample.strat_ssto)
rownames(sample.strat_anova) <- c('SSB', 'SSH', 'Total')
colnames(sample.strat_anova) <- c('df', 'SumOfSquares')
sample.strat_anova

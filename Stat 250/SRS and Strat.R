# Adrian Cuyugan - 91450

## 1. Load dataset agpop.dat into R.
setwd('D://Stat 250//')
agpop <- read.csv('agpop.dat', head = T, sep = ',')

## 2. Create a dartaframe in R from the dataset in (1) without any missing value for ACRES92.

agpop <- subset(agpop, ACRES92 != -99)   			# Invalid cases or < 0
agpop <- agpop[!is.na(agpop $ACRES92),]	      # Remove NAs 

## 3. Create four (4) additional dataframes for each of the regions.
agpop_ne <- subset(agpop, REGION == 'NE')
agpop_nc <- subset(agpop, REGION == 'NC')
agpop_s <- subset(agpop, REGION == 'S')
agpop_w <- subset(agpop, REGION == 'W')

## 4. For replication
set.seed(91450)

## 5. Obtain an SRS of size 300 freom the dataframe in (2).
agpop.srs <- agpop[sample(nrow(agpop), size = 300),]

## 6. Obtain an estimate of the population mean using eqn (2.8).
# For estimating the population mean y_bar from an SRS, use the sample mean.
srs.mean <- mean(agpop.srs$ACRES92); print(srs.mean)

# Variance estimation of SRS: (1-n/N) * (s^2/n)
fpc <- (1-nrow(agpop.srs)/nrow(agpop))
srs.var <- fpc * (sd(agpop.srs$ACRES92)/nrow(agpop.srs)); print(srs.var)

## 7. Obtain a 95% CI for the population mean using equations (2.12) and (2.22)
# y_bar - t_(alpha/2,n-1)*(sqrt(fpc)*(var/sqrt(n)); y_bar + t_(alpha/2,n-1)*(sqrt(fpc)*(var/sqrt(n))

se.srs <- sqrt(srs.var)	                            # Compute standard error
t.95_n299 <- qt(.025, df = 299, lower.tail = F)			# Compute t 95%, n-1	

srs.ci_low <- srs.mean - t.95_n299 * sqrt(fpc) * sqrt(var.srs)/sqrt(nrow(agpop.srs))
srs.ci_up <- srs.mean + t.95_n299 * sqrt(fpc) * sqrt(var.srs)/sqrt(nrow(agpop.srs))

list(y_bar.estimates = c(mean = srs.mean, variance = srs.var), 
     ci_95 = c(low = srs.ci_low, up = srs.ci_up))

## 8. Obtain a stratified sample of size 300 from the dataframes from (3) allocated
## according to the sizes mentioned in page 75 (Lohr).
# NE = 21; NC = 103; S = 135; W = 41, 
agstrat_ne <- agpop_ne[sample(nrow(agpop_ne), size = 21),]
agstrat_nc <- agpop_nc[sample(nrow(agpop_nc), size = 103),]
agstrat_s <- agpop_s[sample(nrow(agpop_s), size = 135),]
agstrat_w <- agpop_w[sample(nrow(agpop_w), size = 41),]

## 9. Obtain an estimate of the population mean using equation (3.2)

# Number of counties in stratum:
northeast <- 220
northcentral <- 1054
south <- 1382
west <- 422

# Compute for strata means
strat_ne.mean <- mean(agstrat_ne$ACRES92); print(strat_ne.mean)   # Northeast      mean of ACRES92
strat_nc.mean <- mean(agstrat_nc$ACRES92); print(strat_nc.mean)   # North Central  mean of ACRES92
strat_s.mean <- mean(agstrat_s$ACRES92); print(strat_s.mean)      # South          mean of ACRES92
strat_w.mean <- mean(agstrat_w$ACRES92); print(strat_w.mean)      # West           mean of ACRES92

# Compute sample stratum averages
strat.mean <- (21/300) * strat_ne.mean + 
  (103/300) * strat_nc.mean + 
  (135/300) * strat_s.mean + 
  (42/300) * strat_w.mean
print(strat.mean)

# Estimate mean population quantities of each stratum
pop.mean_ne <- northeast * strat_ne.mean      # Northeast
pop.mean_nc <- northcentral * strat_nc.mean   # North Central
pop.mean_s <- south * strat_s.mean            # South
pop.mean_w <- west * strat_w.mean             # West

(pop.mean_ne + pop.mean_nc + pop.mean_s + pop.mean_w) / (northeast + northcentral + south + west)

# Compute for strata variances
strat_ne.var <- (1-nrow(agstrat_ne)/300) * (sd(agstrat_ne$ACRES92)/nrow(agstrat_ne)); print(strat_ne.var)
strat_nc.var <- (1-nrow(agstrat_nc)/300) * (sd(agstrat_nc$ACRES92)/nrow(agstrat_nc)); print(strat_nc.var)
strat_s.var <- (1-nrow(agstrat_s)/300) * (sd(agstrat_s$ACRES92)/nrow(agstrat_s)); print(strat_s.var)
strat_w.var <- (1-nrow(agstrat_w)/300) * (sd(agstrat_w$ACRES92)/nrow(agstrat_w)); print(strat_w.var)

## 10. Obtain a 95% CI for the population mean using the formulas on page 79 (Lohr)
## using the percentile of the t-distribution with n-H df (n = 300, H = 4).

# Compute for variance of y_bar strata
strat_ne.ybar_var <- (1-nrow(agstrat_ne)/300) * (1-nrow(agstrat_ne)/300)^2 * (sd(agstrat_ne$ACRES92)/nrow(agstrat_ne))
strat_nc.ybar_var <- (1-nrow(agstrat_nc)/300) * (1-nrow(agstrat_nc)/300)^2 * (sd(agstrat_nc$ACRES92)/nrow(agstrat_nc))
strat_s.ybar_var <- (1-nrow(agstrat_s)/300) * (1-nrow(agstrat_s)/300)^2 * (sd(agstrat_s$ACRES92)/nrow(agstrat_s))
strat_w.ybar_var <- (1-nrow(agstrat_w)/300) * (1-nrow(agstrat_w)/300)^2 * (sd(agstrat_w$ACRES92)/nrow(agstrat_w))

# Compute for variance of y_bar
strat.ybar_var <- sqrt(strat_ne.ybar_var + strat_nc.ybar_var + strat_s.ybar_var + strat_w.ybar_var)

# Compute for 95% CI 
strat.ci_low <- strat.mean - 1.96 * sqrt(fpc) * strat.ybar_var
strat.ci_up <- strat.mean + 1.96 * sqrt(fpc) * strat.ybar_var

list(y_bar.estimates = c(mean = strat.mean, 
                         variance = strat.ybar_var),
     strat.var = c(strat_ne.var = strat_ne.var,
                   strat_nc.var = strat_nc.var,
                   strat_s.var = strat_s.var,
                   strat_w.var = strat_w.var),
     ci_95 = c(low = strat.ci_low, 
               up = strat.ci_up))

#### Stat 250 ####

# Checks if the required packages are installed
LoadLibrary_v2 <- function(RequiredPackages) {
  remainingPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])];
  
  if(length(remainingPackages)) {
    install.packages(remainingPackages);
  }
  
  for(packageName in requiredPackages) {
    library(packageName, character.only = TRUE, quietly = TRUE);
  }
}

# List of required packages to be installed and loaded 
requiredPackages <- c('data.table', # reading text files into a table dataframe
                      'ggplot2',    # plotting
                      'RCurl');     # connects to git

# Calls the function to load listed libraries
LoadLibrary_v2(requiredPackages)

# Load datasets
agpop <- getURL("https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv")
agpop <- read.csv(text = agpop)
agsrs <- getURL("https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv")
agsrs <- read.csv(text = agsrs)
agstrat <- getURL("https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv")
agstrat <- read.csv(text = agstrat)

# Load these when internet is not available
agpop <- read.csv('agpop.dat', head = T, sep = ',')
agsrs <- read.csv('agsrs.dat', head = T, sep = ',')
agstrat <- read.csv('agstrat.dat', head = T, sep = ',')

# Remove negative values on ACRES92
agpop <- subset(agpop, ACRES92 >= 0)

# Set seed for replication
set.seed(91450)


#### Simple random sample w/o replacement ####
# Perform 300 SRSWOR from population
agpop.srswor <- agpop[sample(1:nrow(agpop), size = 300, replace = F),]

# Plot histogram of sample
ggplot(data = agpop.srswor, aes(x = ACRES92/1e6)) + 
  geom_histogram() + 
  xlab('Millions of Acres Devoted to Farms') +
  ylab('Frequency')

# Plot histogram of sample (using base package)
hist(agpop.srswor$ACRES92/1e6, 
     xlab = 'Millions of Acres Devoted to Farms',
     ylab = 'Frequency',
     main = 'ACRES92')

# For estimating the population mean y_bar from an SRS, we use the sample mean
srswor.mean <- mean(agpop.srswor$ACRES92)
print(srswor.mean)

# An unbiased estimator of the variance of y_bar is
# sample variance of SRSWOR: (1-n/N) * (s^2/n)
srswor.var <- (1-nrow(agpop.srswor)/nrow(agpop)) * (sd(agpop.srswor$ACRES92)/nrow(agpop.srswor))
print(srswor.var)

# The standard error (SE) is the square root of the estimated variance of y_bar
srswor.se <- sqrt(srswor.var)
print(srswor.se)


#### Stratified sampling ####
# For this example, we use the four census regions of the United States:
# Northeast, North Central, South, and West—as strata

# Number of countries in stratum:
northeast <- 220
northcentral <- 1054
south <- 1382
west <- 422

# Number of countries in sample:
# NE: 21, NC: 103, S: 135, W: 41
agpop.strat_ne <- subset(agpop, REGION == 'NE')
agpop.strat_ne <- agpop.strat_ne[sample(1:nrow(agpop.strat_ne), size = 21, replace = F),]
agpop.strat_nc <- subset(agpop, REGION == 'NC')
agpop.strat_nc <- agpop.strat_nc[sample(1:nrow(agpop.strat_nc), size = 103, replace = F),]
agpop.strat_s <- subset(agpop, REGION == 'S')
agpop.strat_s <- agpop.strat_s[sample(1:nrow(agpop.strat_s), size = 135, replace = F),]
agpop.strat_w <- subset(agpop, REGION == 'W')
agpop.strat_w <- agpop.strat_w[sample(1:nrow(agpop.strat_w), size = 41, replace = F),]

# Union of dataframes of each stratum
agpop.strat <- rbind.data.frame(agpop.strat_ne, agpop.strat_nc, agpop.strat_s, agpop.strat_w)

# Check if the total sample of 4 strata = 21 + 103 + 135 + 41 = 300
nrow(agpop.strat_ne) + nrow(agpop.strat_nc) + nrow(agpop.strat_s) + nrow(agpop.strat_w)

# Compute for strata means
strat_ne.mean <- mean(agpop.strat_ne$ACRES92)
strat_nc.mean <- mean(agpop.strat_nc$ACRES92)
strat_s.mean <- mean(agpop.strat_s$ACRES92)
strat_w.mean <- mean(agpop.strat_w$ACRES92)

print(strat_ne.mean)   # Northeast      mean of ACRES92
print(strat_nc.mean)   # North Central  mean of ACRES92
print(strat_s.mean)    # South          mean of ACRES92
print(strat_w.mean)    # West           mean of ACRES92

# Estimate mean population quantities of each stratum
northeast * strat_ne.mean       # Northeast
northcentral * strat_nc.mean    # North Central
south * strat_s.mean            # South
west * strat_w.mean             # West

# Compute for strata variances
strat_ne.var <- (1-nrow(agpop.strat_ne)/nrow(agpop)) * (sd(agpop.strat_ne$ACRES92)/nrow(agpop.strat_ne))
strat_nc.var <- (1-nrow(agpop.strat_nc)/nrow(agpop)) * (sd(agpop.strat_nc$ACRES92)/nrow(agpop.strat_nc))
strat_s.var <- (1-nrow(agpop.strat_s)/nrow(agpop)) * (sd(agpop.strat_s$ACRES92)/nrow(agpop.strat_s))
strat_w.var <- (1-nrow(agpop.strat_w)/nrow(agpop)) * (sd(agpop.strat_w$ACRES92)/nrow(agpop.strat_w))

print(strat_ne.var)   # Northeast       variance of ACRES92
print(strat_nc.var)   # North Central   variance of ACRES92
print(strat_s.var)    # South           variance of ACRES92
print(strat_w.var)    # West            variance of ACRES92

# Boxplot of each stratum
ggplot(data = agpop.strat, aes(x = REGION, y = ACRES92/1e6)) + 
  geom_boxplot(aes(col = REGION)) + 
  theme_minimal() + 
  theme(legend.position="none") +
  ylab('Millions of Acres Devoted to Farms') +
  xlab('Region')

# Boxplot of each stratum (using base package)
with(agpop.strat, boxplot(ACRES92 ~ REGION, 
                          ylab = 'Millions of Acres Devoted to Farms',
                          xlab = 'Region',
                          main = 'ACRES92'))

# Estimate variance population quantities of each stratum - INCOMPLETE
# N * (1-n/N) * var/n
northeast^2 * (1-nrow(agpop.strat_ne)/northeast) *        # Northeast
northcentral * strat_nc.var    # North Central
south * strat_s.var            # South
west * strat_w.var             # West


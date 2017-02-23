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
agpop.strat_ne <- 

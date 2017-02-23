# Replace the URL of the source file, uses CSV instead of XLSX
toluca <- read.csv('C:/Users/adminuser/Dropbox/MasterOfStatistics/Stat 223/Toluca.csv', head = T, sep = ',')
toluca.lm <- lm(Work_Hrs ~ Lot_Size, data = toluca, model = T)
summary(toluca.lm); aov(toluca.lm)
plot(toluca.lm) # Diagnostic plots

require(ggplot2)
ggplot(toluca, aes(x = Lot_Size, y = Work_Hrs)) + 
  geom_point() + theme_minimal() +
  stat_smooth(method = 'lm', level = 0.9)

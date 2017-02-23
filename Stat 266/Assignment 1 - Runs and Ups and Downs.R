require(randtests)
require(ggplot2)
require(gridExtra)

assign1 <- read.csv('./Stat 266/Stat 266 - Assignment 1 (Runs and Ups and Downs).csv', head = T, sep = ',')
assign1$Date <- as.Date(assign1$Date, format = '%Y-%m-%d')
assign1$Subset <- ifelse(assign1$Date < '2015-03-13', 'Exact Test', 'Normal Approx')
mttr.range <- range(assign1$MTTR)

# Normal approximation
normaltest <- bartels.rank.test(assign1$MTTR, 
                                alternative = 'left.sided', 
                                pvalue = 'normal')

# Use exact calculation or beta distribution
exacttest <- bartels.rank.test(assign1[1:10,3], alternative = 'left.sided', pvalue = 'auto')

plot1 <- ggplot(assign1, aes(x = Date, y = MTTR)) + 
  geom_line() + geom_point(size = 3) + 
  theme_minimal() + ylim(mttr.range) +
  ggtitle('Run Chart, n = 22')
plot2 <- ggplot(assign1[1:10,], aes(x = Date, y = MTTR)) +
  geom_line() + geom_point(size = 3) + 
  theme_minimal() + ylim(mttr.range) +
  ggtitle('Run Chart, n = 10')
grid.arrange(plot2, plot1, ncol = 2)


####
n10 <- read.table('clipboard', sep = '\t', head = T)
rank(n10$MTTR)
diff(rank(n10$MTTR))

n22 <- read.table('clipboard', head = T, sep = '\t')
rank(n22$MTTR)
diff(rank(n22$MTTR))

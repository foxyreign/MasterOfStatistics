require(randtests)

# KS Two-Sample Test
rural <- c(1.1, -21.7, -16.3, -11.3, -10.4, -7.0, -2.0, -1.9, 6.2)
nonrural <- c(-2.4, 9.9, 14.2, 18.4, 20.1, 23.1, 70.4)

plot(ecdf(rural), pch = 19, verticals = T,col='blue', main = NULL, xlab = '')
lines(ecdf(nonrural), verticals = T, pch = 46, lty = 2)
legend(x = -25, y = 0.9, legend = c("Rural", "Non-Rural"), 
       lty = c(1,2), col = c('blue', 'black'), cex = 0.6)

ks.test(x = rural, y = nonrural, alternative = 'two.sided', exact = T)

# Wald-Wolfowitz Runs Test
combined <- c(rural, nonrural)
runs.test(combined, alternative = 'two.sided', pvalue = 'exact', plot = T)

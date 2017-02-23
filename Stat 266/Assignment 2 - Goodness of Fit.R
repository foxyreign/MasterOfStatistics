#### Problem 4.34 ####
ks.observed <- c(0.40, 0.50, 0.52, 0.58, 0.60, 0.65, 0.80, 0.80, 0.82, 0.95)
ks.hypothesized <- (ks.observed^2)*(3-2*ks.observed)
ks.df <- cbind.data.frame(ks.observed, ks.hypothesized)
ks.df # Views the dataset

# Plots the empirical cumulative distribution 
plot(ecdf(ks.observed), pch=19, verticals = T,col='blue', main=NULL, xlab='')
lines(ecdf(ks.hypothesized), verticals=T, pch=46, lty=2)
legend(x = 0.37, y = 0.9, legend=c("Observed", "Hypothesized"), 
       lty = c(1,2), col =c('blue', 'black'), cex=0.6)

# Runs KS test using two-sample but the other population is based on the hypothesized distribution
ks.test(x = ks.observed, y = ks.hypothesized)

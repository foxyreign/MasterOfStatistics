# One-Sample Wilcox Signed Rank Test 
# p-value may be different because this does not consider midranks for ties
affected.agegroup <- c(0.600, 0.068, 0.025, 0.078, -0.022, 0.072, 0.041, 0.860, 0.012, 0.035)
agegroup.25to29 <- c(-0.025, -0.023, 0.004, -0.008, 0.061, 0.015, -0.035, -0.016, -0.061, 0.051)

wilcox.test(x = affected.agegroup, y = agegroup.25to29,
            mu = 0, # hypothesis is that the median is equals 0
            alternative = 'two.sided', # two-tailed test
            paired = T, # one-sample paired test
            exact = T, # compute for exact probability
            correct = F, # not using continuity correction and normal approximation
            conf.int = T, conf.level = 0.95) # computes for confidence interval at 95%

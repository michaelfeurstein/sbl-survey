setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(pwr)

# get medium effect size for anova
cohen.ES(test = "anov", size = "medium")

# 3 groups
# medium effect size (0.25)
# significance level 0.05
# 80% power
power_anova <- pwr.anova.test(k = 3, f = 0.25, sig.level = 0.05, power = 0.80)

# after study / power analysis for Friedman test (post-hoc calculation of power)
# source: 
# - https://www.statisticssolutions.com/wp-content/uploads/wp-post-to-pdf-enhanced-cache/1/friedman-anova-7-dependent-variables.pdf
# - Lehmann: Lehmann, E.L. (2006) Nonparametrics: Statistical methods based on ranks. New York, NY: Springer.
samplesize_friedman <- power_anova$n * 1.15
samplesize_friedman

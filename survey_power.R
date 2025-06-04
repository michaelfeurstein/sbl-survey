setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(pwr)

# get medium effect size for anova
cohen.ES(test = "anov", size = "small")

# 3 groups
# medium effect size (0.25)
# significance level 0.05
# 80% power
pwr.anova.test(k = 3, f = 0.25, sig.level = 0.05, power = 0.80)

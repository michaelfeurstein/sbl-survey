# this file is used as a pre sandbox for the cleaner R notebook

library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read from prepared csv
df <- read.csv("data-survey-sus_prepared.csv")
df <- subset(df, select = -c(X))

# Usability SUS Score
# boxplot - visual inspection

par(mar=c(5, 5, 3, 2))
boxplot(sus ~ notation, data = df, main = "Usability (System Usability Scale)", xlab = "Notation", ylab = "SUS score", names = c("CNL","KV","SC"))

# reporting mean and sd:

df %>%
  group_by(notation) %>%
  summarize(mean = mean(sus), sd = sd(sus), min = min(sus), max = max(sus), med = median(sus), q1 = quantile(sus, 0.25), q3 = quantile(sus, 0.75))

##### mean difference ####
# values
sus_cnl <- mean(df$sus[df$notation == "cnl"])
sus_kv <- mean(df$sus[df$notation == "kv"])
sus_sc <- mean(df$sus[df$notation == "sc"])
# difference between cnl kv: cnl-kv
sus_diffcnlkv <- sus_cnl-sus_kv
# avergae of nl kv
sus_avgcnlkv <- (sus_cnl+sus_kv)/2
sus_ratiocnlkv <- sus_diffcnlkv/sus_avgcnlkv
# percentage
sus_percent_diff_cnlkv <- abs(sus_ratiocnlkv*100)
print(c("CNL/KV mean difference in percentage: ", sus_percent_diff_cnlkv))

# difference between sc kv: sc-kv
sus_diffsckv <- sus_sc-sus_kv
# avergae of nl kv
sus_avgsckv <- (sus_sc+sus_kv)/2
sus_ratiosckv <- sus_diffsckv/sus_avgsckv
# percentage
sus_percent_diff_sckv <- abs(sus_ratiosckv*100)
print(c("SC/KV mean difference in percentage: ", sus_percent_diff_sckv))



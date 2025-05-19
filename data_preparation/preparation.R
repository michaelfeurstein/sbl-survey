setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyr)
library(dplyr)

###
### data import: initial ####
###

mydata <- read.csv("results-survey623939-anon.csv", sep = ",", header = TRUE)

###
### data-prepared: prepare data from limesurvey csv dump ####
###

# Preparing data-survey-sus_prepared.csv
# header: subject, id, seed, notation, sus

# create running index (subject)
mydata$subject <- seq.int(nrow(mydata)) 

# move subject column to first position
mydata <- mydata[,c(ncol(mydata),1:(ncol(mydata)-1))]

# remove unnecessary columns for SUS
df <- subset(mydata, select=-c(submitdate,lastpage,startlanguage,startdate,datestamp,PARTICIPANTTYPE.SQ001.,PARTICIPANTTYPE.SQ002.,PARTICIPANTTYPE.SQ003.,PARTICIPANTTYPE.other.,authorHOWMANYYEARS,authorHOWMANYVIDEOS,researchHOWMANYYEARS,researchHOWMANYPUB,userHOWMANYYEARS,userHOWMANYCOURSES,INFO01,INFO02,INFO03,READCHECK,NOTATION01HELP,COMMENTS01,NOTATION02HELP,COMMENTS02,NOtATION03HELP,COMMENTS03,NOTATIONOVERVIEW,RANKING.SQ001.,RANKING.SQ002.,RANKING.SQ003.,CLAIMEFFICIENCY.SQ001.,CLAIMEFFECTIVENESS.SQ001.,TEXTasFIRSTSTEP.SQ001.,TEXTwithGUI.SQ001.,GENERALCOMMENTS))

# replace text with numbers
# 1 - strongly disagree --> 5 - strongly agree
responseMapping <- c("1 - strongly disagree" = 1, "2" = 2, "3" = 3, "4" = 4, "5 - strongly agree" = 5)
for (i in 4:ncol(df)) {
  df[, i] <- responseMapping[df[, i]]
}

# pseudo:
# go into each row 
#   inside row go through specific columns and calculate score

# get likert responses for: sc, cnl, kv
# script-based notation (sc)
df_sc_sus <- subset(df, select=c(NOTATION01.SQ001.:NOTATION01.SQ010.))
# controlled natural-language notation (cnl)
df_cnl_sus <- subset(df, select=c(NOTATION02.SQ001.:NOTATION02.SQ010.))
# key-value notation (kv)
df_kv_sus <- subset(df, select=c(NOTATION03.SQ001.:NOTATION03.SQ010.))

# create function called calculateSUS 
# source: https://stackoverflow.com/a/43499215/693052
calculateSUS <- function(df) {
  df$sus <- "0"
  for(i in seq_along(2:nrow(df))) {
    score <- 0
    row <- df[i,]
    for (j in 1:10) {
      if((j %% 2) == 0) {
        # even
        score <- score + (5 - (as.numeric(row[,j])))
      } else {
        # odd
        score <- score + ((as.numeric(row[,j])) - 1)
      }
    }
    susscore <- score * 2.5
    df[i,]$sus <- susscore
  }
  df
}

# Calculate SUS scores for: sc, cnl, kv
df_sc_sus = calculateSUS(df_sc_sus)
df_cnl_sus = calculateSUS(df_cnl_sus)
df_kv_sus = calculateSUS(df_kv_sus)

# remove last row (NA row)
df_sc_sus <- head(df_sc_sus,-1)
df_cnl_sus <- head(df_cnl_sus,-1)
df_kv_sus <- head(df_kv_sus,-1)

# prepare final df
# later start
df <- head(df,-1)

# get from df: subject, id, seed
df_prep_sc <- data.frame(cbind(df[,c(1:3)]))
df_prep_cnl <- data.frame(cbind(df[,c(1:3)]))
df_prep_kv <- data.frame(cbind(df[,c(1:3)]))

# SC
# add notation column
df_prep_sc$notation <- "sc"
# merge into subject, id, seed, sus dataframe
df_prep_sc <- data.frame(cbind(df_prep_sc,df_sc_sus$sus))
# rename column sus
# source: https://stackoverflow.com/a/16490387/693052
names(df_prep_sc)[names(df_prep_sc) == 'df_sc_sus.sus'] <- 'sus'

# CNL
# add notation column
df_prep_cnl$notation <- "cnl"
# merge into subject, id, seed, sus dataframe
df_prep_cnl <- data.frame(cbind(df_prep_cnl,df_cnl_sus$sus))
# rename column sus
# source: https://stackoverflow.com/a/16490387/693052
names(df_prep_cnl)[names(df_prep_cnl) == 'df_cnl_sus.sus'] <- 'sus'

#KV
# add notation column
df_prep_kv$notation <- "kv"
# merge into subject, id, seed, sus dataframe
df_prep_kv <- data.frame(cbind(df_prep_kv,df_kv_sus$sus))
# rename column sus
# source: https://stackoverflow.com/a/16490387/693052
names(df_prep_kv)[names(df_prep_kv) == 'df_kv_sus.sus'] <- 'sus'

# merge df_prep_: sc,cnl,kv on top of each other
df_prep <- rbind(df_prep_sc, df_prep_cnl, df_prep_kv)

# transform sus into double
df_prep <- transform(df_prep, sus = as.double(df_prep$sus))

## Export 
# boxcox transform sus
df_prep$sus.boxcox = (df_prep$sus^3.2-1)/3.2

# write to csv so we don't need to run above lines too often
write.csv(df_prep, "../data-survey-sus_prepared.csv")

###
### data-sus-tidy: prepare tidy sus data csv's for graphs ####
###
### note: this is currently quite repetitive
### can be improved with functions (currently no time)
###
### sc: script based
### cnl: controlled-natural language
### kv: key-value based
###

# get only the likert responses for: sc,cnl,kv
df_sc <- df_sc_sus[,1:10]
df_cnl <- df_cnl_sus[,1:10]
df_kv <- df_kv_sus[,1:10]

responseMapping <- c("1" = "A1", "2" = "A2", "3" = "A3", "4" = "A4", "5" = "A5")

# prepare in same style as R export from limesurvey
for (i in 1:ncol(df_sc)) {
  df_sc[, i] <- responseMapping[df_sc[, i]]
  df_sc[, i] <- as.character(df_sc[, i])
  df_sc[, i] <- factor(df_sc[, i], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
}

for (i in 1:ncol(df_cnl)) {
  df_cnl[, i] <- responseMapping[df_cnl[, i]]
  df_cnl[, i] <- as.character(df_cnl[, i])
  df_cnl[, i] <- factor(df_cnl[, i], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
}

for (i in 1:ncol(df_kv)) {
  df_kv[, i] <- responseMapping[df_kv[, i]]
  df_kv[, i] <- as.character(df_kv[, i])
  df_kv[, i] <- factor(df_kv[, i], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
}

# check - should be character coded in A1:A5
#sapply(df_sc, mode)

# manually code columns and data based on limesurvey
# SC
# 1
attributes(df_sc)$variable.labels[1] <- "[I think that I would like to use this notation frequently.] Please answer the following questions about the notation you just used:"
names(df_sc)[1] <- "PQ1. I think that I would like to use this notation frequently."
# 2
attributes(df_sc)$variable.labels[2] <- "[I find the notation unnecessarily complex.] Please answer the following questions about the notation you just used:"
names(df_sc)[2] <- "NQ1. I find the notation unnecessarily complex."
# 3
attributes(df_sc)$variable.labels[3] <- "[I think the notation would be easy to use.] Please answer the following questions about the notation you just used:"
names(df_sc)[3] <- "PQ2. I think the notation would be easy to use."
# 4
attributes(df_sc)$variable.labels[4] <- "[I think that I would need the support of a technical person*** to be able to use this notation.] Please answer the following questions about the notation you just used:"
names(df_sc)[4] <- "NQ2. I think that I would need the support of a technical person to be able to use this notation."
# 5
attributes(df_sc)$variable.labels[5] <- "[I find the various functions* in this notation are well integrated.] Please answer the following questions about the notation you just used:"
names(df_sc)[5] <- "PQ3. I find the various functions in this notation are well integrated."
# 6
attributes(df_sc)$variable.labels[6] <- "[I think there is too much inconsistency** in this notation.] Please answer the following questions about the notation you just used:"
names(df_sc)[6] <- "NQ3. I think there is too much inconsistency in this notation."
# 7
attributes(df_sc)$variable.labels[7] <- "[I would imagine that most people would learn to use this notation very quickly.] Please answer the following questions about the notation you just used:"
names(df_sc)[7] <- "PQ4. I would imagine that most people would learn to use this notation very quickly."
# 8
attributes(df_sc)$variable.labels[8] <- "[I would find the notation very cumbersome to use.] Please answer the following questions about the notation you just used:"
names(df_sc)[8] <- "NQ4. I would find the notation very cumbersome to use."
# 9
attributes(df_sc)$variable.labels[9] <- "[I would feel very confident using the notation.] Please answer the following questions about the notation you just used:"
names(df_sc)[9] <- "PQ5. I would feel very confident using the notation."
# 10
attributes(df_sc)$variable.labels[10] <- "[I would need to learn a lot of things before I could get going with this notation.] Please answer the following questions about the notation you just used:"
names(df_sc)[10] <- "NQ5. I would need to learn a lot of things before I could get going with this notation."

# CNL
# 1
attributes(df_cnl)$variable.labels[1] <- "[I think that I would like to use this notation frequently.] Please answer the following questions about the notation you just used:"
names(df_cnl)[1] <- "PQ1. I think that I would like to use this notation frequently."
# 2
attributes(df_cnl)$variable.labels[2] <- "[I find the notation unnecessarily complex.] Please answer the following questions about the notation you just used:"
names(df_cnl)[2] <- "NQ1. I find the notation unnecessarily complex."
# 3
attributes(df_cnl)$variable.labels[3] <- "[I think the notation would be easy to use.] Please answer the following questions about the notation you just used:"
names(df_cnl)[3] <- "PQ2. I think the notation would be easy to use."
# 4
attributes(df_cnl)$variable.labels[4] <- "[I think that I would need the support of a technical person*** to be able to use this notation.] Please answer the following questions about the notation you just used:"
names(df_cnl)[4] <- "NQ2. I think that I would need the support of a technical person to be able to use this notation."
# 5
attributes(df_cnl)$variable.labels[5] <- "[I find the various functions* in this notation are well integrated.] Please answer the following questions about the notation you just used:"
names(df_cnl)[5] <- "PQ3. I find the various functions in this notation are well integrated."
# 6
attributes(df_cnl)$variable.labels[6] <- "[I think there is too much inconsistency** in this notation.] Please answer the following questions about the notation you just used:"
names(df_cnl)[6] <- "NQ3. I think there is too much inconsistency in this notation."
# 7
attributes(df_cnl)$variable.labels[7] <- "[I would imagine that most people would learn to use this notation very quickly.] Please answer the following questions about the notation you just used:"
names(df_cnl)[7] <- "PQ4. I would imagine that most people would learn to use this notation very quickly."
# 8
attributes(df_cnl)$variable.labels[8] <- "[I would find the notation very cumbersome to use.] Please answer the following questions about the notation you just used:"
names(df_cnl)[8] <- "NQ4. I would find the notation very cumbersome to use."
# 9
attributes(df_cnl)$variable.labels[9] <- "[I would feel very confident using the notation.] Please answer the following questions about the notation you just used:"
names(df_cnl)[9] <- "PQ5. I would feel very confident using the notation."
# 10
attributes(df_cnl)$variable.labels[10] <- "[I would need to learn a lot of things before I could get going with this notation.] Please answer the following questions about the notation you just used:"
names(df_cnl)[10] <- "NQ5. I would need to learn a lot of things before I could get going with this notation."

# KV
# 1
attributes(df_kv)$variable.labels[1] <- "[I think that I would like to use this notation frequently.] Please answer the following questions about the notation you just used:"
names(df_kv)[1] <- "PQ1. I think that I would like to use this notation frequently."
# 2
attributes(df_kv)$variable.labels[2] <- "[I find the notation unnecessarily complex.] Please answer the following questions about the notation you just used:"
names(df_kv)[2] <- "NQ1. I find the notation unnecessarily complex."
# 3
attributes(df_kv)$variable.labels[3] <- "[I think the notation would be easy to use.] Please answer the following questions about the notation you just used:"
names(df_kv)[3] <- "PQ2. I think the notation would be easy to use."
# 4
attributes(df_kv)$variable.labels[4] <- "[I think that I would need the support of a technical person*** to be able to use this notation.] Please answer the following questions about the notation you just used:"
names(df_kv)[4] <- "NQ2. I think that I would need the support of a technical person to be able to use this notation."
# 5
attributes(df_kv)$variable.labels[5] <- "[I find the various functions* in this notation are well integrated.] Please answer the following questions about the notation you just used:"
names(df_kv)[5] <- "PQ3. I find the various functions in this notation are well integrated."
# 6
attributes(df_kv)$variable.labels[6] <- "[I think there is too much inconsistency** in this notation.] Please answer the following questions about the notation you just used:"
names(df_kv)[6] <- "NQ3. I think there is too much inconsistency in this notation."
# 7
attributes(df_kv)$variable.labels[7] <- "[I would imagine that most people would learn to use this notation very quickly.] Please answer the following questions about the notation you just used:"
names(df_kv)[7] <- "PQ4. I would imagine that most people would learn to use this notation very quickly."
# 8
attributes(df_kv)$variable.labels[8] <- "[I would find the notation very cumbersome to use.] Please answer the following questions about the notation you just used:"
names(df_kv)[8] <- "NQ4. I would find the notation very cumbersome to use."
# 9
attributes(df_kv)$variable.labels[9] <- "[I would feel very confident using the notation.] Please answer the following questions about the notation you just used:"
names(df_kv)[9] <- "PQ5. I would feel very confident using the notation."
# 10
attributes(df_kv)$variable.labels[10] <- "[I would need to learn a lot of things before I could get going with this notation.] Please answer the following questions about the notation you just used:"
names(df_kv)[10] <- "NQ5. I would need to learn a lot of things before I could get going with this notation."

# now create a tidy df for each: sc,cnl,kv
# source: https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html

#
# SC
#

df_sc_tidy <- df_sc %>% 
  pivot_longer(
    1:10, 
    names_to = "question", 
    values_to = "answer"
  )

df_sc_tidy <- df_sc_tidy %>% arrange(match(question, 
                                           c("PQ1. I think that I would like to use this notation frequently.",
                                             "NQ1. I find the notation unnecessarily complex.",
                                             "PQ2. I think the notation would be easy to use.",
                                             "NQ2. I think that I would need the support of a technical person to be able to use this notation.",
                                             "PQ3. I find the various functions in this notation are well integrated.",
                                             "NQ3. I think there is too much inconsistency in this notation.",
                                             "PQ4. I would imagine that most people would learn to use this notation very quickly.",
                                             "NQ4. I would find the notation very cumbersome to use.",
                                             "PQ5. I would feel very confident using the notation.",
                                             "NQ5. I would need to learn a lot of things before I could get going with this notation.")))

df_sc_tidy <- df_sc_tidy %>% mutate(question = factor(question, 
                                                    levels = c("PQ5. I would feel very confident using the notation.",
                                                               "PQ4. I would imagine that most people would learn to use this notation very quickly.",
                                                               "PQ3. I find the various functions in this notation are well integrated.",
                                                               "PQ2. I think the notation would be easy to use.",
                                                               "PQ1. I think that I would like to use this notation frequently.",
                                                               "NQ5. I would need to learn a lot of things before I could get going with this notation.",
                                                               "NQ4. I would find the notation very cumbersome to use.",
                                                               "NQ3. I think there is too much inconsistency in this notation.",
                                                               "NQ2. I think that I would need the support of a technical person to be able to use this notation.",
                                                               "NQ1. I find the notation unnecessarily complex.")))

# Add new column named 'notion' based on positive or negative framing of question
# Source: https://stackoverflow.com/q/50001383/693052
df_sc_tidy <- df_sc_tidy %>%
  mutate(
    notion = case_when(
      question == "PQ1. I think that I would like to use this notation frequently." ~ "positive",
      question == "NQ1. I find the notation unnecessarily complex." ~ "negative",
      question == "PQ2. I think the notation would be easy to use." ~ "positive",
      question == "NQ2. I think that I would need the support of a technical person to be able to use this notation." ~ "negative",
      question == "PQ3. I find the various functions in this notation are well integrated." ~ "positive",
      question == "NQ3. I think there is too much inconsistency in this notation." ~ "negative",
      question == "PQ4. I would imagine that most people would learn to use this notation very quickly." ~ "positive",
      question == "NQ4. I would find the notation very cumbersome to use." ~ "negative",
      question == "PQ5. I would feel very confident using the notation." ~ "positive",
      question == "NQ5. I would need to learn a lot of things before I could get going with this notation." ~ "negative"
    )
  )

#
# CNL
#

df_cnl_tidy <- df_cnl %>% 
  pivot_longer(
    1:10, 
    names_to = "question", 
    values_to = "answer"
  )

df_cnl_tidy <- df_cnl_tidy %>% arrange(match(question, 
                                           c("PQ1. I think that I would like to use this notation frequently.",
                                             "NQ1. I find the notation unnecessarily complex.",
                                             "PQ2. I think the notation would be easy to use.",
                                             "NQ2. I think that I would need the support of a technical person to be able to use this notation.",
                                             "PQ3. I find the various functions in this notation are well integrated.",
                                             "NQ3. I think there is too much inconsistency in this notation.",
                                             "PQ4. I would imagine that most people would learn to use this notation very quickly.",
                                             "NQ4. I would find the notation very cumbersome to use.",
                                             "PQ5. I would feel very confident using the notation.",
                                             "NQ5. I would need to learn a lot of things before I could get going with this notation.")))

df_cnl_tidy <- df_cnl_tidy %>% mutate(question = factor(question, 
                                                      levels = c("PQ5. I would feel very confident using the notation.",
                                                                 "PQ4. I would imagine that most people would learn to use this notation very quickly.",
                                                                 "PQ3. I find the various functions in this notation are well integrated.",
                                                                 "PQ2. I think the notation would be easy to use.",
                                                                 "PQ1. I think that I would like to use this notation frequently.",
                                                                 "NQ5. I would need to learn a lot of things before I could get going with this notation.",
                                                                 "NQ4. I would find the notation very cumbersome to use.",
                                                                 "NQ3. I think there is too much inconsistency in this notation.",
                                                                 "NQ2. I think that I would need the support of a technical person to be able to use this notation.",
                                                                 "NQ1. I find the notation unnecessarily complex.")))

# Add new column named 'notion' based on positive or negative framing of question
# Source: https://stackoverflow.com/q/50001383/693052
df_cnl_tidy <- df_cnl_tidy %>%
  mutate(
    notion = case_when(
      question == "PQ1. I think that I would like to use this notation frequently." ~ "positive",
      question == "NQ1. I find the notation unnecessarily complex." ~ "negative",
      question == "PQ2. I think the notation would be easy to use." ~ "positive",
      question == "NQ2. I think that I would need the support of a technical person to be able to use this notation." ~ "negative",
      question == "PQ3. I find the various functions in this notation are well integrated." ~ "positive",
      question == "NQ3. I think there is too much inconsistency in this notation." ~ "negative",
      question == "PQ4. I would imagine that most people would learn to use this notation very quickly." ~ "positive",
      question == "NQ4. I would find the notation very cumbersome to use." ~ "negative",
      question == "PQ5. I would feel very confident using the notation." ~ "positive",
      question == "NQ5. I would need to learn a lot of things before I could get going with this notation." ~ "negative"
    )
  )

#
# KV
#

df_kv_tidy <- df_kv %>% 
  pivot_longer(
    1:10, 
    names_to = "question", 
    values_to = "answer"
  )

df_kv_tidy <- df_kv_tidy %>% arrange(match(question, 
                                             c("PQ1. I think that I would like to use this notation frequently.",
                                               "NQ1. I find the notation unnecessarily complex.",
                                               "PQ2. I think the notation would be easy to use.",
                                               "NQ2. I think that I would need the support of a technical person to be able to use this notation.",
                                               "PQ3. I find the various functions in this notation are well integrated.",
                                               "NQ3. I think there is too much inconsistency in this notation.",
                                               "PQ4. I would imagine that most people would learn to use this notation very quickly.",
                                               "NQ4. I would find the notation very cumbersome to use.",
                                               "PQ5. I would feel very confident using the notation.",
                                               "NQ5. I would need to learn a lot of things before I could get going with this notation.")))

df_kv_tidy <- df_kv_tidy %>% mutate(question = factor(question, 
                                                        levels = c("PQ5. I would feel very confident using the notation.",
                                                                   "PQ4. I would imagine that most people would learn to use this notation very quickly.",
                                                                   "PQ3. I find the various functions in this notation are well integrated.",
                                                                   "PQ2. I think the notation would be easy to use.",
                                                                   "PQ1. I think that I would like to use this notation frequently.",
                                                                   "NQ5. I would need to learn a lot of things before I could get going with this notation.",
                                                                   "NQ4. I would find the notation very cumbersome to use.",
                                                                   "NQ3. I think there is too much inconsistency in this notation.",
                                                                   "NQ2. I think that I would need the support of a technical person to be able to use this notation.",
                                                                   "NQ1. I find the notation unnecessarily complex.")))

# Add new column named 'notion' based on positive or negative framing of question
# Source: https://stackoverflow.com/q/50001383/693052
df_kv_tidy <- df_kv_tidy %>%
  mutate(
    notion = case_when(
      question == "PQ1. I think that I would like to use this notation frequently." ~ "positive",
      question == "NQ1. I find the notation unnecessarily complex." ~ "negative",
      question == "PQ2. I think the notation would be easy to use." ~ "positive",
      question == "NQ2. I think that I would need the support of a technical person to be able to use this notation." ~ "negative",
      question == "PQ3. I find the various functions in this notation are well integrated." ~ "positive",
      question == "NQ3. I think there is too much inconsistency in this notation." ~ "negative",
      question == "PQ4. I would imagine that most people would learn to use this notation very quickly." ~ "positive",
      question == "NQ4. I would find the notation very cumbersome to use." ~ "negative",
      question == "PQ5. I would feel very confident using the notation." ~ "positive",
      question == "NQ5. I would need to learn a lot of things before I could get going with this notation." ~ "negative"
    )
  )

# write to csv so we don't need to run above lines too often
write.csv(df_sc_tidy, "../sus_sc_likert_tidy.csv")
write.csv(df_cnl_tidy, "../sus_cnl_likert_tidy.csv")
write.csv(df_kv_tidy, "../sus_kv_likert_tidy.csv")



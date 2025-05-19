setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###
### Approach 1: IMPORT DATA from prepared excel csv ####
###
## Currently using approach 1:
## Reason is that alot of data has already been calculated (SUS score) before 

### using prepared excel data
mydata <- read.csv("data-survey-sus_export.csv", sep = ",", header = TRUE)

# boxcox transform sus
mydata$sus.boxcox = (mydata$sus^3.2-1)/3.2

# write to csv so we don't need to run above lines too often
write.csv(mydata, "../data-survey-sus_prepared.csv")

###
### Approach 2: IMPORT DATA from limesurvey csv dump ####
###

mydata <- read.csv("results-survey623939-anon.csv", sep = ",", header = TRUE)

# Preparing data_survey_prepared.csv
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


### for later use:

# add labels to numbers
for (i in 3:ncol(df)) {
  df[, i] <- factor(df[, i], levels=c("1","2","3","4","5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
}





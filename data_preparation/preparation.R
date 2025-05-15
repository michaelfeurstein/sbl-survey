setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###
### Approach 1: IMPORT DATA from prepared excel csv ###
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
### Approach 2: IMPORT DATA from limesurvey csv dump ###
###

mydata <- read.csv("results-survey623939-anon.csv", sep = ",", header = TRUE)

# Preparing data_survey_prepared.csv
# header: subject, id, seed, notation, sus

# remove unnecessary columns
df <- subset(mydata, select=-c(submitdate,lastpage,startlanguage,startdate,datestamp,PARTICIPANTTYPE.SQ001.,PARTICIPANTTYPE.SQ002.,PARTICIPANTTYPE.SQ003.,PARTICIPANTTYPE.other.,authorHOWMANYYEARS,authorHOWMANYVIDEOS,researchHOWMANYYEARS,researchHOWMANYPUB,userHOWMANYYEARS,userHOWMANYCOURSES,INFO01,INFO02,INFO03,READCHECK,NOTATION01HELP,COMMENTS01,NOTATION02HELP,COMMENTS02,NOtATION03HELP,COMMENTS03,NOTATIONOVERVIEW,RANKING.SQ001.,RANKING.SQ002.,RANKING.SQ003.,CLAIMEFFICIENCY.SQ001.,CLAIMEFFECTIVENESS.SQ001.,TEXTasFIRSTSTEP.SQ001.,TEXTwithGUI.SQ001.,GENERALCOMMENTS))

# replace text with numbers
# 1 - strongly disagree --> 5 - strongly agree
responseMapping <- c("1 - strongly disagree" = 1, "2" = 2, "3" = 3, "4" = 4, "5 - strongly agree" = 5)
for (i in 3:ncol(df)) {
  df[, i] <- responseMapping[df[, i]]
}

# add labels to numbers
for (i in 3:ncol(df)) {
  df[, i] <- factor(df[, i], levels=c("1","2","3","4","5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
}

### TODO: continue here -> calculate SUS scores in R




# this file is used as a pre sandbox for the cleaner R notebook (survey_analysis.Rmd)

library(dplyr)
if(!require(likert)){install.packages("likert")}
library(likert)
if(!require(svglite)){install.packages("svglite")}
library(svglite)
library(ggplot2)
library(ggtext)
if(!require(lemon)){install.packages("lemon")}
library(lemon)
library(stringr)
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)
if(!require(coin)){install.packages("coin")}
library(coin)
if(!require(multcomp)){install.packages("multcomp")}
library(multcomp)
if(!require(tsutils)){install.packages("tsutils")}
library(tsutils)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####
# imports ####
####

# read from prepared csv
df <- read.csv("data_prepared/data-survey-sus_prepared.csv")
df <- subset(df, select = -c(X))

# read from prepared csv
profiledata <- read.csv("data-survey-profiles_prepared.csv")

# post hoc questions prepared
posthocdata <- read.csv("data-post-hoc_prepared.csv")
posthocdata <- head(posthocdata,-1)

# sus responses prepared
df_sc_tidy <- read.csv("data_prepared/sus_sc_likert_tidy.csv")
df_cnl_tidy <- read.csv("data_prepared/sus_cnl_likert_tidy.csv")
df_kv_tidy <- read.csv("data_prepared/sus_kv_likert_tidy.csv")

# ranking prepared
ranking <- read.csv("data_prepared/data-survey-ranking_prepared.csv")

####
# profile ####
####

# participant profiles
profileSums <- colSums(profiledata == "Yes")
profile_columns = c("Profile","Frequency")
df_p = data.frame(matrix(nrow = 0, ncol = length(profile_columns))) 
colnames(df_p) = profile_columns
df_p[nrow(df_p) + 1,] = c("Author",profileSums["author"])
df_p[nrow(df_p) + 1,] = c("User",profileSums["user"])
df_p[nrow(df_p) + 1,] = c("Researcher",profileSums["researcher"])
df_p[nrow(df_p) + 1,] = c("Other",3) # manual count for profile author:no user:no researcher:no

# convert frequency column into numeric format
df_p$Frequency = as.numeric(as.character(df_p$Frequency))

# order dataframe ascending
# this way it's displayed descending in horizontal barplot
df_p <- df_p[order(df_p$Frequency),] 

# plot barplot
counts <- table(df_p$Frequency)
par(mar=c(3, 7, 3, 3))
profile_plot <- barplot(height=df_p$Frequency, names=df_p$Profile, main="Participant Profiles", horiz=T,las=1,xlim=c(0,50),width = c(2,2,2,2,2))
text(x=df_p$Frequency, profile_plot, labels = paste(df_p$Frequency), pos=4, offset=0.3, xpd=T)

# profile details ####
# Author 
## Years active
## How many years have you been authoring / producing educational videos, lecture
## recordings or other video-based learning content?
profiledata$authorHOWMANYYEARS <- as.numeric(as.character(profiledata$authorHOWMANYYEARS))
summary(na.omit(profiledata$authorHOWMANYYEARS))

## Videos created 
## How many educational videos, lecture recordings or other video-based learning
## content have you produced until now?
profiledata$authorHOWMANYVIDEOS <- as.numeric(as.character(profiledata$authorHOWMANYVIDEOS))
summary(na.omit(profiledata$authorHOWMANYVIDEOS))

# User 
## Years active
## How many years have you been using video-based learning content?
profiledata$userHOWMANYYEARS <- as.numeric(as.character(profiledata$userHOWMANYYEARS))
summary(na.omit(profiledata$userHOWMANYYEARS))

## Courses
## How many courses have you tought with the support of video-based learning con-
## tent? Please also count recurring courses, which possibly took place each semester.
profiledata$userHOWMANYCOURSES <- as.numeric(as.character(profiledata$userHOWMANYCOURSES))
summary(na.omit(profiledata$userHOWMANYCOURSES))

# Researcher
## Years researching
## How many years have you conducted research in the field of video-based learning
## / technology enhanced learning / educational technology?
profiledata$researchHOWMANYYEARS <- as.numeric(as.character(profiledata$researchHOWMANYYEARS))
summary(na.omit(profiledata$researchHOWMANYYEARS))

## Papers published
## How many papers have you published in the field of video-based learning? 
profiledata$researchHOWMANYPUB <- as.numeric(as.character(profiledata$researchHOWMANYPUB))
summary(na.omit(profiledata$researchHOWMANYPUB))

####
# dependent variables ####
####

## sus ####

### boxplot - visual inspection ####

par(mar=c(5, 5, 3, 2))
boxplot(sus ~ notation, data = df, main = "Usability (System Usability Scale)", xlab = "Notation", ylab = "SUS score", names = c("CNL","KV","SC"))

### reporting mean and sd: ####

df %>%
  group_by(notation) %>%
  summarize(mean = mean(sus), sd = sd(sus), min = min(sus), max = max(sus), med = median(sus), q1 = quantile(sus, 0.25), q3 = quantile(sus, 0.75))

### mean difference ####
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

### sus responses ####

# In preparation for plotting we setup a function to define a limit width for strings wrapped
# Source juliasilge (comment on github from Oct 4, 2022)
# Link: https://github.com/juliasilge/tidytext/issues/222
custom_labeler <- function(x) {
  x %>%
    str_replace("___.+$", "") %>%
    str_wrap(width = 50)
}

#### prepare ####

# First we use mutate and factor to bring questions (PQ and NQ) into desired order for plotting

#kv
df_kv_tidy <- df_kv_tidy %>% mutate(question_short = substr(question, start = 1, stop = 3))

df_kv_tidy <- df_kv_tidy %>% mutate(question_short = factor(question_short, 
                                                      levels = c("PQ5",
                                                                 "PQ4",
                                                                 "PQ3",
                                                                 "PQ2",
                                                                 "PQ1",
                                                                 "NQ5",
                                                                 "NQ4",
                                                                 "NQ3",
                                                                 "NQ2",
                                                                 "NQ1")))

df_kv_tidy_individual <- df_kv_tidy %>% mutate(question = factor(question, 
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

#cnl
df_cnl_tidy <- df_cnl_tidy %>% mutate(question_short = substr(question, start = 1, stop = 3))

df_cnl_tidy <- df_cnl_tidy %>% mutate(question_short = factor(question_short, 
                                                              levels = c("PQ5",
                                                                         "PQ4",
                                                                         "PQ3",
                                                                         "PQ2",
                                                                         "PQ1",
                                                                         "NQ5",
                                                                         "NQ4",
                                                                         "NQ3",
                                                                         "NQ2",
                                                                         "NQ1")))

df_cnl_tidy_individual <- df_cnl_tidy %>% mutate(question = factor(question, 
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

#sc
df_sc_tidy <- df_sc_tidy %>% mutate(question_short = substr(question, start = 1, stop = 3))

df_sc_tidy <- df_sc_tidy %>% mutate(question_short = factor(question_short, 
                                                            levels = c("PQ5",
                                                                       "PQ4",
                                                                       "PQ3",
                                                                       "PQ2",
                                                                       "PQ1",
                                                                       "NQ5",
                                                                       "NQ4",
                                                                       "NQ3",
                                                                       "NQ2",
                                                                       "NQ1")))

df_sc_tidy_individual <- df_sc_tidy %>% mutate(question = factor(question, 
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

# Then we factorize the Likert answer scale
#kv
df_kv_tidy <- df_kv_tidy %>% mutate(answer = factor(answer, 
                                                    levels = c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree')))

#cnl
df_cnl_tidy <- df_cnl_tidy %>% mutate(answer = factor(answer, 
                                                    levels = c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree')))

#sc
df_sc_tidy <- df_sc_tidy %>% mutate(answer = factor(answer, 
                                                      levels = c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree')))

#### 1: kv ####

# The actual plot code using ggplot2
# option 1: for separate figure use
survey_kv_sus_responses_individual <- ggplot(df_kv_tidy_individual, aes(x=question, fill=answer)) + 
  labs(title = "Key-Value Notation", subtitle = "Individual System Usability Scale Responses") +
  geom_bar(width = 0.7, position = position_stack(reverse = TRUE)) + 
  scale_fill_manual(values=c("darkred","red", "grey", "darkolivegreen1", "darkgreen")) +
  scale_y_continuous(expand = expansion(0)) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0), axis.title.y = element_blank(), axis.title.x =element_blank(), legend.position = "bottom", legend.justification = c(1,1), legend.title = element_blank()) +
  facet_rep_grid(notion ~ ., scales = "free", repeat.tick.labels = "all") +
  scale_x_discrete(labels = custom_labeler) +
  coord_capped_cart(bottom="both", left="both") +
  coord_flip()

# option 2+3: for horizontal + vertical figure use
survey_kv_sus_responses <- ggplot(df_kv_tidy, aes(x=question_short, fill=answer)) + 
  labs(title = "Key-Value Notation") +
  geom_bar(width = 0.7, position = position_stack(reverse = TRUE)) + 
  scale_fill_manual(values=c("darkred","red", "grey", "darkolivegreen1", "darkgreen")) +
  scale_y_continuous(expand = expansion(0)) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0), axis.title.y = element_blank(), axis.title.x =element_blank(), legend.position = "bottom", legend.justification = c(1,1), legend.title = element_blank()) +
  facet_rep_grid(notion ~ ., scales = "free", repeat.tick.labels = "all") +
  scale_x_discrete(labels = custom_labeler) +
  coord_capped_cart(bottom="both", left="both") +
  coord_flip()

#### 2: cnl ####

# The actual plot code using ggplot2
# option 1: for separate figure use
survey_cnl_sus_responses_individual <- ggplot(df_cnl_tidy_individual, aes(x=question, fill=answer)) + 
  labs(title = "Controlled Natural-Language Notation", subtitle = "Individual System Usability Scale Responses") +
  geom_bar(width = 0.7, position = position_stack(reverse = TRUE)) + 
  scale_fill_manual(values=c("darkred","red", "grey", "darkolivegreen1", "darkgreen")) +
  scale_y_continuous(expand = expansion(0)) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0), axis.title.y = element_blank(), axis.title.x =element_blank(), legend.position = "bottom", legend.justification = c(1,1), legend.title = element_blank()) +
  facet_rep_grid(notion ~ ., scales = "free", repeat.tick.labels = "all") +
  scale_x_discrete(labels = custom_labeler) +
  coord_capped_cart(bottom="both", left="both") +
  coord_flip()

# option 2+3: for horizontal + vertical figure use
survey_cnl_sus_responses <- ggplot(df_cnl_tidy, aes(x=question_short, fill=answer)) + 
  labs(title = "Controlled Natural-Language Notation") +
  geom_bar(width = 0.7, position = position_stack(reverse = TRUE)) + 
  scale_fill_manual(values=c("darkred","red", "grey", "darkolivegreen1", "darkgreen")) +
  scale_y_continuous(expand = expansion(0)) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0), axis.title.y = element_blank(), axis.title.x =element_blank(), legend.position = "bottom", legend.justification = c(1,1), legend.title = element_blank()) +
  facet_rep_grid(notion ~ ., scales = "free", repeat.tick.labels = "all") +
  scale_x_discrete(labels = custom_labeler) +
  coord_capped_cart(bottom="both", left="both") +
  coord_flip()

#### 3: sc ####

# The actual plot code using ggplot2
# option 1: for separate figure use
survey_sc_sus_responses_individual <- ggplot(df_sc_tidy_individual, aes(x=question, fill=answer)) + 
  labs(title = "Script Notation", subtitle = "Individual System Usability Scale Responses") +
  geom_bar(width = 0.7, position = position_stack(reverse = TRUE)) + 
  scale_fill_manual(values=c("darkred","red", "grey", "darkolivegreen1", "darkgreen")) +
  scale_y_continuous(expand = expansion(0)) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0), axis.title.y = element_blank(), axis.title.x =element_blank(), legend.position = "bottom", legend.justification = c(1,1), legend.title = element_blank()) +
  facet_rep_grid(notion ~ ., scales = "free", repeat.tick.labels = "all") +
  scale_x_discrete(labels = custom_labeler) +
  coord_capped_cart(bottom="both", left="both") +
  coord_flip()

# option 2+3: for horizontal + vertical figure use
survey_sc_sus_responses <- ggplot(df_sc_tidy, aes(x=question_short, fill=answer)) + 
  labs(title = "Script Notation") +
  geom_bar(width = 0.7, position = position_stack(reverse = TRUE)) + 
  scale_fill_manual(values=c("darkred","red", "grey", "darkolivegreen1", "darkgreen")) +
  scale_y_continuous(expand = expansion(0)) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0), axis.title.y = element_blank(), axis.title.x =element_blank(), legend.position = "bottom", legend.justification = c(1,1), legend.title = element_blank()) +
  facet_rep_grid(notion ~ ., scales = "free", repeat.tick.labels = "all") +
  scale_x_discrete(labels = custom_labeler) +
  coord_capped_cart(bottom="both", left="both") +
  coord_flip()

#### all: kv/cnl/sc ####

# combine with ggarange
# option 2: horizontal
sus_responses_horizontal <- ggarrange(survey_kv_sus_responses, survey_cnl_sus_responses, survey_sc_sus_responses, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

# option 3: vertical
sus_responses_all <- ggarrange(survey_kv_sus_responses, survey_cnl_sus_responses, survey_sc_sus_responses, ncol=1, nrow=3, common.legend = TRUE, legend="bottom")

bottom_text <- c("Questions: \n NQ1: I find the notation unnecessarily complex.\n NQ2: I think that I would need the support of a technical person to be able to use this notation.\n NQ3: I think there is too much inconsistency in this notation.\n NQ4: I would find the notation very cumbersome to use.\n NQ5: I would need to learn a lot of things before I could get going with this notation.\n PQ1: I think that I would like to use this notation frequently.\n PQ2: I think the notation would be easy to use.\n PQ3: I find the various functions in this notation are well integrated.\n PQ4: I would imagine that most people would learn to use this notation very quickly.\n PQ5: I would feel very confident using the notation.")

# option 2: horizontal
sus_responses_all_final_horizontal <- annotate_figure(sus_responses_horizontal,
                top = text_grob("Individual System Usability Scale Responses", hjust = 2.3, size = 14, face = "bold"),
                bottom = text_grob(bottom_text,
                                   hjust = 1, x = 1, size = 10)
)

# option 3: vertical
sus_responses_all_final <- annotate_figure(sus_responses_all,
                                           top = text_grob("Individual System Usability Scale Responses", hjust = 2.3, size = 14, face = "bold"),
                                           bottom = text_grob(bottom_text,
                                                              hjust = 1, x = 1, size = 10)
)

# save
# option 1: individual figures
# kv
ggsave(file="plots/survey_kv_sus_responses.svg", plot=survey_kv_sus_responses_individual, width = 8.15, height = 5.87)
ggsave(file="plots/survey_kv_sus_responses.eps", plot=survey_kv_sus_responses_individual, width = 8.15, height = 5.87)

# cnl
ggsave(file="plots/survey_cnl_sus_responses.svg", plot=survey_cnl_sus_responses_individual, width = 8.15, height = 5.87)
ggsave(file="plots/survey_cnl_sus_responses.eps", plot=survey_cnl_sus_responses_individual, width = 8.15, height = 5.87)

# sc
ggsave(file="plots/survey_sc_sus_responses.svg", plot=survey_sc_sus_responses_individual, width = 8.15, height = 5.87)
ggsave(file="plots/survey_sc_sus_responses.eps", plot=survey_sc_sus_responses_individual, width = 8.15, height = 5.87)

# option 2: horizontal
ggsave(file="plots/survey_all_sus_responses_horizontal.svg", plot=sus_responses_all_final_horizontal, width = 20, height = 10)
ggsave(file="plots/survey_all_sus_responses_horizontal.eps", plot=sus_responses_all_final_horizontal, width = 20, height = 10)

# option 3: vertical
ggsave(file="plots/survey_all_sus_responses_vertical.svg", plot=sus_responses_all_final, width = 10, height = 20)
ggsave(file="plots/survey_all_sus_responses_vertical.eps", plot=sus_responses_all_final, width = 10, height = 20)

## ranking ####

counts_SC <- count(ranking, RANKING.SQ001.)
counts_CNL <- count(ranking, RANKING.SQ002.)
counts_KV <- count(ranking, RANKING.SQ003.)

notation <- c(rep(c("SC"),4),rep(c("CNL"),4),rep(c("KV"),4))
ranking <- rep(c("1st place","2nd place","3rd place","no preference"), 3)  
counts <- c(counts_SC$n[2:5],counts_CNL$n[2:5],counts_KV$n[2:5])
data_ranking <- data.frame(notation,ranking,counts)

desired_order <- c("KV","CNL","SC")
# Re-order the levels
data_ranking$notation <- factor( as.character(data_ranking$notation), levels=desired_order )

# Stacked + count inside bars
survey_ranking_stacked <- ggplot(data_ranking, aes(fill=ranking, y=counts, x=notation)) + 
  geom_bar(position="fill", stat = "identity") +
  geom_text(aes(label=after_stat(counts)), stat="identity", position="fill", vjust = 1.2, size = 2) +
  scale_y_continuous(labels = scales::label_percent(scale = 55, prefix = "", suffix = "")) +
  labs(title = "Personal Ranking of Notations", subtitle = "Ranking based on personal preference for notation", x = "Notation", y = "Counts", fill = "Ranking") +
  theme_bw()

# save
ggsave(file="plots/survey_ranking_stacked.svg", plot=survey_ranking_stacked)
ggsave(file="plots/survey_ranking_stacked.eps", plot=survey_ranking_stacked)

####
# post-hoc questions ####
####

# get the sums of each column with likert levels
strong_disagree <- colSums(posthocdata == "1")
strong_disagree <- as.data.frame(strong_disagree)

disagree <- colSums(posthocdata == "2")
disagree <- as.data.frame(disagree)

neutral <- colSums(posthocdata == "3")
neutral <- as.data.frame(neutral)

agree <- colSums(posthocdata == "4")
agree <- as.data.frame(agree)

strong_agree <- colSums(posthocdata == "5")
strong_agree <- as.data.frame(strong_agree)

## q1-q4 ####

# setup 
col_titles = c("Item","Strong Disagree","Disagree","Neutral","Agree","Strong Agree")
df_posthoc_all = data.frame(matrix(nrow = 0, ncol = length(col_titles))) 
colnames(df_posthoc_all) = col_titles

# fill
df_posthoc_all[1, ] <- c("Q1",strong_disagree[5,],disagree[5,],neutral[5,],agree[5,],strong_agree[5,])
df_posthoc_all[2, ] <- c("Q2",strong_disagree[6,],disagree[6,],neutral[6,],agree[6,],strong_agree[6,])
df_posthoc_all[3, ] <- c("Q3",strong_disagree[7,],disagree[7,],neutral[7,],agree[7,],strong_agree[7,])
df_posthoc_all[4, ] <- c("Q4",strong_disagree[8,],disagree[8,],neutral[8,],agree[8,],strong_agree[8,])

# convert to numeric
i <- c(2:6)  
df_posthoc_all[, i] <- apply(df_posthoc_all[, i], 2, function(x) as.numeric(as.character(x)))
#sapply(df_posthoc_all, mode)

# plot
title <- "Post-hoc Questions"
q1_plot <- plot(likert(summary = df_posthoc_all), group.order = c("Q1", "Q2", "Q3", "Q4")) +
  labs(title = title, y = "Percentage", caption = "<b>Q1 Efficiency:</b> Do you agree with the statement that a text-based approach could be more efficient?<br><b>Q2 Effectiveness:</b> Do you agree with the statement that a text-based approach could be more effective?<br><b>Q3 Textual notation as a first step:</b> Could you imagine using a textual notation as a starting point to sketch out<br>the main structure of a learning module in order to continue editing in a graphical user interface (GUI)?<br><b>Q4 Textual notation combined with GUI:</b> Could you imagine using a textual notation in combination with<br>an editing environment in a graphical user interface (GUI), allowing you to refine and switch between<br>textual notation and GUI representation?") +
  theme(plot.caption = element_markdown())

q1_plot

#save in variable image to export to svg
image=q1_plot
#save
ggsave(file="plots/posthoc_questions.svg", plot=image, width=7, height=5)

## q1: efficiency ####

# setup 
col_titles = c("Item","Strong Disagree","Disagree","Neutral","Agree","Strong Agree")
df_posthoc_q1 = data.frame(matrix(nrow = 0, ncol = length(col_titles))) 
colnames(df_posthoc_q1) = col_titles

# fill
df_posthoc_q1[1, ] <- c("claim.efficiency",strong_disagree[5,],disagree[5,],neutral[5,],agree[5,],strong_agree[5,])

# convert to numeric
i <- c(2:6)  
df_posthoc_q1[, i] <- apply(df_posthoc_q1[, i], 2, function(x) as.numeric(as.character(x)))
#sapply(df_posthoc_q1, mode)

# plot
title <- "Efficiency of a text-based approach"
subtitle <- "Do you agree with the statement that a text-based approach could be more\nefficient in order to create a video-based learning module?"
plot(likert(summary = df_posthoc_q1)) +
  labs(title = title, subtitle = subtitle, y = "Percentage", caption = "see Appendix for detailed formulation of question") +
  theme(axis.text.y = element_blank())

#save in variable image to export to svg
image=plot(likert(summary = df_posthoc_q1)) +
  labs(title = title, subtitle = subtitle, y = "Percentage", caption = "see Appendix for detailed formulation of question") +
  theme(axis.text.y = element_blank())
#save
ggsave(file="plots/posthoc_q1_efficiency.svg", plot=image, width=8, height=3)

## q2: effectiveness ####

# setup 
col_titles = c("Item","Strong Disagree","Disagree","Neutral","Agree","Strong Agree")
df_posthoc_q2 = data.frame(matrix(nrow = 0, ncol = length(col_titles)))
colnames(df_posthoc_q2) = col_titles

# fill
df_posthoc_q2[1, ] <- c("claim.effectiveness",strong_disagree[6,],disagree[6,],neutral[6,],agree[6,],strong_agree[6,])

# convert to numeric
i <- c(2:6)
df_posthoc_q2[, i] <- apply(df_posthoc_q2[, i], 2, function(x) as.numeric(as.character(x)))
#sapply(df_posthoc_q2, mode)

# plot
title <- "Effectiveness of a text-based approach"
subtitle <- "Do you agree with the statement that a text-based approach could be more\neffective in order to create a video-based learning module?"
plot(likert(summary = df_posthoc_q2)) +
  labs(title = title, subtitle = subtitle, y = "Percentage", caption = "see Appendix for detailed formulation of question") +
  theme(axis.text.y = element_blank())

#save in variable image to export to svg
image=plot(likert(summary = df_posthoc_q2)) +
  labs(title = title, subtitle = subtitle, y = "Percentage", caption = "see Appendix for detailed formulation of question") +
  theme(axis.text.y = element_blank())
#save
ggsave(file="plots/posthoc_q2_effectiveness.svg", plot=image, width=8, height=3)

## q3: text as first step ####

# setup 
col_titles = c("Item","Strong Disagree","Disagree","Neutral","Agree","Strong Agree")
df_posthoc_q3 = data.frame(matrix(nrow = 0, ncol = length(col_titles)))
colnames(df_posthoc_q3) = col_titles

# fill
df_posthoc_q3[1, ] <- c("claim.effectiveness",strong_disagree[7,],disagree[7,],neutral[7,],agree[7,],strong_agree[7,])

# convert to numeric
i <- c(2:6)
df_posthoc_q3[, i] <- apply(df_posthoc_q3[, i], 2, function(x) as.numeric(as.character(x)))
#sapply(df_posthoc_q3, mode)

# plot
title <- "Using a textual notation as a first step"
subtitle <- "Could you imagine using a textual notation as a starting point\nto sketch out the main structure of a video-based learning module\nin order to continue editing in a graphical user interface (GUI)?"
plot(likert(summary = df_posthoc_q3)) +
  labs(title = title, subtitle = subtitle, y = "Percentage", caption = "see Appendix for detailed formulation of question") +
  theme(axis.text.y = element_blank())

#save in variable image to export to svg
image=plot(likert(summary = df_posthoc_q3)) +
  labs(title = title, subtitle = subtitle, y = "Percentage", caption = "see Appendix for detailed formulation of question") +
  theme(axis.text.y = element_blank())
#save
ggsave(file="plots/posthoc_q3_text-first-step.svg", plot=image, width=8, height=3)

## q4: text with GUI ####

# setup 
col_titles = c("Item","Strong Disagree","Disagree","Neutral","Agree","Strong Agree")
df_posthoc_q4 = data.frame(matrix(nrow = 0, ncol = length(col_titles)))
colnames(df_posthoc_q4) = col_titles

# fill
df_posthoc_q4[1, ] <- c("claim.effectiveness",strong_disagree[8,],disagree[8,],neutral[8,],agree[8,],strong_agree[8,])

# convert to numeric
i <- c(2:6)
df_posthoc_q4[, i] <- apply(df_posthoc_q4[, i], 2, function(x) as.numeric(as.character(x)))
#sapply(df_posthoc_q4, mode)

# plot
title <- "Using a textual notation in combination with a graphical user interface
(GUI)"
subtitle <- "Could you imagine using a textual notation in combination with an editing environment\nin a graphical user interface (GUI), allowing you to refine and switch\nbetween textual notation and GUI representation?"
plot(likert(summary = df_posthoc_q4)) +
  labs(title = title, subtitle = subtitle, y = "Percentage", caption = "see Appendix for detailed formulation of question") +
  theme(axis.text.y = element_blank())

#save in variable image to export to svg
image=plot(likert(summary = df_posthoc_q4)) +
  labs(title = title, subtitle = subtitle, y = "Percentage", caption = "see Appendix for detailed formulation of question") +
  theme(axis.text.y = element_blank())
#save
ggsave(file="plots/posthoc_q4_text-with-gui.svg", plot=image, width=8, height=3)

# Analysis ####

## Usability ####

# Option 1 from Source 1
# creates boxplots of differences - no time to dig into code for CI plots
# Source: https://www.r-statistics.com/2010/02/post-hoc-analysis-for-friedmans-test-r-code/
source("https://www.r-statistics.com/wp-content/uploads/2010/02/Friedman-Test-with-Post-Hoc.r.txt")  # loading the friedman.test.with.post.hoc function from the internet

friedman.test.with.post.hoc(sus ~ notation | subject, data = df, to.plot.parallel = F)

# Option 2 from Sources 2-4
#tsutils
df_wide <- df %>% pivot_wider(names_from = c(notation), values_from = c(sus))
df_wide <- df_wide %>% dplyr::select(sc,cnl,kv)
result = nemenyi(df_wide,conf.level=0.95,plottype='vmcb')
result$means[2]
result$k

# Option 3 DIY - results don't really make sense - stick with Option 2
# Source: https://bookdown.org/logan_kelly/r_practice/p09.html
# Source: https://stackoverflow.com/a/58739988/693052 
# try and calculate CIs of mean difference yourself
# we have means from result object option 2above (tsutils)
cnl_kv_diff <- result$means[3] - result$means[2]
sc_kv_diff <- result$means[2] - result$means[1]
sc_cnl_diff <- result$means[3] - result$means[1]


# calculate SDs of differences
cnl_kv_sd <- sqrt(result$means[3] + result$means[2])
sc_kv_sd <- sqrt(result$means[2] + result$means[1])
sc_cnl_sd <- sqrt(result$means[3] + result$means[1])

# calculate 95% CI for each
cnl_kv_ci <- cnl_kv_diff + cnl_kv_sd * qt(c(0.025, 0.975), nrow(df_wide)*2)
sc_kv_ci <- sc_kv_diff + sc_kv_sd * qt(c(0.025, 0.975), nrow(df_wide)*2)
sc_cnl_ci <- sc_cnl_diff + sc_cnl_sd * qt(c(0.025, 0.975), nrow(df_wide)*2)

sc_cnl_ci

cnl_kv_ci[1]
cnl_kv_diff
cnl_kv_ci[2]

# plot cnl-kv
sus_original_CI=data.frame(contrast="CNL-KV", lower=cnl_kv_ci[1], mean=cnl_kv_diff, upper=cnl_kv_ci[2])

ggplot(data=sus_original_CI) +
  geom_bar(aes(x=contrast, y=mean), stat="identity", fill="lightblue", position = position_nudge(y = 0)) +
  geom_pointrange(mapping=aes(x=contrast, y=mean, ymin=lower, ymax=upper), size=1, color="black", fill="white", shape=22) +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_y_continuous(limits=c(-5,5)) +
  coord_flip() +
  ylab('Ratio of mean difference') +
  xlab('CNL-KV') + 
  labs(title = "Ratio of differences for mean levels of SUS scores", subtitle = "Tukey HSD: 95% confidence interval", caption = "Note: Mean differences and CI bounds are back-transformed to original scale as a ratio. \n Line of no effect is shifted to 1.") + theme_bw() + theme(legend.position="none",
                                                                                                                                                                                                                                                                                      axis.title.x=element_text(size=12),
                                                                                                                                                                                                                                                                                      axis.text.x=element_text(size=12),
                                                                                                                                                                                                                                                                                      axis.title.y=element_blank(),
                                                                                                                                                                                                                                                                                      axis.text.y=element_text(size=12))

# plot cnl-sc
sus_original_CI=data.frame(contrast="SC-CNL", lower=sc_cnl_ci[1], mean=sc_cnl_diff, upper=sc_cnl_ci[2])

ggplot(data=sus_original_CI) +
  geom_bar(aes(x=contrast, y=mean), stat="identity", fill="lightblue", position = position_nudge(y = 0)) +
  geom_pointrange(mapping=aes(x=contrast, y=mean, ymin=lower, ymax=upper), size=1, color="black", fill="white", shape=22) +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_y_continuous(limits=c(-4,5)) +
  coord_flip() +
  ylab('Ratio of mean difference') +
  xlab('CNL-KV') + 
  labs(title = "Ratio of differences for mean levels of SUS scores", subtitle = "Tukey HSD: 95% confidence interval", caption = "Note: Mean differences and CI bounds are back-transformed to original scale as a ratio. \n Line of no effect is shifted to 1.") + theme_bw() + theme(legend.position="none",
                                                                                                                                                                                                                                                                                      axis.title.x=element_text(size=12),
                                                                                                                                                                                                                                                                                      axis.text.x=element_text(size=12),
                                                                                                                                                                                                                                                                                      axis.title.y=element_blank(),
                                                                                                                                                                                                                                                                                      axis.text.y=element_text(size=12))

# plot kv-sc
sus_original_CI=data.frame(contrast="SC-KV", lower=sc_kv_ci[1], mean=sc_kv_diff, upper=sc_kv_ci[2])

ggplot(data=sus_original_CI) +
  geom_bar(aes(x=contrast, y=mean), stat="identity", fill="lightblue", position = position_nudge(y = 0)) +
  geom_pointrange(mapping=aes(x=contrast, y=mean, ymin=lower, ymax=upper), size=1, color="black", fill="white", shape=22) +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_y_continuous(limits=c(-4,5)) +
  coord_flip() +
  ylab('Ratio of mean difference') +
  xlab('CNL-KV') + 
  labs(title = "Ratio of differences for mean levels of SUS scores", subtitle = "Tukey HSD: 95% confidence interval", caption = "Note: Mean differences and CI bounds are back-transformed to original scale as a ratio. \n Line of no effect is shifted to 1.") + theme_bw() + theme(legend.position="none",
                                                                                                                                                                                                                                                                                      axis.title.x=element_text(size=12),
                                                                                                                                                                                                                                                                                      axis.text.x=element_text(size=12),
                                                                                                                                                                                                                                                                                      axis.title.y=element_blank(),
                                                                                                                                                                                                                                                                                      axis.text.y=element_text(size=12))

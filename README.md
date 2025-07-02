**Replication Package for Survey on Perceived Usability**

Steps to replicate results & plots:

(0) Power analysis [survey_power.R](/survey_power.R)

(1) Run [/data_preparation/preparation.R](/data_preparation/preparation.R)
- limesurvey dump file (csv) is prepared for data analysis into 7 separate CSVs stored in /data_prepared

(2) Run [survey_analysis.Rmd](survey_analysis.Rmd)
- generates plots into /plots folder\
- generates HTML to read up on analysis steps and results

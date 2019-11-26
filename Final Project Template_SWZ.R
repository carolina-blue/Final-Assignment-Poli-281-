rm(list=ls())
library(ggplot2)

setwd("~/OneDrive - University of North Carolina at Chapel Hill/Senior Year/Fall 2019/POLI 281/Assignments/Final Project")

# Load data
anes_FTF_2016 <- read.csv("anes_FTF_2016.csv")

# Start to create a simplified data frame with just the variables we'll need. (I'm doing just the DV. You'll add to this.)
anes <- data.frame(anes_FTF_2016$V162079) # Trump thermometer
colnames(anes)[1] <- "trump_therm" # Rename it 
anes$clinton_therm <- anes_FTF_2016$V162078


# Convert special variable codes to be NA. Note that, as a general matter, negative numbers refer to various kinds of missing data. (Respondent refused to answer the question, stopped the interview, or something else.) In this case, the Post-election questionnaire told me that codes of 998 and 999 refer to people who said they didn't know Trump or Clinton.
anes$clinton_therm[anes$clinton_therm < 0] <- NA
anes$clinton_therm[anes$clinton_therm > 100] <- NA
anes$trump_therm[anes$trump_therm < 0] <- NA
anes$trump_therm[anes$trump_therm > 100] <- NA

# Create and examine Trump minus Clinton thermometer
anes$t_minus_c <- anes$trump_therm - anes$clinton_therm
summary(anes)
ggplot(anes, aes(x=anes$t_minus_c)) + geom_histogram()

anes$party_ID <- anes_FTF_2016$V161155 #Party ID
anes$education <- anes_FTF_2016$V161270 #High Level of Education
anes$years_Senators <- anes_FTF_2016$V161513 # How many years Senators are elected for?
anes$spend_least <- anes_FTF_2016$V161514 # What do we spend the least on?
anes$house_control <- anes_FTF_2016$V161515  # Which party had control in House?
anes$senate_control <- anes_FTF_2016$V161516 # Which party had control in Senate?
anes$PK <- 0


# Code to drop incomplete cases--might come in handy.
anes <- anes[complete.cases(anes),]

#Calculate PK Scores
anes$PK[anes$years_Senators == 6] <- anes$PK[anes$years_Senators == 6] + .25
anes$PK[anes$spend_least == 1] <- anes$PK[anes$spend_least == 1] + .25
anes$PK[anes$house_control == 2] <- anes$PK[anes$house_control == 2] + .25
anes$PK[anes$senate_control == 2] <- anes$PK[anes$senate_control == 2] + .25

#Then Subset ANES to Rep, Dem and Ind.
anes_Dem <- data.frame(subset(anes, anes$party_ID == 1))
anes_Rep <- data.frame(subset(anes, anes$party_ID == 2))
anes_Ind <- data.frame(subset(anes, anes$party_ID == 3))

#Run Analysis


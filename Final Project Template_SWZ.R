rm(list=ls())

library(ggplot2)

setwd()

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


# The following lines of code add our Independent Variable (Respondent's degree of Political Knowledge) to the 'anes' data frame created by Professor Ryan. This is part of our data wrangling; selecting and manipulating the data into a format that facilitates the running of the analysis we are interested in

# PK questions between quotations are shortened and not exact copies. For exact question, refer to our appendix or look up variable in variable list
anes$party_ID <- anes_FTF_2016$V161155 # Respondent's Party ID
anes$education <- anes_FTF_2016$V161270 # Respondent's Highest Level of Education, a possible confounder to investigate later
anes$years_Senators <- anes_FTF_2016$V161513 # PK question: "How many years Senators are elected for?"
anes$spend_least <- anes_FTF_2016$V161514 # PK question: "What does Federal Gov. currently spend the least on?"
anes$house_control <- anes_FTF_2016$V161515  # PK question: "Which party had control in House?"
anes$senate_control <- anes_FTF_2016$V161516 # PK question: "Which party had control in Senate?"
anes$PK <- 0 # Creates column filled (temporarily) with 0's. This column will become each respondent's Political Knowledge score based on the above PK questions. I.e. it will become our IV


# Code to drop incomplete cases--might come in handy.
anes <- anes[complete.cases(anes),]

# Calculates PK Scores. The number after the '==' indicates the factually correct answer to the PK question. If respondents answered correctly. They receive + 0.25. For why our group chose + 0.25 as the number, please refer to our discussion section on P X (FILL THIS IN)
anes$PK[anes$years_Senators == 6] <- anes$PK[anes$years_Senators == 6] + .25
anes$PK[anes$spend_least == 1] <- anes$PK[anes$spend_least == 1] + .25
anes$PK[anes$house_control == 2] <- anes$PK[anes$house_control == 2] + .25
anes$PK[anes$senate_control == 2] <- anes$PK[anes$senate_control == 2] + .25

#Cleans up and ranks the education variable given from the ANES survey.
anes$edu_ranked <- NA
anes$edu_ranked[anes$education == 1 | anes$education == 2 |anes$education == 3 | anes$education == 4] <- 1 # Below High School
anes$edu_ranked[anes$education == 5 | anes$education == 6 | anes$education == 7 | anes$education == 8] <- 2 # Some High School, No Diploma
anes$edu_ranked[anes$education == 9] <- 3 # High School Diploma or Equivalent
anes$edu_ranked[anes$education == 10] <- 4 # Some College, No Degree
anes$edu_ranked[anes$education == 11 | anes$education == 12] <- 5 # Associates Degree
anes$edu_ranked[anes$education == 13] <- 6 # Bachelors Degree
anes$edu_ranked[anes$education == 14] <- 7 # Masters Degree
anes$edu_ranked[anes$education == 15 | anes$education == 16] <- 8 # Doctorate Degree & Professional School

# Creates subsets from 'anes' according to party identification: Republicans, Democrats and Independents. Allows for additional analysis which controls for party identification.
# The party_ID column also includes 3 other values we decided not to include. Refer to discussion X for full explanation. For reference, those values are:
# "0 = No Preference"
# "5 = Other Party, Specify"
# "-8 = Don't Know"
# "- 9 = Refused" (refused to answer)

anes_Dem <- data.frame(subset(anes, anes$party_ID == 1)) # Democrat identification subset
anes_Rep <- data.frame(subset(anes, anes$party_ID == 2)) # Republican identification subset
anes_Ind <- data.frame(subset(anes, anes$party_ID == 3)) # Independent identification subset

#Run Analysis

#Overall Analysis
ovr_fit <- lm(anes$t_minus_c ~ anes$PK)
summary(ovr_fit)

ovr_fit2 <- lm(anes$t_minus_c ~ anes$PK + anes$education) #Control for Education
summary(ovr_fit2)

ovr_graph <- ggplot(anes, aes(x = PK, y = t_minus_c)) + geom_point() + geom_smooth(method='lm', formula= y~x, color = "purple") + geom_hline(yintercept = 0) + xlab("Political Knowledge Score") + ylab("Candidate Thermometer") + ggtitle("Effect of Political Knowledge on Candidate Likeness \nOverall")
ovr_graph

#Democrat Analysis
dem_fit <-lm(anes_Dem$t_minus_c ~ anes_Dem$PK) 
summary(dem_fit)

dem_fit2 <-lm(anes_Dem$t_minus_c ~ anes_Dem$PK + anes_Dem$education) #Control for Education
summary(dem_fit2)

Dem_graph <- ggplot(anes_Dem, aes(x = PK, y = t_minus_c)) + geom_point() + geom_smooth(method='lm', formula= y~x, color = "blue") + geom_hline(yintercept = 0) + xlab("Political Knowledge Score") + ylab("Candidate Thermometer") + ggtitle("Effect of Political Knowledge on Candidate Likeness \n among Democrats")
Dem_graph

#Republican Analysis
rep_fit <- lm(anes_Rep$t_minus_c ~ anes_Rep$PK) 
summary(rep_fit)

rep_fit2 <-lm(anes_Rep$t_minus_c ~ anes_Rep$PK + anes_Rep$education) #Control for Education
summary(rep_fit2)

Rep_graph <- ggplot(anes_Rep, aes(x = PK, y = t_minus_c)) + geom_point() + geom_smooth(method='lm', formula= y~x, color = "red") + geom_hline(yintercept = 0) + xlab("Political Knowledge Score") + ylab("Candidate Thermometer") + ggtitle("Effect of Political Knowledge on Candidate Likeness \n among Republicans")
Rep_graph

#Independent Analysis
ind_fit <- lm(anes_Ind$t_minus_c ~ anes_Ind$PK) 
summary(ind_fit)

ind_fit2 <- lm(anes_Ind$t_minus_c ~ anes_Ind$PK + anes_Ind$education) #Control for Education
summary(ind_fit2)

Ind_graph <- ggplot(anes_Ind, aes(x = PK, y = t_minus_c)) + geom_point() + geom_smooth(method='lm', formula= y~x, color = "green") + geom_hline(yintercept = 0) + xlab("Political Knowledge Score") + ylab("Candidate Thermometer") + ggtitle("Effect of Political Knowledge on Candidate Likeness \n among Independents")
Ind_graph

#Combined Graph
comb_graph <- ggplot(anes, aes(x = PK, y = t_minus_c)) + geom_point() +geom_smooth(method='lm', formula= y~x, color = "purple") + geom_smooth(data = anes_Dem, method = 'lm', formula= y~x, color = "blue") + geom_smooth(data = anes_Rep, method = 'lm', formula = y~x, color = "red") + geom_smooth(data = anes_Ind, method = 'lm', formula= y~x, color = "green") + geom_hline(yintercept = 0) + xlab("Political Knowledge Score") + ylab("Candidate Thermometer") + ggtitle("Effect of Political Knowledge on Candidate Likeness") + theme(legend.position = "right") 
comb_graph
       
  

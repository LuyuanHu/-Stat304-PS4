#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from 
# Author: Luyuan Hu, Jiayi Wang, Linwei Yao & Bihan Lu
# Data: 22 October 2020
# Contact: luyuan.hu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(foreign)
library(mice)
library(e1071)

# Read in the raw data (You might need to change this if you use a different dataset)
raw_data1 <- read_dta("inputs/data/ns20200625.dta")
# Add the labels
raw_data1 <- labelled::to_factor(raw_data1)
# Just keep some variables
reduced_data1 <- 
  raw_data1 %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)

#

x <- as.data.frame(table(reduced_data1$state)) 
p = ggplot(x, aes(x = "", y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y")   ## 把柱状图折叠成饼图（极坐标）
p

length(table(reduced_data1$race_ethnicity))



#### Clean up ####



reduced_data1 <- reduced_data1 %>% 
  mutate(Trump = ifelse(vote_2020=="Donald Trump", 1, 0)) 

reduced_data1 <- reduced_data1 %>% 
  mutate(Biden = ifelse(vote_2020=="Joe Biden", 1, 0)) 

reduced_data1 <- reduced_data1 %>% 
  mutate(hispan = ifelse(hispanic=="Not Hispanic", 0, 1)) 

reduced_data1 <- reduced_data1 %>% 
  mutate(whiteshare = ifelse(race_ethnicity=="White", 1, 0)) 

reduced_data1 <- reduced_data1 %>% 
  mutate(blackshare = ifelse(race_ethnicity=="Black, or African American", 1, 0)) 


cces <- reduced_data1 %>%
  mutate(
         agegroup = ifelse(age >= 18 & age <= 29, "18-29",
                           ifelse(age >= 30 & age <= 44, "30-44",
                                  ifelse(age >= 45 & age <= 54, "45-54",
                                         ifelse(age >= 55 & age <= 64, "55-64",
                                                ifelse(age >= 65, "65+", NA))))),
         age.2 = ifelse(age >= 18 & age <= 44, "18-44",
                        ifelse(age >= 45, "45+", NA)),
         educ6 = ifelse(education == "No HS", "Non-High School Graduate",
                        ifelse(education == "High school graduate" |education == "Completed some high school", "High School Graduate",
                               ifelse(education == "Some college"|education == "Completed some college, but no degree"|education=="Associate Degree", "Some College",
                                      ifelse(education == "College Degree (such as B.A., B.S.)", "College Degree",
                                             ifelse(education == "Masters degree", "Masters degree",
                                                    ifelse(education == "Doctorate degree", "Doctorate degree", NA)))))))

cces <- cces %>%
  mutate(ideo5.pre = ifelse(ideo5 == "Not sure", NA, 
                            ifelse(ideo5 == "Very liberal", "Very liberal",
                                   ifelse(ideo5 == "Liberal", "Liberal",
                                          ifelse(ideo5 == "Moderate", "Moderate",
                                                 ifelse(ideo5 == "Conservative", "Conservative",
                                                        ifelse(ideo5 == "Very conservative", "Very conservative", NA)))))),
         ideo5.pre.r = ifelse(ideo5 == "Not sure" , NA, 
                              ifelse(ideo5 == "Very liberal", 1,
                                     ifelse(ideo5 == "Liberal", 2,
                                            ifelse(ideo5 == "Moderate", 3,
                                                   ifelse(ideo5 == "Conservative", 4,
                                                          ifelse(ideo5 == "Very conservative", 5, NA)))))),
         ideo3.pre = ifelse(ideo5.pre == "Very liberal" | ideo5.pre == "Liberal", "Liberal",
                            ifelse(ideo5.pre == "Very conservative" | ideo5.pre == "Conservative", "Conservative",
                                   ifelse(ideo5.pre == "Moderate", "Moderate", NA))))



# MUTATE: income & retire ------
cces <- cces %>%
  mutate(income.6 = ifelse(household_income == "Less than $14,999" | household_income == "$15,000 to $19,999" | household_income == "$20,000 to $24,999"|household_income == "$25,000 to $29,999", "$30k>",
                           ifelse(household_income == "$30,000 to $34,999" | household_income == "$35,000 to $39,999"|household_income == "$40,000 to $44,999" | household_income == "$45,000 to $49,999", "$30k-50k",
                                  ifelse(household_income == "$50,000 to $54,999" | household_income == "$55,000 to $59,999"|household_income == "$60,000 to $64,999" | household_income == "$65,000 to $69,999", "$50k-70k",
                                         ifelse(household_income == "$70,000 to 74,999" | household_income == "$75,000 to $79,999"|household_income == "$80,000 to $84,999" | household_income == "$85,000 to $89,999"|household_income == "$90,000 to $94,999" | household_income == "$95,000 to $99,999", "$70k-100k",
                                                ifelse(household_income == "$100,000 to $124,999" | household_income == "$125,000 to $149,999" | household_income == "$150,000 to $174,999"| household_income ==  "$175,000 to $199,999", "$100k<",NA))))),
         retired = ifelse(employment == "Retired", 1,0),
         labor_force = ifelse(employment == "Full-time employed"|employment == "Self-employed"|employment =="Part-time employed","in the labor force","not in the labor force")
  )

cces <- cces %>%
  mutate(
    race3 = ifelse(race_ethnicity == "White", "White",
                   ifelse(race_ethnicity == "Black, or African American", "Black", "Other")),
    vote.2 = ifelse(vote_2020=="Donald Trump", "Trump",
                                  ifelse(vote_2020=="Joe Biden", "Biden", NA)))


survey <- cces %>%
  select(vote.2,
         race3,
         agegroup,
         gender,
         income.6,
         educ6,
         census_region,
         labor_force
         )

survey.na_omit <- survey %>% na.omit()


NB_Predictions = predict(Naive_Bayes_Model,
                         as.matrix(post_stra),
                         type = "raw") # type="class" or "raw"





NB_Predictions[,1][1:10]

NB_Predictions <- NB_Predictions[,1] %>% 
  as.vector()

NB_Predictions <- ifelse(NB_Predictions>0.5,"Trump","Biden")





# what about logit? 
survey.logit <- survey.na_omit %>% 
  mutate(vote.2.binary = ifelse(vote.2=="Trump",1,0))

fit <- glm(vote.2.binary ~ . ,
           survey.logit %>% as.data.frame() %>% select(-vote.2),
           family=binomial(link='logit'))


LG_Predictions <- ifelse(predict(fit,post_stra) > 0.5,"Trump","Biden")




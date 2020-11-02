#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://usa.ipums.org/usa/index.shtml
# Author: Luyuan Hu, Jiayi Wang, Linwei Yao & Bihan Lu
# Data: 22 October 2020
# Contact: luyuan.hu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- read_dta("inputs/data/usa_00004.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_data)

reduced_data <- 
  raw_data %>% 
  select(
         sex, 
         age, 
         race, 
         hispan,
         educd,
         region,
         labforce,
         ftotinc)



reduced_data[which(as.integer(reduced_data$age) < 18),] <- NA

reduced_data <- reduced_data %>% na.omit()

reduced_data <- reduced_data %>% 
  mutate(gender = ifelse(sex=="male", "Male", "Female"),
         agegroup = ifelse(as.integer(age) >= 18 & as.integer(age) <= 29, "18-29",
                           ifelse(as.integer(age) >= 30 & as.integer(age) <= 44, "30-44",
                                  ifelse(as.integer(age) >= 45 & as.integer(age) <= 54, "45-54",
                                         ifelse(as.integer(age) >= 55 & as.integer(age) <= 64, "55-64",
                                                ifelse(as.integer(age) >= 65, "65+", NA))))),
         educ6 =          ifelse(educd == "some college, but less than 1 year"|educd == "associate's degree, type not specified", "Some College",
                               ifelse(educd == "regular high school diploma", "High School Graduate",
                                      ifelse(educd == "professional degree beyond a bachelor's degree", "College Degree",
                                             ifelse(educd ==  "master's degree", "Masters degree",
                                                    ifelse(educd == "doctoral degree", "Doctorate degree", "Non-High School Graduate"))))),
         census_region = ifelse(region=="east north central div", "Northeast",
                                ifelse(region=="west north central div", "Midwest",
                                       ifelse(region == "south atlantic division"| region=="east south central div", "South",
                                              ifelse(region=="west south central div", "West", NA)))),
         labor_force = ifelse(labforce == "yes, in the labor force", "in the labor force",
                           ifelse(labforce == "no, not in the labor force", "not in the labor force", NA)),
         
         income.6 = ifelse(as.integer(ftotinc) < 30000, "$30k>",
                           ifelse(as.integer(ftotinc) < 50000, "$30k-50k",
                                  ifelse(as.integer(ftotinc) < 70000, "$50k-70k",
                                         ifelse(as.integer(ftotinc) < 100000, "$70k-100k",
                                                ifelse(as.integer(ftotinc)> 100000, "$100k<", NA))))),
         race3 = ifelse(as.integer(race) == 1, "White",
                        ifelse(as.integer(race) == 2, "Black", "Other"))
         )



post_stra <- reduced_data %>%
  select(
         race3,
         agegroup,
         gender,
         income.6,
         educ6,
         census_region,
         labor_force
  )



         
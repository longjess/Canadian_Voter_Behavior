library(tidyverse)
library(haven)
library(fastDummies)
library(nnet)
library(lmtest)
library(DescTools)
library(janitor)
library(sjmisc)

setwd("C:/Jessica/Stanford/Winter 2023/STATS 305B/Project")

load("data_cleaned.RData")
load("data_cleaned_2004.RData")
load("data_final.RData")
load("data_final_2004.RData")
load("data_final_full.RData")
load("data_final_full_2004.RData")

# Want to find if data is not representative of overall voting population 
# proportionally 
# Proportions in population taken from census data in 2006 and 2021
# Proportions were calculated for the population of Canada minus Quebec
# Computations were done in Excel 

n_2021 <- nrow(data_cleaned)
n_2004 <- nrow(data_cleaned_2004)

table(data_final$union)/n_2021
table(data_final_2004$union)/n_2004
union_prop_2004 <- 4005.4*1000/23384735 # Number of union workers/eligible voters, in Canada
union_prop_2021 <- 4696.7*1000/28332715
t.test(data_final$union, mu = union_prop_2004)
t.test(data_final_2004$union, mu = union_prop_2021)
# Difference not significant at 0.05

table(data_cleaned$education)/n_2021
table(data_cleaned_2004$education)/n_2004
# Higher education seems to be overrepresented
# Test Bachelor's degree and completed secondary to check
t.test(data_final_full$education_Bachelors_degree, mu = 0.130595)
t.test(data_final_full_2004$education_Bachelors_degree, mu = 0.196026)
# Difference is significant for 2021
t.test(data_final_full$education_Completed_secondary, mu = 0.263338)
t.test(data_final_full_2004$education_Completed_secondary, mu = 0.276679)
# Difference is significant

table(data_cleaned$gender)/n_2021
table(data_cleaned_2004$gender)/n_2004
male_prop_2004 <- 0.482694191 
male_prop_2021 <- 0.486704231
t.test(data_final_full$gender_male, mu = male_prop_2004)
t.test(data_final_full_2004$gender_male, mu = male_prop_2021)
# Difference is significant

table(data_cleaned$age_group)/n_2021
table(data_cleaned_2004$age_group)/n_2004
t.test(data_final_full$age_group_2, mu = 0.738841507)
t.test(data_final_full_2004$age_group_2, mu = 0.692875989)
t.test(data_final_full$age_group_3, mu = 0.171310728)
t.test(data_final_full_2004$age_group_3, mu = 0.228304896)
# Some of the age groups have a significant difference from the population

library(tidyverse)
library(haven)
library(fastDummies)
library(nnet)
library(lmtest)
library(DescTools)
library(cesR)

setwd("C:/Jessica/Stanford/Winter 2023/STATS 305B/Project")

get_ces("ces2004")

data_selected_2004 <- ces2004 %>%
  select(cps_rgen, # Gender
         province,
         cps_s3, # Education level
         cps_s9, # Religion
         cps_s17, # First language
         cps_s6a, # Belonging to a union
         cps_s18, # Total household income
         cps_s2, # Marital status
         cps_s1, # Year of birth
         pes_a3_3) # Which party voted for
colnames(data_selected_2004) <-  c('gender', 'province', 'education', 'religion',
                                   'first_language', 'union','income', 'marital','birth_year', 'vote_choice')
save(data_selected_2004, file="data_selected_2004.RData")

load("data_selected_2004.RData")
# Remove Quebec
# Keep votes for Liberal, Conservative, NDP
data_filtered_2004 <- data_selected_2004 %>%
  filter(province != 24, vote_choice %in% c(1, 2, 3))
# We have 1778 data points left
save(data_filtered_2004, file="data_filtered_2004.RData")

load("data_filtered_2004.RData")
data_cleaned_2004 <- data_filtered_2004 %>%
  mutate(province = case_when(
    province == 48 ~ "Alberta",
    province == 59 ~ "British_Columbia",
    province == 46 ~ "Manitoba",
    province == 13 ~ "New_Brunswick",
    province == 10 ~ "Newfoundland_and_Labrador",
    province == 61 ~ "Northwest Territories",
    province == 12 ~ "Nova_Scotia",
    province == 62 ~ "Nunavut",
    province == 35 ~ "Ontario",
    province == 11 ~ "Prince_Edward_Island",
    province == 47 ~ "Saskatchewan",
    province == 60 ~ "Yukon")) %>%
  # Replace numerical value with label (male/female)
  mutate(gender=as.character(as_factor(gender))) %>%
  mutate(education = case_when(
    education %in% c(1:4) ~ "Less_than_secondary",
    education == 5 ~ "Completed_secondary",
    education == 6 ~ "Some_college",
    education == 7 ~ "Completed_college",
    education == 8 ~ "Some_university",
    education == 9 ~ "Bachelors_degree",
    education == 10 ~ "Masters_degree",
    education == 11 ~ "Professional_degree_or_doctorate",
    !(education %in% c(1:11)) ~ "No_answer")) %>%
  filter(education != "No_answer") %>%
  mutate(religion = case_when(
    religion %in% c(0, 98) ~ "Non_religious", 
    religion %in% c(1, 9, 13, 16) ~ "Mainline_Protestant",
    religion %in% c(2, 12, 14, 18, 19, 20) ~ "Other_Protestant",
    religion == 4 ~ "Catholic",
    religion %in% c(5, 7, 10, 17) ~ "Other_Christian",
    religion %in% c(3, 6, 7, 8, 11, 15, 97) ~ "Other_religion",
    !(religion %in% c(0:20, 97, 98)) ~ "No_answer")) %>%
  mutate(native_anglophone = case_when(
    first_language == 1 ~ 1,
    first_language != 1 ~ 0)) %>% 
  mutate(native_francophone = case_when(
    first_language == 5 ~ 1,
    first_language != 5 ~ 0)) %>%
  select(-first_language) %>%
  mutate(union = case_when(
    union == 1 ~ 1, 
    union != 1 ~ 0)) %>%
  # New income categories to match with 2021
  mutate(income_cat = case_when(
    income %in% c(1, 2) ~ "less_than_30k", 
    income %in% c(3:5) ~ "30_to_60k", 
    income %in% c(6:8) ~ "60_to_90k", 
    income %in% c(9, 10) ~ "more_than_90k",
    !(income %in% c(1:10)) ~ "No_answer")) %>%  
  select(-income) %>% 
  mutate(marital = case_when(
    marital == 1 ~ "Married", 
    marital == 2 ~ "Living_w_partner",
    marital == 3 ~ "Divorced",
    marital == 4 ~ "Seperated",
    marital == 5 ~ "Widowed",
    marital == 6 ~ "Never_married",
    !(marital %in% c(1:6)) ~ "No_answer")) %>%
  mutate(age = 2004 - birth_year) %>%
  select(-birth_year) %>%
  mutate(age_group = case_when(
    age < 25 ~ 1,
    age %in% c(25:64) ~ 2,
    age >= 65 ~ 3)) %>%
  filter(!is.na(age_group)) %>%
  mutate(vote_choice = case_when(
    vote_choice == 1 ~ "Liberal",
    vote_choice == 2 ~ "Conservative",
    vote_choice == 3 ~ "NDP")) %>%
  mutate(year = 2004) %>%
  replace(is.na(.), 0)

save(data_cleaned_2004, file="data_cleaned_2004.RData")  

proportions_pop <- read_csv("population_proportions.csv") %>%
  # Want to deal with completed secondary, some college, some uni separately
  filter(education != "Completed_secondary") 
# Compute the proportions in the data
data_counts <- data_cleaned_2004 %>% group_by(gender, year, age_group) %>%
  summarise(total_count=n(), .groups = 'drop')
proportions_data <- data_cleaned_2004 %>%
  group_by(gender, year, age_group, education) %>%
  summarise(count=n(), .groups = 'drop') %>%
  left_join(data_counts) %>%
  mutate(proportions_data = count/total_count)
# Address the portion of the population that is not accounted for
# Want to assign this portion to Completed_secondary, Some_college, and Some_university
proportions_data_leftover <- proportions_data %>%
  # Calculate the leftover portion that these three make up in the data
  filter(education %in% c("Completed_secondary", "Some_college", "Some_university")) %>%
  group_by(gender, year, age_group) %>%
  mutate(proportions_data_leftover_total=sum(proportions_data)) %>%
  ungroup() %>%
  # Find the ratio between these three relative to the total leftover
  mutate(proportions_data_leftover_ratio = proportions_data/proportions_data_leftover_total)
proportions_data <- proportions_data %>%
  left_join(proportions_pop) %>%
  left_join(proportions_data_leftover) %>% 
  replace(is.na(.), 0) %>%
  # Calculate the leftover portion in the population
  group_by(gender, year, age_group) %>%
  mutate(proportions_total=sum(proportion)) %>%
  ungroup() %>%
  mutate(proportions_leftover=1-proportions_total) %>%
  # Estimate the proportions of Completed_secondary, Some_college, and Some_university
  # in the total population using the ratios in the data 
  mutate(proportion = ifelse(proportion==0,
                             proportions_data_leftover_ratio*proportions_leftover,
                             proportion))
proportions_data_2004 <- proportions_data %>% 
  # Weight is proportion in population/proportion in sample
  mutate(weight = proportion/proportions_data) %>%
  select(gender, year, age_group, education, weight)
save(proportions_data_2004, file="proportions_data_2004.RData")

data_final_2004 <- data_cleaned_2004 %>% 
  left_join(proportions_data_2004) %>%
  select("vote_choice", "union", "education", "religion", "weight", "year") %>%
  # Create dummy variables
  dummy_cols(select_columns = c("education", "religion")) %>%
  # Remove original columns
  select(-c("education", "religion"))

save(data_final_2004, file="data_final_2004.RData") 

# Create dummy variables for all features, for additional analysis
data_final_full_2004 <- data_cleaned_2004 %>%
  dummy_cols(select_columns = c("gender", "education", "religion", "province", "marital",
                                "income_cat", "age_group")) %>%
  select(-c("gender", "education", "religion", "province", "marital",
            "income_cat", "age_group"))

save(data_final_full_2004, file="data_final_full_2004.RData")
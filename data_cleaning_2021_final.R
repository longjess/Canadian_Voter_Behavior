library(tidyverse)
library(haven)
library(fastDummies)
library(nnet)
library(lmtest)
library(DescTools)

setwd("C:/Jessica/Stanford/Winter 2023/STATS 305B/Project")

data_raw <- read_dta('2021 Canadian Election Study v1.0.dta')

data_selected <- data_raw %>%
  select(cps21_genderid,
         cps21_province,
         cps21_education,
         cps21_religion,
         cps21_language_1,
         cps21_language_2,
         cps21_union,
         cps21_income_cat,
         cps21_marital, 
         cps21_yob,
         pes21_votechoice2021)
save(data_selected, file="data_selected.RData")

# Remove Quebec
# Keep votes for Liberal, Conservative, NDP
data_filtered <- data_selected %>%
  filter(cps21_province != 11, pes21_votechoice2021 %in% c(1, 2, 3))
# We have 8255 data points left
save(data_filtered, file="data_filtered.RData")

load("data_filtered.RData")
data_cleaned <- data_filtered %>%
  mutate(province = case_when(
    cps21_province == 1 ~ "Alberta",
    cps21_province == 2 ~ "British_Columbia",
    cps21_province == 3 ~ "Manitoba",
    cps21_province == 4 ~ "New_Brunswick",
    cps21_province == 5 ~ "Newfoundland_and_Labrador",
    cps21_province == 6 ~ "Northwest_Territories",
    cps21_province == 7 ~ "Nova_Scotia",
    cps21_province == 8 ~ "Nunavut",
    cps21_province == 9 ~ "Ontario",
    cps21_province == 10 ~ "Prince_Edward_Island",
    cps21_province == 12 ~ "Saskatchewan",
    cps21_province == 13 ~ "Yukon")) %>%
  select(-cps21_province) %>%
  mutate(gender = case_when(
    cps21_genderid == 1 ~ "male",
    cps21_genderid == 2 ~ "female",
    !(cps21_genderid %in% c(1:2)) ~ "other")) %>%
  filter(gender != "other") %>% 
  select(-cps21_genderid) %>%
  mutate(education = case_when(
    cps21_education %in% c(1:4) ~ "Less_than_secondary",
    cps21_education == 5 ~ "Completed_secondary",
    cps21_education == 6 ~ "Some_college",
    cps21_education == 7 ~ "Completed_college",
    cps21_education == 8 ~ "Some_university",
    cps21_education == 9 ~ "Bachelors_degree",
    cps21_education == 10 ~ "Masters_degree",
    cps21_education == 11 ~ "Professional_degree_or_doctorate",
    !(cps21_education %in% c(1:11)) ~ "No_answer")) %>%
  filter(education != "No_answer") %>% 
  select(-cps21_education) %>%
  mutate(religion = case_when(
    cps21_religion %in% c(1:2) ~ "Non_religious", 
    cps21_religion %in% c(8, 13, 16, 18) ~ "Mainline_Protestant",
    cps21_religion %in% c(9, 15, 17, 19, 20, 21) ~ "Other_Protestant",
    cps21_religion == 10 ~ "Catholic",
    cps21_religion %in% c(11, 12, 14) ~ "Other_Christian",
    cps21_religion %in% c(3, 4, 5, 6, 7, 22) ~ "Other_religion",
    !(cps21_religion %in% c(1:22)) ~ "No_answer")) %>%
  select(-cps21_religion) %>%
  rename(native_anglophone = cps21_language_1) %>%
  mutate(native_anglophone = case_when(
    native_anglophone == 1 ~ 1,
    native_anglophone != 1 ~ 0)) %>% 
  rename(native_francophone = cps21_language_2) %>%
  mutate(native_francophone = case_when(
    native_francophone == 1 ~ 1,
    native_francophone != 1 ~ 0)) %>%
  mutate(union = case_when(
    cps21_union == 1 ~ 1, 
    cps21_union != 1 ~ 0)) %>%
  select(-cps21_union) %>%
  # New income categories to match with 2021
  mutate(income_cat = case_when(
    cps21_income_cat %in% c(1, 2) ~ "less_than_30k", 
    cps21_income_cat == 3 ~ "30_to_60k", 
    cps21_income_cat == 4 ~ "60_to_90k", 
    cps21_income_cat %in% c(5:8) ~ "more_than_90k", 
    !(cps21_income_cat %in% c(1:8)) ~ "No_answer")) %>%   
  select(-cps21_income_cat) %>%
  mutate(marital = case_when(
    cps21_marital == 1 ~ "Married", 
    cps21_marital == 2 ~ "Living_w_partner",
    cps21_marital == 3 ~ "Divorced",
    cps21_marital == 4 ~ "Seperated",
    cps21_marital == 5 ~ "Widowed",
    cps21_marital == 6 ~ "Never_married",
    !(cps21_marital %in% c(1:6)) ~ "No_answer")) %>%
  select(-cps21_marital) %>%
  rename(age = cps21_yob) %>%
  mutate(age_group = case_when(
    age < 25 ~ 1,
    age %in% c(25:64) ~ 2,
    age >= 65 ~ 3)) %>%
  mutate(vote_choice = case_when(
    pes21_votechoice2021 == 1 ~ "Liberal",
    pes21_votechoice2021 == 2 ~ "Conservative",
    pes21_votechoice2021 == 3 ~ "NDP")) %>%
  select(-pes21_votechoice2021) %>%
  mutate(year = 2021) %>%
  replace(is.na(.), 0)

save(data_cleaned, file="data_cleaned.RData")

proportions_pop <- read_csv("population_proportions.csv") %>%
  # Want to deal with completed secondary, some college, some uni separately
  filter(education != "Completed_secondary") 
# Compute the proportions in the data
data_counts <- data_cleaned %>% group_by(gender, year, age_group) %>%
  summarise(total_count=n(), .groups = 'drop')
proportions_data <- data_cleaned %>%
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
proportions_data <- proportions_data %>% 
# Weight is proportion in population/proportion in sample
  mutate(weight = proportion/proportions_data) %>%
  select(gender, year, age_group, education, weight)
save(proportions_data, file="proportions_data.RData")

data_final <- data_cleaned %>% 
  left_join(proportions_data) %>%
  select("vote_choice", "union", "education", "religion", "weight", "year") %>%
  # Create dummy variables
 dummy_cols(select_columns = c("education", "religion")) %>%
  # Remove original columns
  select(-c("education", "religion"))

save(data_final, file="data_final.RData")

# Create dummy variables for all features, for additional analysis
data_final_full <- data_cleaned %>%
  dummy_cols(select_columns = c("gender", "education", "religion", "province", "marital",
                                "income_cat", "age_group")) %>%
  select(-c("gender", "education", "religion", "province", "marital",
            "income_cat", "age_group"))

save(data_final_full, file="data_final_full.RData")


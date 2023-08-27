library(tidyverse)
library(haven)
library(fastDummies)
library(nnet)
library(lmtest)
library(DescTools)
library(janitor)
library(sjmisc)
library(marginaleffects)

setwd("C:/Jessica/Stanford/Winter 2023/STATS 305B/Project")

# Load final data
load("data_final.RData")
load("data_final_2004.RData")

data_final <- data_final %>% rbind(data_final_2004) %>%
  mutate(year = case_when(
    year == 2021 ~ 1, 
    year == 2004 ~ 0)) %>% 
  relocate(union, .after = year)

# Use Liberal as base, as they are the winners in both years
data_final$vote_choice <- relevel(as.factor(data_final$vote_choice), ref = "Liberal")
fit <- multinom(vote_choice ~ . - weight + year:(. - weight),
                data = data_final, weights=weight, maxit=100)
summary(fit)

# Returns dataframe of coefficients, p-values, and significance
get_fit_results <- function(fit){
  coeffs <- as.data.frame(t(summary(fit)$coefficients))
  # Z-score
  z <- t(summary(fit)$coefficients/summary(fit)$standard.errors)
  # p-value
  p <- as.data.frame(1 - pnorm(abs(z), 0, 1)) * 2
  colnames(p) <- c("Conservative_pval", "NDP_pval")
  results <- cbind(coeffs, p) %>%
    mutate(Conservative_sig = (Conservative_pval < 0.05)) %>%
    mutate(NDP_sig = (NDP_pval < 0.05)) 
  return(results)
}
results <- get_fit_results(fit)
view(results)
save(results, file="results.RData")

# Adds single and interaction term to give total impact on log odds
get_2021_effects <- function(i, results, k){
  C <- results$Conservative
  C_odds <- C[i] + C[i+k]
  N <- results$NDP
  N_odds <- N[i] + N[i+k]
  return(c(C_odds, N_odds))
}
# k is the number of rows apart a variable is from its interaction with year
k <- 16
variable_names <- fit$coefnames[c(3:18)]
effects_2021 <- sapply(c(3:18), get_2021_effects, results, k)
effects_2021 <- data_frame(variable=variable_names, 
                           Conservative = effects_2021[1, ], NDP = effects_2021[2, ])
save(effects_2021, file="effects_2021.RData")

# Marginal effects
me <- marginaleffects(fit, type = "probs")
me_summary <- summary(me)
me_summary$sig <- me_summary$p.value < 0.05

# For comparison
fit_unweighted <- multinom(vote_choice ~ . - year - weight + year:(. - year - weight),
                data = data_final, maxit=100)
summary(fit_unweighted)



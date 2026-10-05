---
title: "Quality ANC Coverage Analysis, BDHS 2022"
author: "Rubyath Binte Hasan"
date: "2026-10-04"
---

## Data cleaning and transformation
#load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, haven, survey)

#load raw data, downloaded data in stata format 
raw_anc_data <- read_dta("BDNR81FL.dta")

#inclusion criteria:
#1. Women who had at least one birth in the last 3 years (v238 != 0)
#2. Exclude "Don't know" (98) or "Missing" (99) responses for ANC visits
#3. Exclude "NA"/missing responses for ANC visits

#first checking the response limit 
table(raw_anc_data$m14)
#apply the filters and cap ANC visits at highest 20 visits (m14 <= 20)
anc_data_filtered <- raw_anc_data %>% filter(v238 != 0, m14 <= 20, !is.na(m14))

n_women <- nrow(anc_data_filtered)
cat("Final analytical sample consists of", n_women, "women.\n")

#generate composite quality ANC indicator
anc_data_final <- anc_data_filtered %>%
  mutate(anc_4plus = ifelse(m14 >= 4, "4plus", "Incmplt"),
    anc_provider = ifelse(m2a == 1 | m2b == 1 | m2c == 1 | m2d == 1 | m2e == 1, "skilled_prov", "non_skilled"),
    test_all = ifelse(m42a == 1 & m42c == 1 & m42d == 1 & m42e == 1 & m42m == 1, "tested", "not_tested"),
    quality_anc = ifelse(anc_4plus == "4plus" & anc_provider == "skilled_prov" & test_all == "tested", "Yes", "No"),
    
#convert the variables to factor and update label of the categories
    quality_anc = factor(quality_anc, levels = c("No", "Yes")),
    s115_1 = factor(s115_1, levels = 0:4, labels = c("No education", "Primary incomplete", "Primary complete", "Secondary incomplete", "Secondary and Higher")),
    v102 = factor(v102, levels = 1:2, labels = c("Urban", "Rural")),
    v169a = factor(v169a, levels = 0:1, labels = c("Don't have phone", "Have phone")),
    v190 = factor(v190, levels = 1:5, labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    v024 = factor(v024, levels = 1:8, labels = c("Barishal", "Chattogram", "Dhaka", "Khulna", "Mymensingh", "Rajshahi", "Rangpur", "Sylhet")),

#recode age and lock factor levels to prevent alphabetical sorting
    age_group = case_when(
      v013 == 1 ~ "<20",
      v013 %in% 2:4 ~ "20-34",
      v013 %in% 5:7 ~ "35-49"),
    age_group = factor(age_group, levels = c("<20", "20-34", "35-49")))

#consider the survey design
#create survey weights
   anc_data_final <- anc_data_final %>% mutate(wt = v005 / 1000000)

#save final analytical dataset    
saveRDS(anc_data_final, "anc_data_final.rds")

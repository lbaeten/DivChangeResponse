#=================
# Data preparation
#=================

library(dplyr)

# Read data
dta <- read.csv("data/Vellend_data_original.csv", header=T)
dta_SR <- tbl_df(dta) %>% filter(SR_analysis == 1)

dta2 <- read.csv("data/Vellend_data_updated.csv", header=T)
dta_SR2 <- tbl_df(dta2) %>% filter(SR_analysis == 1)

# Calculate study indicators
n <- nrow(dta_SR)
dta_SR$Study_nr <- rep(NA,n)
study_ID <- unique(dta_SR$Study)
k <- length(study_ID)
for(j in 1:k){
  dta_SR$Study_nr[dta_SR$Study == study_ID[j]] <- j
}

# Calculate study indicators
n <- nrow(dta_SR2)
dta_SR2$Study_nr <- rep(NA,n)
study_ID <- unique(dta_SR2$Study)
k <- length(study_ID)
for(j in 1:k){
  dta_SR2$Study_nr[dta_SR2$Study == study_ID[j]] <- j
}

# Original versus updated data
# dta_SR_orig <- dta_SR %>% filter(Round == 1)
# dta_SR_upd <- dta_SR %>% filter(Round == 2)

dta_SR_orig <- dta_SR
dta_SR_upd <- dta_SR2

# Calculate average duration at study level
dta_duration <- dta_SR %>%
  group_by(Study) %>%
  summarize(mn_duration = mean(Duration)) %>%
  left_join(dta_SR, .)

dta_duration2 <- dta_SR2 %>%
  group_by(Study) %>%
  summarize(mn_duration = mean(Duration)) %>%
  left_join(dta_SR2, .)

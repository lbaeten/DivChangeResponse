#=================
# Data preparation
#=================

library(dplyr)

# Read data
dta <- read.csv("data/sd01.csv", header=T)
dta_SR <- tbl_df(dta) %>% filter(SR_analysis == 1)

# Calculate study indicators
n <- nrow(dta_SR)
dta_SR$Study_nr <- rep(NA,n)
study_ID <- unique(dta_SR$Study)
k <- length(study_ID)
for(j in 1:k){
  dta_SR$Study_nr[dta_SR$Study == study_ID[j]] <- j
}

# Original versus updated data
dta_SR_orig <- dta_SR %>% filter(Round == 1)
dta_SR_upd <- dta_SR %>% filter(Round == 2)

# Calculate average duration at study level
dta_duration <- dta_SR %>%
  group_by(Study) %>%
  summarize(mn_duration = mean(Duration)) %>%
  left_join(dta_SR, .)
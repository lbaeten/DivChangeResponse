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

# Calculate average duration at study level
dta_duration <- dta_SR %>%
  group_by(Study) %>%
  summarize(mn_duration = mean(Year2_min - Year1_min)) %>%
  left_join(dta_SR, .)
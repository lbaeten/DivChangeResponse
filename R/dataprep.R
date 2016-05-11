#=================
# Data preparation
#=================

library(dplyr)

# Read data and filter data
vel_orig <- read.csv("data/Vellend_data_original.csv", header=T) %>%
  tbl_df() %>% 
  filter(SR_analysis == 1)

vel_upd <- read.csv("data/Vellend_data_updated.csv", header=T) %>%
  tbl_df() %>%
  filter(SR_analysis == 1)

# Calculate study indicators
n <- nrow(vel_orig)
vel_orig$Study_nr <- rep(NA,n)
study_ID <- unique(vel_orig$Study)
k <- length(study_ID)
for(j in 1:k){
  vel_orig$Study_nr[vel_orig$Study == study_ID[j]] <- j
}

n <- nrow(vel_upd)
vel_upd$Study_nr <- rep(NA,n)
study_ID <- unique(vel_upd$Study)
k <- length(study_ID)
for(j in 1:k){
  vel_upd$Study_nr[vel_upd$Study == study_ID[j]] <- j
}

rm(list = c("j", "k", "n", "study_ID"))

# Calculate average duration at study level
vel_orig <- vel_orig %>%
  group_by(Study) %>%
  summarize(mn_duration = mean(Duration)) %>%
  left_join(vel_orig, .)

vel_upd <- vel_upd %>%
  group_by(Study) %>%
  summarize(mn_duration = mean(Duration)) %>%
  left_join(vel_upd, .)
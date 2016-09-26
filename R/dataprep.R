#=================
# Data preparation
#=================

library(dplyr)

# Dornelas data
#--------------
dorn_raw <- read.csv("data/Dornelas_by_study.csv") %>%
  filter(modified_study_147 == "no")
  
dorn_modif <-  read.csv("data/Dornelas_by_study.csv") %>%
  filter(modified_study_147 == "yes")

# Vellend data
#-------------
  # Read data and filter data
vel_orig <- read.csv("data/Vellend_data_original.csv", header=T) %>%
  tbl_df() %>% 
  filter(SR_analysis == 1)

vel_upd <- read.csv("data/Vellend_data_updated.csv", header=T) %>%
  tbl_df() %>%
  filter(SR_analysis == 1)

  # "Impact only" data
vel_orig_I <- vel_orig %>%
  filter(Driver != "Cessation_grazing", Driver != "Post-disturbance", Driver != "Post-fire")

vel_upd_I<- vel_upd %>%
  filter(Driver != "Cessation_grazing", Driver != "Post-disturbance", Driver != "Post-fire")

  # Calculate study indicators
studyindicators <- function(infile){
  n <- nrow(infile)
  Study_nr <- rep(NA, n)
  study_ID <- unique(infile$Study)
  k <- length(study_ID)
  for(j in 1:k){
    Study_nr[infile$Study == study_ID[j]] <- j
  }
  return(Study_nr)
}

vel_orig$Study_nr <- studyindicators(vel_orig)
vel_orig_I$Study_nr <- studyindicators(vel_orig_I)
vel_upd$Study_nr <- studyindicators(vel_upd)
vel_upd_I$Study_nr <- studyindicators(vel_upd_I)
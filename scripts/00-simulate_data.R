#### Preamble ####
# Purpose: 
# Author:
# Date:
# Contact:
# License:
# Pre-requisites:


#### Workspace Setup ####
library(tidyverse)


#### Build Simulated Data ####
set.seed(250)


# Simulate
simulated_questionnaire_responses <-
  tibble(
    participant_number =
      rep(c(1:200)),
    
    country = 
      sample(
        x = c("Denmark", "Sweden", "U.S.", "U.K."),
        size = 200,
        replace = TRUE
        ),
    
    question_1 = 
      sample(
        rep(c(1:4)),
        size = 200,
        replace = T
      ),
    
    question_2 = 
      sample(
        rep(c(1:4)),
        size = 200,
        replace = T
      ),
    
    question_3 = 
      sample(
        rep(c(1:4)),
        size = 200,
        replace = T
      )
  )

simulated_questionnaire_responses

# 

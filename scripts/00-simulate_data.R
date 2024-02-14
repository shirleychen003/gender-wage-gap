#### Preamble ####
# Purpose: 
# Author:
# Date:
# Contact:
# License:
# Pre-requisites:


#### Workspace Setup ####
library(tidyverse)
library(dplyr)


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

# Rotate Data
simulated_questionnaire_responses_long <- 
  simulated_questionnaire_responses |>
  pivot_longer(
    cols = c("question_1", "question_2", "question_3"),
    names_to = "question",
    values_to = "response"
  )

# Graph Simulated Data
simulated_questionnaire_responses_long %>% 
  filter(response >= 3) %>% 
  # mutate(response = as.factor(response)) %>% 
  ggplot(aes(x = question, fill = country)) +
  geom_histogram(stat="count", position = "dodge2") +
  labs(x = "Question Response", y = "Number of observations", fill = "Country")



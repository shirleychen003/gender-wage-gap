#### Preamble ####
# Purpose: Clean dataset by removing unnecessary columns and renaming columns
# Author: Shirley Chen
# Date: January 2024
# Contact: sshirleyy.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: none

#### Workspace Setup ####
library(tidyverse)
library(here)
library(janitor)
library(opendatatoronto)
library(ggplot2)
library(dplyr)

# Read in data
raw_gender_data <- read_csv("inputs/data/raw_data.csv")

cleaned_gender_data <- clean_names(raw_gender_data)

# Removing unnecessary columns
first_cleaned_gender_data <-
  subset(cleaned_gender_data, select = c(country, v14))

# Keep countries US(6), UK(4), Denmark(32), Sweden(13)
selected_countries <- c("6", "4", "32", "13")
first_cleaned_gender_data <- first_cleaned_gender_data |>
  filter(country %in% selected_countries)

# Rename numbers to their respective country
first_cleaned_gender_data <- first_cleaned_gender_data |>
  mutate(country = recode(country, "6" = "US", "4" = "UK", "32" = "Denmark", 
                          "13" = "Sweden"))

first_cleaned_gender_data$country <- 
  factor(first_cleaned_gender_data$country, levels = c("US", "UK", "Denmark",
                                                       "Sweden"))

first_cleaned_gender_data |>
  ggplot(mapping = aes(x = v14, fill = country)) +
  geom_bar(position = "dodge2") +
  theme(legend.position = "bottom")





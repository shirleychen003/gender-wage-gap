#### Preamble ####
# Purpose: Basic data integrity checks for quarterly datasets using tidyverse.
# Authors: Shirley Chen, Jessica Im, David James Dimalanta
# Date: 19 February 2024
# Contact: [Insert Contact Email Here]
# License: MIT
# Pre-requisites:
# 00-simulate_data.R
# 01-download_data.R
# 02-clean_data.R

#### Workspace setup ####
library(tidyverse)
library(dplyr)

#### Load Data ####
q1_2002 <- read_csv("inputs/data/analysis_data/cleaned_q1_2002.csv")
q2_2002 <- read_csv("inputs/data/analysis_data/cleaned_q2_2002.csv")
q3_2002 <- read_csv("inputs/data/analysis_data/cleaned_q3_2002.csv")
q4_2002 <- read_csv("inputs/data/analysis_data/cleaned_q4_2002.csv")

#### Define data check function ####
check_data <- function(data) {
  # Check for unique countries
  unique_countries <- data |> pull("country") |> unique()
  cat("Unique countries:", length(unique_countries), "\n")

  # Check for range of 'number_of_reports'
  min_reports <- min(data$number_of_reports, na.rm = TRUE)
  max_reports <- max(data$number_of_reports, na.rm = TRUE)
  cat("Min and Max of number_of_reports:", min_reports, "-", max_reports, "\n")

  # Check for range of 'percentage'
  min_percentage <- min(data$percentage, na.rm = TRUE)
  max_percentage <- max(data$percentage, na.rm = TRUE)
  cat("Min and Max of percentage:", min_percentage, "-", max_percentage, "\n")

  # Check for missing values in 'question_response'
  missing_values <- sum(is.na(data$question_response))
  cat("Missing values in question_response:", missing_values, "\n")
}

#### Apply checks to each dataset ####
cat("### Checks for Q1 2002 Dataset ###\n")
check_data(q1_2002)
cat("\n### Checks for Q2 2002 Dataset ###\n")
check_data(q2_2002)
cat("\n### Checks for Q3 2002 Dataset ###\n")
check_data(q3_2002)
cat("\n### Checks for Q4 2002 Dataset ###\n")
check_data(q4_2002)
#### Preamble ####
# Purpose: Download data from the American Economic Association
# Author: Shirley Chen
# Date: January 2024
# Contact: sshirleyy.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: none

#### Workspace setup ####
library(tidyverse)
library(haven)

#### Acquire data ####
converted_data <- read_dta("inputs/data/ISSP-Gender-Norms-2002.dta")

#### Save data as CSV file ####
write_csv(converted_data, "inputs/data/raw_data.csv")


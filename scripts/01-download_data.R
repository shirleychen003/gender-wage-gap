#### Preamble ####
# Purpose: Download data from the American Economic Association
# Author: Shirley Chen
# Date: January 19 2024
# Contact: sshirleyy.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: none

#### Workspace setup ####
library(tidyverse)
library(haven)

#### Acquire data ####
converted_2002_data <- read_dta("inputs/data/ISSP-Gender-Norms-2002.dta")
converted_2012_data <- read_dta("inputs/data/ISSP-Gender-Norms-2012.dta")

#### Save data as CSV file ####
write_csv(converted_2002_data, "inputs/data/raw_data_2002.csv")
write_csv(converted_2012_data, "inputs/data/raw_data_2012.csv")
#### Preamble ####
# Purpose: 
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

#### Question 1:Should women work after marriage and before kids? ####
# 31 = Chile (CL), 35 = Brazil (BR)
# v14 = Shld women work:after marr.before kids
# v15 = Shld women work:child under school age
# v16 = Shld women work:youngest kid at school
# v17 = Shld women work: when kids left home

# Open data
response_data <-
  read_csv(
    "inputs/data/raw_data.csv",
    show_col_types = FALSE
  )

# Clean data
response_data <-
  clean_names(response_data) |>
  select(v3, country, v14) |>
  drop_na(v14) |>
  filter(country == 31 | country == 35)

## Rename question responses
response_data <-
  response_data |>
  mutate(
    v14 = case_match(
      v14,
      1 ~ "Work Full-Time",
      2 ~ "Work Part-Time",
      3 ~ "Stay at Home",
      6 ~ "Woman Can Choose",
      8 ~ "Can't Choose",
      9 ~ "No Answer"
    )
  )

## Rename countries
response_data <-
  response_data |>
  mutate(
    country = case_match(
      country,
      31 ~ "Chile",
      35 ~ "Brazil"
    )
  )

# Create new dataframes to calculate percentage 

## Create a count of num of reports per country
report_counts_country <- response_data |>
  group_by(country) |>
  summarise(number_of_reports = n())

## Create a count of num of reports per question per country
report_counts_question <- response_data |>
  group_by(country, v14) |>
  summarise(number_of_reports = n())

# Create new dataframes of the percentage
chile_count <- length(which(response_data$country == "Chile"))
brazil_count <- length(which(response_data$country == "Brazil"))

## Create response percentage for Chile
response_data_chile <-
  report_counts_question |>
  filter(country == "Chile") |>
  mutate(percentage = number_of_reports / chile_count)

## Create response percentage for Brazil
response_data_brazil <-
  report_counts_question |>
  filter(country == "Brazil") |>
  mutate(percentage = number_of_reports / brazil_count)

# Bind response data per country together into one dataframe
report_counts_percentage <-
  bind_rows(response_data_chile) |>
  bind_rows(response_data_brazil) 

report_counts_percentage$v14 <-
  factor(report_counts_percentage$v14, levels = c("Work Full-Time", 
                                                  "Work Part-Time", 
                                                  "Stay at Home"))


# Graph Simulated Data
report_counts_percentage |>
  ggplot(aes(x = v14, y = percentage, fill = country)) +
  ggtitle("Should women work after marriage and before kids?") +
  geom_col(position = "dodge") +
  theme(legend.position = "bottom") +
  labs(x = "Question Responses",
       y = "Percentage of Observations", fill = "Country") +
  scale_fill_manual( values = c("#012169", "#DA291C")) +
  scale_y_continuous(labels = scales::percent)





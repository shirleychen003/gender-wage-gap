library(dplyr)
library(tidyverse)
library(knitr)
library(janitor)
library(lubridate)

# 6 = US, 13 = Sweden, 32 = Denmark, 4 = Great Britain
# v14 = Shld women work:after marr.before kids 

response_data <-
  read_csv(
    "inputs/data/raw_data.csv",
    show_col_types = FALSE
  )

response_data <-
  clean_names(response_data) |>
  select(v3, country, v14) |>
  filter(v14 <= 6) |>
  filter(country == 6 | country == 13 | country == 32 | country == 4)
  
head(response_data)
  

response_data <-
  response_data |>
  mutate(
    v14 = case_match(
        v14,
        1 ~ "Work Full-Time",
        2 ~ "Work Part-Time",
        3 ~ "Stay at Home",
        6 ~ "Woman Can Choose"
      )
  )

head(response_data)


response_data <-
  response_data |>
  mutate(
    country = case_match(
      country,
      6 ~ "U.S.",
      13 ~ "Sweden",
      32 ~ "Denmark",
      4 ~ "U.K."
    )
  )

head(response_data)

# Create a count of num of reports per country
report_counts_country <- response_data |>
  group_by(country) |>
  summarise(number_of_reports = n())

# Create a count of num of reports per question per country
report_counts_question <- response_data |>
  group_by(country, v14) |>
  summarise(number_of_reports = n())

response_data_denmark <-
  report_counts_question |>
  filter(country == "Denmark") |>
  mutate(percentage = number_of_reports / 1232 * 100)

response_data_sweden <-
  report_counts_question |>
  filter(country == "Sweden") |>
  mutate(percentage = number_of_reports / 1011 * 100)

response_data_us <-
  report_counts_question |>
  filter(country == "U.S.") |>
  mutate(percentage = number_of_reports / 984 * 100)

response_data_uk <-
  report_counts_question |>
  filter(country == "U.K.") |>
  mutate(percentage = number_of_reports / 1715 * 100)

report_counts_percentage <-
  bind_rows(response_data_denmark) |>
  bind_rows(response_data_sweden) |>
  bind_rows(response_data_us) |>
  bind_rows(response_data_uk)
  
report_counts_percentage


# Graph Simulated Data
report_counts_percentage |>
  ggplot(aes(x = v14, fill = country)) +
  geom_bar(stat="count", position = "dodge2") +
  labs(x = "Question Response", y = "Percentage of observations", fill = "Country")

stat


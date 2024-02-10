library(dplyr)
library(tidyverse)
library(knitr)
library(janitor)
library(lubridate)

#### Should women work after marriage and before kids? ####
# 6 = US, 13 = Sweden, 32 = Denmark, 4 = Great Britain
# v14 = Shld women work:after marr.before kids

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
  filter(country == 6 | country == 13 | country == 32 | country == 4)
  
head(response_data)
  
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
      6 ~ "U.S.",
      13 ~ "Sweden",
      32 ~ "Denmark",
      4 ~ "U.K."
    )
  )

head(response_data)


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

## Create response percentage for Denmark
response_data_denmark <-
  report_counts_question |>
  filter(country == "Denmark") |>
  mutate(percentage = number_of_reports / 1223)

## Create response percentage for Sweden
response_data_sweden <-
  report_counts_question |>
  filter(country == "Sweden") |>
  mutate(percentage = number_of_reports / 1011)

## Create response percentage for U.S.
response_data_us <-
  report_counts_question |>
  filter(country == "U.S.") |>
  mutate(percentage = number_of_reports / 984)

## Create response percentage for U.K.
response_data_uk <-
  report_counts_question |>
  filter(country == "U.K.") |>
  mutate(percentage = number_of_reports / 1715)


# Bind response data per country together into one dataframe
report_counts_percentage <-
  bind_rows(response_data_denmark) |>
  bind_rows(response_data_sweden) |>
  bind_rows(response_data_us) |>
  bind_rows(response_data_uk)


# 
report_counts_percentage$country <- 
  factor(report_counts_percentage$country, levels = c("U.S.", "U.K.", 
                                                      "Denmark", "Sweden"))
report_counts_percentage$v14 <-
  factor(report_counts_percentage$v14, levels = c("Work Full-Time", 
                                                  "Work Part-Time", 
                                                  "Stay at Home"))


# Graph Simulated Data
report_counts_percentage |>
  ggplot(aes(x = v14, y = percentage, fill = country)) +
  geom_col(position = "dodge") +
  theme(legend.position = "bottom") +
  labs(x = "Question Responses",
       y = "Percentage of Observations", fill = "Country") +
  scale_fill_manual( values = c("#26476C", "#5C7439", "#863B3E", "#D6832F")) +
  scale_y_continuous(labels = scales::percent)

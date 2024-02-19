library(dplyr)
library(tidyverse)
library(knitr)
library(janitor)
library(lubridate)

#### Should women work after marriage and before kids? ####
# 6 = US, 13 = Sweden, 32 = Denmark, 4 = Great Britain
# v14 = Shld women work:after marr.before kids

# Open data
response_data_q1 <-
  read_csv(
    "inputs/data/raw_data.csv",
    show_col_types = FALSE
  )

# Clean data
response_data_q1 <-
  clean_names(response_data_q1) |>
  select(country, v14) |>
  drop_na(v14) |>
  filter(country == 6 | country == 13 | country == 32 | country == 4)

head(response_data_q1)

## Rename question responses
response_data_q1 <-
  response_data_q1 |>
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
response_data_q1 <-
  response_data_q1 |>
  mutate(
    country = case_match(
      country,
      6 ~ "U.S.",
      13 ~ "Sweden",
      32 ~ "Denmark",
      4 ~ "U.K."
    )
  )

head(response_data_q1)


# Create new dataframes to calculate percentage

## Create a count of num of reports per country
report_counts_country <- response_data_q1 |>
  group_by(country) |>
  summarise(number_of_reports = n())

## Create a count of num of reports per question per country
report_counts_question <- response_data_q1 |>
  group_by(country, v14) |>
  summarise(number_of_reports = n())


# Create new dataframes of the percentage

## Create response percentage for Denmark
response_data_q1_denmark <-
  report_counts_question |>
  filter(country == "Denmark") |>
  mutate(percentage = number_of_reports / 1223)

## Create response percentage for Sweden
response_data_q1_sweden <-
  report_counts_question |>
  filter(country == "Sweden") |>
  mutate(percentage = number_of_reports / 1011)

## Create response percentage for U.S.
response_data_q1_us <-
  report_counts_question |>
  filter(country == "U.S.") |>
  mutate(percentage = number_of_reports / 984)

## Create response percentage for U.K.
response_data_q1_uk <-
  report_counts_question |>
  filter(country == "U.K.") |>
  mutate(percentage = number_of_reports / 1715)


# Bind response data per country together into one dataframe
report_counts_percentage <-
  bind_rows(response_data_q1_denmark) |>
  bind_rows(response_data_q1_sweden) |>
  bind_rows(response_data_q1_us) |>
  bind_rows(response_data_q1_uk)


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
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))



#### Should women work with children under school age? ####
# 6 = US, 13 = Sweden, 32 = Denmark, 4 = Great Britain
# v15 = Shld women work:child under school age

# Open data
response_data_q2 <-
  read_csv(
    "inputs/data/raw_data.csv",
    show_col_types = FALSE
  )

# Clean data
response_data_q2 <-
  clean_names(response_data_q2) |>
  select(country, v15) |>
  drop_na(v15) |>
  filter(country == 6 | country == 13 | country == 32 | country == 4)

head(response_data_q2)

## Rename question responses
response_data_q2 <-
  response_data_q2 |>
  mutate(
    v15 = case_match(
      v15,
      1 ~ "Work Full-Time",
      2 ~ "Work Part-Time",
      3 ~ "Stay at Home",
      6 ~ "Woman Can Choose",
      8 ~ "Can't Choose",
      9 ~ "No Answer"
    )
  )

## Rename countries
response_data_q2 <-
  response_data_q2 |>
  mutate(
    country = case_match(
      country,
      6 ~ "U.S.",
      13 ~ "Sweden",
      32 ~ "Denmark",
      4 ~ "U.K."
    )
  )

head(response_data_q2)


# Create new dataframes to calculate percentage 

## Create a count of num of reports per country
report_counts_country_q2 <- response_data_q2 |>
  group_by(country) |>
  summarise(number_of_reports = n())

## Create a count of num of reports per question per country
report_counts_question_q2 <- response_data_q2 |>
  group_by(country, v15) |>
  summarise(number_of_reports = n())


# Create new dataframes of the percentage

## Create response percentage for Denmark
response_data_q2_denmark <-
  report_counts_question_q2 |>
  filter(country == "Denmark") |>
  mutate(percentage = number_of_reports / 1235)

## Create response percentage for Sweden
response_data_q2_sweden <-
  report_counts_question_q2 |>
  filter(country == "Sweden") |>
  mutate(percentage = number_of_reports / 975)

## Create response percentage for U.S.
response_data_q2_us <-
  report_counts_question_q2 |>
  filter(country == "U.S.") |>
  mutate(percentage = number_of_reports / 1014)

## Create response percentage for U.K.
response_data_q2_uk <-
  report_counts_question_q2 |>
  filter(country == "U.K.") |>
  mutate(percentage = number_of_reports / 1683)


# Bind response data per country together into one dataframe
report_counts_percentage_q2 <-
  bind_rows(response_data_q2_denmark) |>
  bind_rows(response_data_q2_sweden) |>
  bind_rows(response_data_q2_us) |>
  bind_rows(response_data_q2_uk)


# 
report_counts_percentage_q2$country <- 
  factor(report_counts_percentage_q2$country, levels = c("U.S.", "U.K.", 
                                                      "Denmark", "Sweden"))
report_counts_percentage_q2$v15 <-
  factor(report_counts_percentage_q2$v15, levels = c("Work Full-Time", 
                                                  "Work Part-Time", 
                                                  "Stay at Home"))


# Graph Simulated Data
report_counts_percentage_q2 |>
  ggplot(aes(x = v15, y = percentage, fill = country)) +
  geom_col(position = "dodge") +
  theme(legend.position = "bottom") +
  labs(x = "Question Responses",
       y = "Percentage of Observations", fill = "Country") +
  scale_fill_manual( values = c("#26476C", "#5C7439", "#863B3E", "#D6832F")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))





#### Should women work with youngest kid in school? ####
# 6 = US, 13 = Sweden, 32 = Denmark, 4 = Great Britain
# v15 = Shld women work:youngest kid at school

# Open data
response_data_q3 <-
  read_csv(
    "inputs/data/raw_data.csv",
    show_col_types = FALSE
  )

# Clean data
response_data_q3 <-
  clean_names(response_data_q3) |>
  select(country, v16) |>
  drop_na(v16) |>
  filter(country == 6 | country == 13 | country == 32 | country == 4)

head(response_data_q3)

## Rename question responses
response_data_q3 <-
  response_data_q3 |>
  mutate(
    v16 = case_match(
      v16,
      1 ~ "Work Full-Time",
      2 ~ "Work Part-Time",
      3 ~ "Stay at Home",
      6 ~ "Woman Can Choose",
      8 ~ "Can't Choose",
      9 ~ "No Answer"
    )
  )

## Rename countries
response_data_q3 <-
  response_data_q3 |>
  mutate(
    country = case_match(
      country,
      6 ~ "U.S.",
      13 ~ "Sweden",
      32 ~ "Denmark",
      4 ~ "U.K."
    )
  )

head(response_data_q3)


# Create new dataframes to calculate percentage 

## Create a count of num of reports per country
report_counts_country_q3 <- response_data_q3 |>
  group_by(country) |>
  summarise(number_of_reports = n())

## Create a count of num of reports per question per country
report_counts_question_q3 <- response_data_q3 |>
  group_by(country, v16) |>
  summarise(number_of_reports = n())


# Create new dataframes of the percentage

## Create response percentage for Denmark
response_data_q3_denmark <-
  report_counts_question_q3 |>
  filter(country == "Denmark") |>
  mutate(percentage = number_of_reports / 1230)

## Create response percentage for Sweden
response_data_q3_sweden <-
  report_counts_question_q3 |>
  filter(country == "Sweden") |>
  mutate(percentage = number_of_reports / 973)

## Create response percentage for U.S.
response_data_q3_us <-
  report_counts_question_q3 |>
  filter(country == "U.S.") |>
  mutate(percentage = number_of_reports / 1014)

## Create response percentage for U.K.
response_data_q3_uk <-
  report_counts_question_q3 |>
  filter(country == "U.K.") |>
  mutate(percentage = number_of_reports / 1693)


# Bind response data per country together into one dataframe
report_counts_percentage_q3 <-
  bind_rows(response_data_q3_denmark) |>
  bind_rows(response_data_q3_sweden) |>
  bind_rows(response_data_q3_us) |>
  bind_rows(response_data_q3_uk)


# 
report_counts_percentage_q3$country <- 
  factor(report_counts_percentage_q3$country, levels = c("U.S.", "U.K.", 
                                                         "Denmark", "Sweden"))
report_counts_percentage_q3$v16 <-
  factor(report_counts_percentage_q3$v16, levels = c("Work Full-Time", 
                                                     "Work Part-Time", 
                                                     "Stay at Home"))


# Graph Simulated Data
report_counts_percentage_q3 |>
  ggplot(aes(x = v16, y = percentage, fill = country)) +
  geom_col(position = "dodge") +
  theme(legend.position = "bottom") +
  labs(x = "Question Responses",
       y = "Percentage of Observations", fill = "Country") +
  scale_fill_manual( values = c("#26476C", "#5C7439", "#863B3E", "#D6832F")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))




#### Should women work when kids have left home? ####
# 6 = US, 13 = Sweden, 32 = Denmark, 4 = Great Britain
# v15 = Shld women work: when kids left home

# Open data
response_data_q4 <-
  read_csv(
    "inputs/data/raw_data.csv",
    show_col_types = FALSE
  )

# Clean data
response_data_q4 <-
  clean_names(response_data_q4) |>
  select(country, v17) |>
  drop_na(v17) |>
  filter(country == 6 | country == 13 | country == 32 | country == 4)

head(response_data_q4)

## Rename question responses
response_data_q4 <-
  response_data_q4 |>
  mutate(
    v17 = case_match(
      v17,
      1 ~ "Work Full-Time",
      2 ~ "Work Part-Time",
      3 ~ "Stay at Home",
      6 ~ "Woman Can Choose",
      8 ~ "Can't Choose",
      9 ~ "No Answer"
    )
  )

## Rename countries
response_data_q4 <-
  response_data_q4 |>
  mutate(
    country = case_match(
      country,
      6 ~ "U.S.",
      13 ~ "Sweden",
      32 ~ "Denmark",
      4 ~ "U.K."
    )
  )

head(response_data_q4)


# Create new dataframes to calculate percentage 

## Create a count of num of reports per country
report_counts_country_q4 <- response_data_q4 |>
  group_by(country) |>
  summarise(number_of_reports = n())

## Create a count of num of reports per question per country
report_counts_question_q4 <- response_data_q4 |>
  group_by(country, v17) |>
  summarise(number_of_reports = n())


# Create new dataframes of the percentage

## Create response percentage for Denmark
response_data_q4_denmark <-
  report_counts_question_q4 |>
  filter(country == "Denmark") |>
  mutate(percentage = number_of_reports / 1204)

## Create response percentage for Sweden
response_data_q4_sweden <-
  report_counts_question_q4 |>
  filter(country == "Sweden") |>
  mutate(percentage = number_of_reports / 989)

## Create response percentage for U.S.
response_data_q4_us <-
  report_counts_question_q4 |>
  filter(country == "U.S.") |>
  mutate(percentage = number_of_reports / 966)

## Create response percentage for U.K.
response_data_q4_uk <-
  report_counts_question_q4 |>
  filter(country == "U.K.") |>
  mutate(percentage = number_of_reports / 1617)


# Bind response data per country together into one dataframe
report_counts_percentage_q4 <-
  bind_rows(response_data_q4_denmark) |>
  bind_rows(response_data_q4_sweden) |>
  bind_rows(response_data_q4_us) |>
  bind_rows(response_data_q4_uk)


# 
report_counts_percentage_q4$country <- 
  factor(report_counts_percentage_q4$country, levels = c("U.S.", "U.K.", 
                                                         "Denmark", "Sweden"))
report_counts_percentage_q4$v17 <-
  factor(report_counts_percentage_q4$v17, levels = c("Work Full-Time", 
                                                     "Work Part-Time", 
                                                     "Stay at Home"))


# Graph Simulated Data
report_counts_percentage_q4 |>
  ggplot(aes(x = v17, y = percentage, fill = country)) +
  geom_col(position = "dodge") +
  theme(legend.position = "bottom") +
  labs(x = "Question Responses",
       y = "Percentage of Observations", fill = "Country") +
  scale_fill_manual( values = c("#26476C", "#5C7439", "#863B3E", "#D6832F")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))


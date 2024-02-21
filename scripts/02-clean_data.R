#### Preamble ####
# Purpose: Clean downloaded data using various R packages
# Authors: Shirley Chen, Jessica Im, David James Dimalanta
# Date: 19 February 2024
# Contact: david.dimalanta@mail.utoronto.ca
# License: MIT
# Pre-requisites:
# 00-simulate_data.R
# 01-download_data.R

# Workspace Setup
library(dplyr)
library(tidyverse)
library(knitr)
library(janitor)
library(lubridate)
library(readr)

#### Should women work after marriage and before kids? ####
# 6 = US, 32 = Denmark, 31 = Chile, 35 = Brazil, 21 = Philippines, 24 = Japan
# v14 = Shld women work:after marr.before kids

# Open data
response_data_q1 <-
  read_csv(
    "inputs/data/raw_data_2002.csv",
    show_col_types = FALSE
  )

# Clean data
response_data_q1 <-
  clean_names(response_data_q1) |>
  rename(question_response = v14) |>
  select(country, question_response) |>
  drop_na(question_response) |>
  filter(country == 6 | country == 31 | country == 32 | country == 35 |
           country == 21 | country == 24)


## Rename question responses
response_data_q1 <-
  response_data_q1 |>
  mutate(
    question_response = case_match(
      question_response,
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
      31 ~ "Chile",
      32 ~ "Denmark",
      35 ~ "Brazil",
      21 ~ "Philippines",
      24 ~ "Japan"
    )
  )



# Create new dataframes to calculate percentage


## Create a count of num of reports per question per country
report_counts_question <- response_data_q1 |>
  group_by(country, question_response) |>
  summarise(number_of_reports = n())



# Create a count of total responses for each country


denmark_count <- length(which(response_data_q1$country == "Denmark"))
us_count <- length(which(response_data_q1$country == "U.S."))
chile_count <- length(which(response_data_q1$country == "Chile"))
brazil_count <- length(which(response_data_q1$country == "Brazil"))
philippines_count <- length(which(response_data_q1$country == "Philippines"))
japan_count <- length(which(response_data_q1$country == "Japan"))



# Create new dataframes of the percentage


## Create response percentage for Denmark
response_data_q1_denmark <-
  report_counts_question |>
  filter(country == "Denmark") |>
  mutate(percentage = number_of_reports / denmark_count)

## Create response percentage for U.S.
response_data_q1_us <-
  report_counts_question |>
  filter(country == "U.S.") |>
  mutate(percentage = number_of_reports / us_count)

## Create response percentage for Chile
response_data_q1_chile <-
  report_counts_question |>
  filter(country == "Chile") |>
  mutate(percentage = number_of_reports / chile_count)

## Create response percentage for Brazil
response_data_q1_brazil <-
  report_counts_question |>
  filter(country == "Brazil") |>
  mutate(percentage = number_of_reports / brazil_count)

## Create response percentage for Philippines
response_data_q1_philippines <-
  report_counts_question |>
  filter(country == "Philippines") |>
  mutate(percentage = number_of_reports / philippines_count)

## Create response percentage for Japan
response_data_q1_japan <-
  report_counts_question |>
  filter(country == "Japan") |>
  mutate(percentage = number_of_reports / japan_count)


# Bind response data per country together into one dataframe
cleaned_q1_2002 <-
  bind_rows(response_data_q1_denmark) |>
  bind_rows(response_data_q1_us) |>
  bind_rows(response_data_q1_chile) |>
  bind_rows(response_data_q1_brazil) |>
  bind_rows(response_data_q1_philippines) |>
  bind_rows(response_data_q1_japan)

write_csv(cleaned_q1_2002, "outputs/data/cleaned_q1_2002.csv")




#### Should women work with children under school age? ####
# 6 = US, 32 = Denmark, 31 = Chile, 35 = Brazil, 21 = Philippines, 24 = Japan
# v15 = Shld women work:child under school age

# Open data
response_data_q2 <-
  read_csv(
    "inputs/data/raw_data_2002.csv",
    show_col_types = FALSE
  )

# Clean data
response_data_q2 <-
  clean_names(response_data_q2) |>
  rename(question_response = v15) |>
  select(country, question_response) |>
  drop_na(question_response) |>
  filter(country == 6 | country == 31 | country == 32 | country == 35 |
           country == 21 | country == 24)


## Rename question responses
response_data_q2 <-
  response_data_q2 |>
  mutate(
    question_response = case_match(
      question_response,
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
      31 ~ "Chile",
      32 ~ "Denmark",
      35 ~ "Brazil",
      21 ~ "Philippines",
      24 ~ "Japan"
    )
  )



# Create new dataframes to calculate percentage 

## Create a count of num of reports per question per country
report_counts_question_q2 <-
  response_data_q2 |>
  group_by(country, question_response) |>
  summarise(number_of_reports = n())



# Create a count of total responses for each country
denmark_count_q2 <- length(which(response_data_q2$country == "Denmark"))
us_count_q2 <- length(which(response_data_q2$country == "U.S."))
chile_count_q2 <- length(which(response_data_q2$country == "Chile"))
brazil_count_q2 <- length(which(response_data_q2$country == "Brazil"))
philippines_count_q2 <- length(which(response_data_q2$country == "Philippines"))
japan_count_q2 <- length(which(response_data_q2$country == "Japan"))



# Create new dataframes of the percentage


## Create response percentage for Denmark
response_data_q2_denmark <-
  report_counts_question_q2 |>
  filter(country == "Denmark") |>
  mutate(percentage = number_of_reports / denmark_count_q2)

## Create response percentage for U.S.
response_data_q2_us <-
  report_counts_question_q2 |>
  filter(country == "U.S.") |>
  mutate(percentage = number_of_reports / us_count_q2)

## Create response percentage for Chile
response_data_q2_chile <-
  report_counts_question_q2 |>
  filter(country == "Chile") |>
  mutate(percentage = number_of_reports / chile_count_q2)

## Create response percentage for Brazil
response_data_q2_brazil <-
  report_counts_question_q2 |>
  filter(country == "Brazil") |>
  mutate(percentage = number_of_reports / brazil_count_q2)

## Create response percentage for Philippines
response_data_q2_philippines <-
  report_counts_question_q2 |>
  filter(country == "Philippines") |>
  mutate(percentage = number_of_reports / philippines_count_q2)

## Create response percentage for Japan
response_data_q2_japan <-
  report_counts_question_q2 |>
  filter(country == "Japan") |>
  mutate(percentage = number_of_reports / japan_count_q2)


# Bind response data per country together into one dataframe
cleaned_q2_2002 <-
  bind_rows(response_data_q2_denmark) |>
  bind_rows(response_data_q2_us) |>
  bind_rows(response_data_q2_chile) |>
  bind_rows(response_data_q2_brazil) |>
  bind_rows(response_data_q2_philippines) |>
  bind_rows(response_data_q2_japan)

write_csv(cleaned_q2_2002, "outputs/data/cleaned_q2_2002.csv")


#### Should women work with youngest kid in school? ####
# 6 = US, 13 = Sweden, 32 = Denmark, 4 = Great Britain
# v15 = Shld women work:youngest kid at school

# Open data
response_data_q3 <-
  read_csv(
    "inputs/data/raw_data_2002.csv",
    show_col_types = FALSE
  )

# Clean data
response_data_q3 <-
  clean_names(response_data_q3) |>
  rename(question_response = v16) |>
  select(country, question_response) |>
  drop_na(question_response) |>
  filter(country == 6 | country == 31 | country == 32 | country == 35 | 
           country == 21 | country == 24)

## Rename question responses
response_data_q3 <-
  response_data_q3 |>
  mutate(
    question_response = case_match(
      question_response,
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
      31 ~ "Chile",
      32 ~ "Denmark",
      35 ~ "Brazil",
      21 ~ "Philippines",
      24 ~ "Japan"
    )
  )



# Create new dataframes to calculate percentage 

## Create a count of num of reports per question per country
report_counts_question_q3 <- response_data_q3 |>
  group_by(country, question_response) |>
  summarise(number_of_reports = n())



# Create a count of total responses for each country
denmark_count_q3 <- length(which(response_data_q3$country == "Denmark"))
us_count_q3 <- length(which(response_data_q3$country == "U.S."))
chile_count_q3 <- length(which(response_data_q3$country == "Chile"))
brazil_count_q3 <- length(which(response_data_q3$country == "Brazil"))
philippines_count_q3 <- length(which(response_data_q3$country == "Philippines"))
japan_count_q3 <- length(which(response_data_q3$country == "Japan"))



# Create new dataframes of the percentage


## Create response percentage for Denmark
response_data_q3_denmark <-
  report_counts_question_q3 |>
  filter(country == "Denmark") |>
  mutate(percentage = number_of_reports / denmark_count_q3)

## Create response percentage for U.S.
response_data_q3_us <-
  report_counts_question_q3 |>
  filter(country == "U.S.") |>
  mutate(percentage = number_of_reports / us_count_q3)

## Create response percentage for Chile
response_data_q3_chile <-
  report_counts_question_q3 |>
  filter(country == "Chile") |>
  mutate(percentage = number_of_reports / chile_count_q3)

## Create response percentage for Brazil
response_data_q3_brazil <-
  report_counts_question_q3 |>
  filter(country == "Brazil") |>
  mutate(percentage = number_of_reports / brazil_count_q3)

## Create response percentage for Philippines
response_data_q3_philippines <-
  report_counts_question_q3 |>
  filter(country == "Philippines") |>
  mutate(percentage = number_of_reports / philippines_count_q3)

## Create response percentage for Japan
response_data_q3_japan <-
  report_counts_question_q3 |>
  filter(country == "Japan") |>
  mutate(percentage = number_of_reports / japan_count_q3)


# Bind response data per country together into one dataframe
cleaned_q3_2002 <-
  bind_rows(response_data_q3_denmark) |>
  bind_rows(response_data_q3_us) |>
  bind_rows(response_data_q3_chile) |>
  bind_rows(response_data_q3_brazil) |>
  bind_rows(response_data_q3_philippines) |>
  bind_rows(response_data_q3_japan)

write_csv(cleaned_q3_2002, "outputs/data/cleaned_q3_2002.csv")

#### Should women work when kids have left home? ####
# 6 = US, 13 = Sweden, 32 = Denmark, 4 = Great Britain
# v15 = Shld women work: when kids left home

# Open data
response_data_q4 <-
  read_csv(
    "inputs/data/raw_data_2002.csv",
    show_col_types = FALSE
  )

# Clean data
response_data_q4 <-
  clean_names(response_data_q4) |>
  rename(question_response = v17) |>
  select(country, question_response) |>
  drop_na(question_response) |>
  filter(country == 6 | country == 31 | country == 32 | country == 35 | 
           country == 21 | country == 24)


## Rename question responses
response_data_q4 <-
  response_data_q4 |>
  mutate(
    question_response = case_match(
      question_response,
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
      31 ~ "Chile",
      32 ~ "Denmark",
      35 ~ "Brazil",
      21 ~ "Philippines",
      24 ~ "Japan"
    )
  )



# Create new dataframes to calculate percentage 


## Create a count of num of reports per question per country
report_counts_question_q4 <- response_data_q4 |>
  group_by(country, question_response) |>
  summarise(number_of_reports = n())




# Create a count of total responses for each country
denmark_count_q4 <- length(which(response_data_q4$country == "Denmark"))
us_count_q4 <- length(which(response_data_q4$country == "U.S."))
chile_count_q4 <- length(which(response_data_q4$country == "Chile"))
brazil_count_q4 <- length(which(response_data_q4$country == "Brazil"))
philippines_count_q4 <- length(which(response_data_q4$country == "Philippines"))
japan_count_q4 <- length(which(response_data_q4$country == "Japan"))



# Create new dataframes of the percentage


## Create response percentage for Denmark
response_data_q4_denmark <-
  report_counts_question_q4 |>
  filter(country == "Denmark") |>
  mutate(percentage = number_of_reports / denmark_count_q4)

## Create response percentage for U.S.
response_data_q4_us <-
  report_counts_question_q4 |>
  filter(country == "U.S.") |>
  mutate(percentage = number_of_reports / us_count_q4)

## Create response percentage for Chile
response_data_q4_chile <-
  report_counts_question_q4 |>
  filter(country == "Chile") |>
  mutate(percentage = number_of_reports / chile_count_q4)

## Create response percentage for Brazil
response_data_q4_brazil <-
  report_counts_question_q4 |>
  filter(country == "Brazil") |>
  mutate(percentage = number_of_reports / brazil_count_q4)

## Create response percentage for Philippines
response_data_q4_philippines <-
  report_counts_question_q4 |>
  filter(country == "Philippines") |>
  mutate(percentage = number_of_reports / philippines_count_q4)

## Create response percentage for Japan
response_data_q4_japan <-
  report_counts_question_q4 |>
  filter(country == "Japan") |>
  mutate(percentage = number_of_reports / japan_count_q4)


# Bind response data per country together into one dataframe
cleaned_q4_2002 <-
  bind_rows(response_data_q4_denmark) |>
  bind_rows(response_data_q4_us) |>
  bind_rows(response_data_q4_chile) |>
  bind_rows(response_data_q4_brazil) |>
  bind_rows(response_data_q4_philippines) |>
  bind_rows(response_data_q4_japan)

write_csv(cleaned_q4_2002, "outputs/data/cleaned_q4_2002.csv")




#### 2012 Data - Should women work child under school age ####
# Open data

# Philippines = 608
# Chile = 152

response_data_2012 <-
  read_csv(
    "inputs/data/raw_data_2012.csv",
    show_col_types = FALSE
  )

# Clean data
response_data_2012_q2 <-
  clean_names(response_data_2012) |>
  rename(country = v4) |>
  rename(question_response = v12) |>
  select(country, question_response) |>
  drop_na(question_response) |>
  filter(country == 608 | country == 152)

## Rename question responses
response_data_2012_q2 <-
  response_data_2012_q2 |>
  mutate(
    question_response = case_match(
      question_response,
      1 ~ "Work Full-Time",
      2 ~ "Work Part-Time",
      3 ~ "Stay at Home",
      6 ~ "Woman Can Choose",
      8 ~ "Can't Choose",
      9 ~ "No Answer"
    )
  )

## Rename countries
response_data_2012_q2 <-
  response_data_2012_q2 |>
  mutate(
    country = case_match(
      country,
      152 ~ "Chile",
      608 ~ "Philippines"
    )
  )

# Create new dataframes to calculate percentage


## Create a count of num of reports per question per country
report_counts_question_2012_q2 <- response_data_2012_q2 |>
  group_by(country, question_response) |>
  summarise(number_of_reports = n())



# Create a count of total responses for each country
chile_count_2012_q2 <- length(which(response_data_2012_q2$country == "Chile"))
philippines_count_2012_q2 <- length(which(response_data_2012_q2$country == "Philippines"))



# Create new dataframes of the percentage


## Create response percentage for Chile
response_data_2012_q2_chile <-
  report_counts_question_2012_q2 |>
  filter(country == "Chile") |>
  mutate(percentage = number_of_reports / chile_count_2012_q2)

## Create response percentage for Philippines
response_data_2012_q2_philippines <-
  report_counts_question_2012_q2 |>
  filter(country == "Philippines") |>
  mutate(percentage = number_of_reports / philippines_count_2012_q2)



# Bind response data per country together into one dataframe
report_counts_percentage_2012_q2 <-
  bind_rows(response_data_2012_q2_chile) |>
  bind_rows(response_data_2012_q2_philippines)

report_counts_percentage_2012_q2

# Create year variable
response_data_2012_q2_chile <-
  response_data_2012_q2_chile |>
  mutate(year_collected = "2012")

response_data_2012_q2_philippines <-
  response_data_2012_q2_philippines |>
  mutate(year_collected = "2012")

response_data_q2_chile <-
  response_data_q2_chile |>
  mutate(year_collected = "2002")

response_data_q2_philippines <-
  response_data_q2_philippines |>
  mutate(year_collected = "2002")


# Bind response data for school age children from 2012 and 2002
comparison_q2 <-
  bind_rows(response_data_2012_q2_chile) |>
  bind_rows(response_data_2012_q2_philippines) |>
  bind_rows(response_data_q2_chile) |>
  bind_rows(response_data_q2_philippines)


write_csv(comparison_q2, "outputs/data/comparison_q2.csv")


#### 2012 Data - Should women work youngest child in schl ####
# Open data
# Philippines=608
# Chile=152

response_data_2012 <-
  read_csv(
    "inputs/data/raw_data_2012.csv",
    show_col_types = FALSE
  )

# Clean data
response_data_2012_q3 <-
  clean_names(response_data_2012) |>
  rename(country = v4) |>
  rename(question_response = v13) |>
  select(country, question_response) |>
  drop_na(question_response) |>
  filter(country == 608 | country == 152)

## Rename question responses
response_data_2012_q3 <-
  response_data_2012_q3 |>
  mutate(
    question_response = case_match(
      question_response,
      1 ~ "Work Full-Time",
      2 ~ "Work Part-Time",
      3 ~ "Stay at Home",
      6 ~ "Woman Can Choose",
      8 ~ "Can't Choose",
      9 ~ "No Answer"
    )
  )

## Rename countries
response_data_2012_q3 <-
  response_data_2012_q3 |>
  mutate(
    country = case_match(
      country,
      152 ~ "Chile",
      608 ~ "Philippines"
    )
  )

# Create new dataframes to calculate percentage


## Create a count of num of reports per question per country
report_counts_question_2012_q3 <- response_data_2012_q3 |>
  group_by(country, question_response) |>
  summarise(number_of_reports = n())



# Create a count of total responses for each country
chile_count_2012_q3 <- length(which(response_data_2012_q3$country == "Chile"))
philippines_count_2012_q3 <- length(which(response_data_2012_q3$country == "Philippines"))



# Create new dataframes of the percentage


## Create response percentage for Chile
response_data_2012_q3_chile <-
  report_counts_question_2012_q3 |>
  filter(country == "Chile") |>
  mutate(percentage = number_of_reports / chile_count_2012_q3)

## Create response percentage for Philippines
response_data_2012_q3_philippines <-
  report_counts_question_2012_q3 |>
  filter(country == "Philippines") |>
  mutate(percentage = number_of_reports / philippines_count_2012_q3)



# Bind response data per country together into one dataframe
report_counts_percentage_2012_q3 <-
  bind_rows(response_data_2012_q3_chile) |>
  bind_rows(response_data_2012_q3_philippines)

report_counts_percentage_2012_q3

# Create year variable
response_data_2012_q3_chile <-
  response_data_2012_q3_chile |>
  mutate(year_collected = "2012")

response_data_2012_q3_philippines <-
  response_data_2012_q3_philippines |>
  mutate(year_collected = "2012")

response_data_q3_chile <-
  response_data_q3_chile |>
  mutate(year_collected = "2002")

response_data_q3_philippines <-
  response_data_q3_philippines |>
  mutate(year_collected = "2002")


# Bind response data for school age children from 2012 and 2002
comparison_q3 <-
  bind_rows(response_data_2012_q3_chile) |>
  bind_rows(response_data_2012_q3_philippines)|>
  bind_rows(response_data_q3_chile) |>
  bind_rows(response_data_q3_philippines)

write_csv(comparison_q3, "outputs/data/comparison_q3.csv")
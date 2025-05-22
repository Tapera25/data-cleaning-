# Author: Oscar
# Date: 2025-05-22
# Description: Tutorial on data cleaning 
# Github Link: https://github.com/your_repo

#

# libraries ---------------------------------------------------------------

library(tidyverse)

# data --------------------------------------------------------------------
# Create a sample data frame
raw_data <- data_frame(
  id = 1:15,
  name = c("Alice", "Bob", "Charlie", "Dana", NA, "Eli", "Frank", "Grace", "Henry", "Ivy", "Jack", "Kara", "Leo", "Mona", "N/A"),
  age = c(25, 34, NA, 45, 52, "Thirty", 29, 37, NA, 22, 43, 31, 28, "unknown", 30),
  gender = c("F", "M", "M", "F", "F", "M", "Male", "Female", "F", "F", "M", "F", "M", NA, "M"),
  income = c(50000, 60000, 55000, 58000, 62000, 48000, NA, 57000, 59000, 51000, "unknown", 63000, 54000, 56000, 52000),
  join_date = c("2020-01-15", "2019-07-30", "2018/06/22", "15-08-2017", NA, "2021-10-10", "2020-05-05", "unknown", "2017-03-18", "2019-12-01", "2022-01-01", "2016-11-11", "2018-08-08", "2023-02-02", "2017-07-07"),
  department = c("HR", "Finance", "HR", "IT", "HR", "finance", "NA", "IT", "Operations", "HR", "HR", "finance", "IT", "HR", "Operations")
)

#data view
glimpse(raw_data)

#2 cleaning age column
cleaned_df <- raw_data %>% 
  mutate(age= case_when(age=="Thirty"~30,
                        TRUE~parse_number(age)))

#cleaning gender column
cleaned_df <- cleaned_df %>% 
  mutate(gender= case_when(gender=="F"~"Female",
                           gender=="M"~"Male",
                           TRUE~gender))

#cleaning date format column
cleaned_df <- cleaned_df %>% 
  mutate(join_date=case_when(join_date=="unknown"~NA_character_,
                             TRUE~as.character(join_date))) %>% 
  mutate(join_date=parse_date_time(join_date,orders=c("ymd","dmy","mdy")))

#cleaning income column
cleaned_df <- cleaned_df %>%  
  mutate(income=case_when(income=="unknown"~NA_real_,
                          TRUE~parse_number(income)))

#filtering data
cleaned_df %>% 
  filter(gender=="Female",age>30, income>60000,department=="HR")
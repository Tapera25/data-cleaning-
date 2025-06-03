# Author: Oscar 
# Date: 2025-06-03
# Description: Data cleaning & basic stats
# Github Link: https://github.com/your_repo# Load tidyverse

library(tidyverse)

# Simulate raw data
#set.seed(123)
raw_data <- tibble(
  id = 1:15,
  name = c("Alice", "Bob", "Charlie", "Dana", NA, "Eli", "Frank", "Grace", "Henry", "Ivy", "Jack", "Kara", "Leo", "Mona", "N/A"),
  age = c(25, 34, NA, 45, 52, "Thirty", 29, 37, NA, 22, 43, 31, 28, "unknown", 30),
  gender = c("F", "M", "M", "F", "F", "M", "Male", "Female", "F", "F", "M", "F", "M", NA, "M"),
  income = c(50000, 60000, 55000, 58000, 62000, 48000, NA, 57000, 59000, 51000, "unknown", 63000, 54000, 56000, 52000),
  joinDate = c("2020-01-15", "2019-07-30", "2018/06/22", "15-08-2017", NA, "2021-10-10", "2020-05-05", "unknown", "2017-03-18", "2019-12-01", "2022-01-01", "2016-11-11", "2018-08-08", "2023-02-02", "2017-07-07"),
  department = c("HR", "Finance", "HR", "IT", "HR", "finance", "NA", "IT", "Operations", "HR", "HR", "finance", "IT", "HR", "Operations")
)

  # 1. View structure of the data
glimpse(raw_data)

# 2. Convert Age to numeric (handling non-numeric entries)
clean_data <- raw_data %>%
  mutate(Age = parse_number(as.character(age)))

# 3. Convert Income to numeric (handling non-numeric)
clean_data <- clean_data %>%
  mutate(income = parse_number(as.character(income)))

# 4. Fix Gender inconsistencies
clean_data <- clean_data %>%
  mutate(gender = case_when(
    gender %in% c("M", "Male") ~ "Male",
    gender %in% c("F", "Female") ~ "Female",
    TRUE ~ NA_character_
  ))

# Exercise
# Get the 30 age

# 5. Standardize Department values (title case)
clean_data <- clean_data %>%
  mutate(department = str_to_title(department))

# 6. Convert JoinDate to proper Date format
clean_data <- clean_data %>%
  mutate(joinDate = parse_date(joinDate, format = "%Y-%m-%d", na = "unknown"))

## Exercise
#Get the NA dates

# 7. Handle alternative date formats
clean_data <- clean_data %>%
  mutate(joinDate = if_else(is.na(joinDate),
                            parse_date(raw_data$joinDate, format = "%d-%m-%Y"),
                            joinDate))

# 8. Remove rows where Name is missing or “N/A”
clean_data <- clean_data %>%
  filter(!is.na(name) & name != "N/A")

# 9. Remove rows where Age is NA
clean_data <- clean_data %>%
  filter(!is.na(Age))

# 10. Create Age Group column
clean_data <- clean_data %>%
  mutate(age_group = case_when(
    age < 30 ~ "Young",
    age >= 30 & age < 45 ~ "Middle",
    # between(age,30, 45) ~ "Middle",
    age >= 45 ~ "Senior"
  ))

# 11. Create a flag for high income (>55k)
clean_data <- clean_data %>%
  mutate(high_income = if_else(income > 55000, TRUE, FALSE))

# 12. Extract year from JoinDate
clean_data <- clean_data %>%
  mutate(join_year = lubridate::year(joinDate)) #mdy, ymd

# 13. Count number of records per Department
clean_data %>%
  count(department)

# 14. Rename Income to AnnualIncome
clean_data <- clean_data %>%
  rename(annual_income = income)

# 15. Select only key columns
clean_data <- clean_data %>%
  select(id, name, age, gender, department, annual_income)

# 16. Arrange by Age descending
clean_data <- clean_data %>%
  arrange(desc(age))

# 17. Filter rows where Age is over 35
over_35 <- clean_data %>%
  filter(age > 35)

# 18. Group by Department and summarize average income
clean_data %>%
  group_by(department) %>%
  summarise(avg_income = mean(annual_income, na.rm = TRUE),
            med_income = median(annual_income, na.rm = T),
            n = n())

# 19. Add rank by income
clean_data <- clean_data %>%
  mutate(income_rank = dense_rank(desc(annual_income)))

# 20. Identify duplicate names
clean_data %>%
  count(name) %>%
  filter(n > 1)

# 21. Add row ID if needed
clean_data <- clean_data %>%
  mutate(row_id = row_number())

# 22. Replace NA in Gender with "Unknown"
clean_data <- clean_data %>%
  mutate(gender = replace_na(gender, "Unknown"))

# 23. Replace NA in Age with median
median_age <- median(clean_data$age, na.rm = TRUE)
clean_data <- clean_data %>%
  mutate(age = replace_na(age, median_age))

# 24. Pivot wider: suppose we have multiple scores
# Example (simulate new scores table)
scores <- tibble(
  id = rep(1:5, each = 2),
  subject = rep(c("Math", "Science"), 5),
  score = sample(60:100, 10)
)

wide_scores <- scores %>%
  pivot_wider(names_from = subject, values_from = score)

wide_scores %>% 
  pivot_longer(cols =  c("Math" , "Science"), names_to = "subject",values_to = "score")

# 25. Join with scores data
clean_data <- clean_data %>%
  left_join(wide_scores, by = "id")

# 26. Filter departments starting with "H"
clean_data %>%
  filter(str_starts(department, "H"))

# 27. Summarise count and mean income by gender
clean_data %>%
  group_by(gender) %>%
  summarise(count = n(),
            mean_income = mean(annual_income, na.rm = TRUE))

# 28. Detect outliers in income (boxplot method)
q1 <- quantile(clean_data$annual_income, 0.25, na.rm = TRUE)
q3 <- quantile(clean_data$annual_income, 0.75, na.rm = TRUE)

iqr <- q3 - q1

outliers <- clean_data %>%
  filter(annual_income < (q1 - 1.5*iqr) | annual_income > (q3 + 1.5*iqr))

# 29. Collapse all NA values in any column
na_summary <- clean_data %>%
  summarise_all(~sum(is.na(.)))

# 30. Save clean data to CSV
write_csv(clean_data, "output/clean_employee_data.csv")

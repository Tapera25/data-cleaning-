# Author: Oscar
# Date: 2025-06-04
# Description: Lesson on Exploratory data Analysis (EDA) using R.
# Github Link: https://github.com/your_repo

# Load necessary libraries
library(tidyverse)
library(ggtext)

# Set the seed for reproducibility
set.seed(123)


# Create a hospital 100 sample dataset with 5 variables (numeric, character, date)
df <- tibble(
  id = 1:100,
  gender = sample(c("M", "F"), 100, replace = TRUE),
  age = sample(2:90, 100, replace = TRUE),
  admission_date = sample(seq(as.Date('2020-01-01'), as.Date('2023-01-01'),
                              by="day"), 100, replace = TRUE),
  diagnosis = sample(c("Flu", "Cold", "COVID-19", "Diabetes", "Hypertension"),
                     100, replace = TRUE)
  
)

# Display the first few rows of the dataset
df %>%
  head()

# glimpse
df %>% glimpse()

# EDA ---------------------------------------------------------------------

## Histogram -------------------------------------------------------------

# ggplot(df, aes(x = age)) +
#   geom_histogram(binwidth = 5, fill = "#FE7800", color = "black") +
#   labs(title = "Histogram of Age", x = "Age", y = "Count") +
#   theme_minimal()

# Histogram of age
df %>%
  ggplot( aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#f5b041",
                 color = "black") +
  labs(title = "Histogram of Age", x = "Age", y = "Count") +
  theme_bw()


# histogram of age with density
df %>%
  ggplot(aes(x = age)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "#FE7800",
                 color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(title = "Histogram and Density of Age", x = "Age", y = "Density") +
  theme_bw()


# Histogram of age with density and rug plot
df %>%
  ggplot(aes(x = age)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "#FE7800",
                 color = "black") +
  geom_density(color = "blue", size = 1) +
  geom_rug(sides = "b", color = "darkgreen") +
  labs(title = "Histogram, Density, and Rug Plot of Age",
       x = "Age", y = "Density") +
  theme_bw()



ggplot(df, aes(x = age, fill = gender)) +
  geom_histogram(position = "identity", alpha = 0.4,
                 bins = 15, color = "black") +
  labs(
    title = "Age Distribution by Gender",
    x = "Age",
    y = "Count"
  ) +
  scale_fill_manual(values = c("M" = "blue", "F" = "pink")) +
  theme_minimal()


ggplot() +
  # Male histogram
  geom_histogram(
    data = df %>% filter(gender == "M"),
    aes(x = age, fill = "Male"),
    alpha = 0.3,
    bins = 15,
    color = "black"
  ) +
  # Female histogram
  geom_histogram(
    data = df %>% filter(gender == "F"),
    aes(x = age, fill = "Female"),
    alpha = 0.7,
    bins = 15,
    color = "black"
  ) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  labs(
    title = "Histogram of Age by Gender with Different Opacity",
    x = "Age",
    y = "Count",
    fill = "Gender"
  ) +
  theme_minimal()


## Boxplot -----------------------------------------------------------------


# Boxplot of diagnosis by age
boxplot_ex <-  df %>%
  ggplot(aes(x = diagnosis, y = age, fill = diagnosis)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Diagnosis", x = "Diagnosis", y = "Age") +
  theme_bw() +
  theme(legend.position = "none")

plotly::ggplotly(boxplot_ex)

# save the boxplot as an image
ggsave("output/boxplot_age_diagnosis.png",
       plot = boxplot_ex,
       width = 8, height = 6)


# Boxplot of diagnosis by age with jitter
df %>%
  ggplot(aes(x = diagnosis, y = age, fill = diagnosis)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  labs(title = "Boxplot of Age by Diagnosis with Jitter",
       x = "Diagnosis", y = "Age") +
  theme_bw() +
  theme(legend.position = "none")

# Boxplot of diagnosis by age with jitter and annotation
df %>%
  ggplot(aes(x = diagnosis, y = age, fill = diagnosis)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  labs(title = "Boxplot of Age by Diagnosis with Jitter and Annotation",
       x = "Diagnosis", y = "Age") +
  theme_bw() +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 80, label = "High Age", color = "red", size = 4) +
  annotate("text", x = 2, y = 70, label = "Moderate Age", color = "blue", size = 4)


ggplot(df, aes(x = diagnosis, y = age, fill = diagnosis)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  theme_minimal() +
  labs(
    title = "Age Distribution by Diagnosis",
    subtitle = "COVID-19 patients appear older on average",
    y = "Age", x = "Diagnosis",
    caption = "Data source: Hospital Admissions Dataset",
    
  ) +
  annotate("curve", x = 3, xend = 3, y = 15, yend = 3,
           arrow = arrow(length = unit(0.02, "npc")), color = "red") +
  annotate("text", x = 3, y = 15, label = "Outlier in COVID-19 group?",
           color = "red")


## Line Plots -----------------------------------------------------

# Line plot of number of admissions by date
df %>%
  group_by(admission_date) %>%
  summarise(num_admissions = n()) %>%
  ggplot(aes(x = admission_date, y = num_admissions)) +
  geom_line(color = "#FE7800", size = 1) +
  labs(title = "Number of Admissions by Date",
       x = "Admission Date", y = "Number of Admissions") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


df %>%
  count(month = floor_date(admission_date, "month")) %>%
  ggplot(aes(x = month, y = n)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(size = 2, color = "steelblue") +
  theme_minimal() +
  labs(
    title = "Monthly Hospital Admissions (2020–2023)",
    subtitle = "Spikes in early 2021 possibly related to COVID-19 waves",
    x = "Month", y = "Admissions"
  ) +
  annotate("rect", xmin = as.Date("2021-03-01"), xmax = as.Date("2021-04-30"),
           ymin = 0, ymax = Inf, alpha = 0.1, fill = "red") +
  annotate("text", x = as.Date("2021-02-01"), y = max(df$id)/5,
           label = "COVID-19 spike?", color = "red")


## Bar plots ---------------------------------------------------------------


# Bar plot of number of admissions by diagnosis
df %>%
  group_by(diagnosis) %>%
  summarise(num_admissions = n()) %>%
  ggplot(aes(x = diagnosis, y = num_admissions, fill = diagnosis)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Admissions by Diagnosis",
       x = "Diagnosis", y = "Number of Admissions") +
  theme_bw() +
  theme(legend.position = "none")

df %>%
  count(gender, diagnosis) %>%
  ggplot(aes(x = diagnosis, y = n, fill = gender)) +
  geom_col(position = "dodge") +
  labs(
    title = "Diagnosis Count by Gender",
    y = "Count", x = "Diagnosis"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()



## Density plots -----------------------------------------------------------

ggplot(df, aes(x = age, fill = gender)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ diagnosis) +
  labs(
    title = "Age Density by Diagnosis and Gender",
    x = "Age", y = "Density"
  ) +
  theme_minimal()
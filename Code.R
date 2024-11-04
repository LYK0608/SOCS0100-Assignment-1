# Set Up My Environment
setwd("/Users/felixli/Desktop/SOCS0100-Assignment-1")
rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  kableExtra, 
  flextable,
  glue,
  ggplot2
)
options(scipen=999)

# Import My Data
energy_access_data <- read.csv("/Users/felixli/Desktop/SOCS0100-Assignment-1/Number of people with and without energy access (OWID based on World Bank, 2021).csv", header = TRUE)

# Try to do some Data Exploration
library(skimr)
skim(energy_access_data)
str(energy_access_data)
head(energy_access_data)
summary(energy_access_data)

# Missing Values
missing_values <- energy_access_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))
print("Number of missing values for each column:")
print(missing_values)
skim(energy_access_data)

# Data Types and Unique Value Counts
data_types <- energy_access_data %>%
  summarise(across(everything(), ~ class(.)))
unique_counts <- energy_access_data %>%
  summarise(across(everything(), ~ length(unique(.))))
print("Data types for each column:")
print(data_types)
print("Number of unique values for each column:")
print(unique_counts)

# Try to List All Unique Entity Names
unique_entities <- unique(energy_access_data$Entity)
print(unique_entities)

# Remove unnecessary regions and international organizations
entities_to_remove <- c(
  "Africa Eastern and Southern", "Africa Western and Central", "Arab World",
  "Caribbean Small States", "Central Europe and the Baltics", "Early-demographic dividend",
  "East Asia & Pacific", "East Asia & Pacific (excluding high income)", "East Asia & Pacific (IDA & IBRD)",
  "Euro area", "Europe & Central Asia", "Europe & Central Asia (excluding high income)", 
  "Europe & Central Asia (IDA & IBRD)", "European Union", "Fragile and conflict affected situations", 
  "Heavily indebted poor countries (HIPC)", "High income", "IBRD only", "IDA & IBRD total", 
  "IDA blend", "IDA only", "IDA total", "Late-demographic dividend", "Latin America & Caribbean", 
  "Latin America & Caribbean (excluding high income)", "Latin America & Caribbean (IDA & IBRD)",
  "Least developed countries: UN classification", "Low & middle income", "Low income", "Lower middle income",
  "Middle East & North Africa", "Middle East & North Africa (excluding high income)", 
  "Middle East & North Africa (IDA & IBRD)", "Middle income", "North America", "OECD members", 
  "Other small states", "Pacific island small states", "Post-demographic dividend", 
  "Pre-demographic dividend", "Small states", "South Asia", "South Asia (IDA & IBRD)",
  "Sub-Saharan Africa", "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa (IDA & IBRD)",
  "Upper middle income", "World"
)
filtered_data <- energy_access_data %>%
  filter(!Entity %in% entities_to_remove)

# Dealing Missing Values
filtered_data_no_na <- filtered_data %>%
  drop_na()

# Descriptive Statistics
descriptive_stats <- filtered_data %>%
  summarise(across(where(is.numeric), list(
    mean = ~ mean(., na.rm = TRUE),
    median = ~ median(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE)
  )))
print("Descriptive statistics for numeric columns:")
print(descriptive_stats)
View(descriptive_stats)

# Renaming
selected_data <- filtered_data_no_na %>%
  select(Entity, Year, 
         Number.of.people.with.access.to.electricity,
         Number.of.people.without.access.to.electricity,
         number_with_clean_fuels_cooking,
         number_without_clean_fuels_cooking) %>%
  rename(
    Country = Entity,
    Year = Year,
    Electricity = Number.of.people.with.access.to.electricity,
    No_Electricity = Number.of.people.without.access.to.electricity,
    Clean_Fuels = number_with_clean_fuels_cooking,
    No_Clean_Fuels = number_without_clean_fuels_cooking
  )

head(selected_data)

# New Variable of Electricity Access Rate
selected_data <- selected_data %>%
  mutate(Access_Rate = Electricity / 
           (Electricity + No_Electricity)
         )
head(selected_data)

# Electricity Access Rate Classification
selected_data <- selected_data %>%
  mutate(
    Access_Level = case_when(
      Access_Rate >= 0.8 ~ "High",
      Access_Rate >= 0.5 & Access_Rate < 0.8 ~ "Medium",
      TRUE ~ "Low"
    )
  )
str(selected_data) 

# Pivot Long
long_format_data <- selected_data %>%
  pivot_longer(cols = c(Electricity, No_Electricity, Clean_Fuels, No_Clean_Fuels),
               names_to = "Variable",
               values_to = "Value")

head(long_format_data)


# Combing All Years
yearly_summary <- selected_data %>%
  group_by(Year) %>%
  summarise(
    avg_access_rate = mean(Access_Rate, na.rm = TRUE),
    avg_clean_fuels = mean(Clean_Fuels, na.rm = TRUE)
  )
print("Yearly Summary:")
print(yearly_summary)

# Draw Bar Charts
ggplot(selected_data, aes(x = reorder(Country, -Access_Rate), y = Access_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Access Rate to Electricity by Country",
    x = "Country",
    y = "Access Rate"
  )

# 定义一个函数来计算每个国家的某个变量的平均值
compute_average <- function(data, group_var, value_var) {
  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(avg_value = mean(.data[[value_var]], na.rm = TRUE))
}

# 使用函数计算不同国家的平均清洁燃料使用人数
average_clean_fuels <- compute_average(selected_data, "Country", "Clean_Fuels")
print(average_clean_fuels)





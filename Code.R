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
filter_entities <- function(data, entities_to_remove) {
  filtered_data <- data %>%
    filter(!Entity %in% entities_to_remove)
  return(filtered_data)
}

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

filtered_data <- filter_entities(energy_access_data, entities_to_remove)

head(filtered_data)

# Dealing Missing Values - Remove rows with any missing values
handle_missing_values <- function(data) {
  clean_data <- data %>%
    drop_na()
  return(clean_data)
}

filtered_data_no_na <- handle_missing_values(filtered_data)
head(filtered_data_no_na)


# Descriptive Statistics
descriptive_stats <- filtered_data_no_na %>%
  summarise(across(where(is.numeric), list(
    mean = ~ mean(., na.rm = TRUE),
    median = ~ median(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE)
  )))
print("Descriptive statistics for numeric columns:")
print(descriptive_stats)

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
  mutate(Electricity_Access_Rate = Electricity / 
           (Electricity + No_Electricity)
         )
head(selected_data)

# New Variable of Clean Fuels Access Rate
selected_data <- selected_data %>%
  mutate(Clean_Fuels_Access_Rate = Clean_Fuels / 
           (Clean_Fuels + No_Clean_Fuels)
  )
head(selected_data)

# Select the latest year data for each country
select_latest_year <- function(data) {
  latest_data <- data %>%
    group_by(Country) %>%
    filter(Year == max(Year)) %>%
    ungroup()
  return(latest_data)
}

latest_data <- select_latest_year(selected_data)

head(latest_data)


# Plot a histogram of the electricity access rate
ggplot(latest_data, aes(x = Electricity_Access_Rate)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(
    title = "Distribution of Electricity Access Rate (Latest Year)",
    x = "Electricity Access Rate",
    y = "Frequency"
  )

# Summarize Electricity Access Rate by Year
yearly_summary <- selected_data %>%
  group_by(Year) %>%
  summarise(mean_electricity_access_rate = mean(Electricity_Access_Rate, na.rm = TRUE),
            mean_clean_fuels_access_rate = mean(Clean_Fuels_Access_Rate, na.rm = TRUE))
print(yearly_summary)

# Plot the Trend Chart by Year
ggplot(yearly_summary, aes(x = Year)) +
  geom_line(aes(y = mean_electricity_access_rate, color = "Electricity Access Rate")) +
  geom_line(aes(y = mean_clean_fuels_access_rate, color = "Clean Fuels Access Rate")) +
  labs(title = "Trends of Access Rates Over the Years", x = "Year", y = "Average Access Rate") +
  scale_color_manual("", values = c("Electricity Access Rate" = "blue", "Clean Fuels Access Rate" = "red"))

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
    filter_all(all_vars(!is.na(.) & . != 0 & trimws(.) != ""))
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

selected_data <- selected_data %>%
  mutate(
    Electricity_Access_Rate = if_else((Electricity + No_Electricity) == 0 | Electricity == 0, NA_real_, Electricity / (Electricity + No_Electricity)),
    Clean_Fuels_Access_Rate = if_else((Clean_Fuels + No_Clean_Fuels) == 0 | Clean_Fuels == 0, NA_real_, Clean_Fuels / (Clean_Fuels + No_Clean_Fuels))
  )


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

plot_bar <- function(data, x_column, y_column, top_n_countries = 10, width = 15, height = 8, output_file = NULL, ascending = FALSE) {
  if (ascending) {
    data <- data %>%
      arrange(!!sym(y_column)) %>% 
      head(top_n_countries)
  } else {
    data <- data %>%
      arrange(desc(!!sym(y_column))) %>% 
      head(top_n_countries)
  }
  data[[x_column]] <- factor(data[[x_column]], levels = data[[x_column]])
  p <- ggplot(data, aes(x = !!sym(x_column), y = !!sym(y_column))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = round(!!sym(y_column), 2)), vjust = -0.3) +
    labs(
      title = paste(y_column, "by", x_column, if (ascending) {
        "\n(The 10 countries with the lowest"
      } else {
        "\n(The 10 countries with the highest"
      }, y_column, ")"),
      x = x_column,
      y = y_column
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    ylim(0, 1)  
  if (!is.null(output_file)) {
    ggsave(output_file, plot = p, width = width, height = height)
  }
  
  print(p)
}

# Visualization of the 10 Countries with the Highest Clean Fuels Access Rates
plot_bar(latest_data, "Country", "Clean_Fuels_Access_Rate", top_n_countries = 10)

# Visualization of the 10 Countries with the Highest Electricity Access Rates
plot_bar(latest_data, "Country", "Electricity_Access_Rate", top_n_countries = 10)

# Visualization of the 10 Countries with the Lowest Clean Fuels Access Rates
plot_bar(latest_data, "Country", "Clean_Fuels_Access_Rate", top_n_countries = 10, ascending = TRUE)

# Visualization of the 10 Countries with the Lowest Electricity Access Rates
plot_bar(latest_data, "Country", "Electricity_Access_Rate", top_n_countries = 10, ascending = TRUE)









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

# Dealing with Missing or Zero Values - Remove rows with any missing, zero, or invalid values in critical columns
filtered_data_no_na <- filtered_data %>%
  filter(
    !is.na(Number.of.people.with.access.to.electricity) & 
      Number.of.people.with.access.to.electricity > 0 &
      !is.na(Number.of.people.without.access.to.electricity) & 
      Number.of.people.without.access.to.electricity > 0 &
      !is.na(number_with_clean_fuels_cooking) & 
      number_with_clean_fuels_cooking > 0 &
      !is.na(number_without_clean_fuels_cooking) & 
      number_without_clean_fuels_cooking > 0
  )

# Rename columns for easier access
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

# Calculate Access Rates
selected_data <- selected_data %>%
  mutate(
    Electricity_Access_Rate = if_else((Electricity + No_Electricity) == 0, NA_real_, Electricity / (Electricity + No_Electricity)),
    Clean_Fuels_Access_Rate = if_else((Clean_Fuels + No_Clean_Fuels) == 0, NA_real_, Clean_Fuels / (Clean_Fuels + No_Clean_Fuels))
  )

# Select the latest year data for each country
latest_data <- selected_data %>%
  group_by(Country) %>%
  filter(Year == max(Year)) %>%
  ungroup()

# Function to Plot Top or Bottom N Countries by Access Rate
plot_bar <- function(data, x_column, y_column, top_n_countries = 10, width = 15, height = 8, output_file = NULL, ascending = FALSE) {
  # Filter out rows with NA in the selected y_column to avoid incorrect visualizations
  data <- data %>%
    filter(!is.na(!!sym(y_column)))
  
  # Sort and select top or bottom countries
  if (ascending) {
    data <- data %>%
      arrange(!!sym(y_column)) %>% 
      head(top_n_countries)
  } else {
    data <- data %>%
      arrange(desc(!!sym(y_column))) %>% 
      head(top_n_countries)
  }
  
  data[[x_column]] <- factor(data[[x_column]], levels = data[[x_column]])
  
  # Create plot
  p <- ggplot(data, aes(x = !!sym(x_column), y = !!sym(y_column))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = round(!!sym(y_column), 2)), vjust = -0.3) +
    labs(
      title = paste(y_column, "by", x_column, if (ascending) {
        "\n(The 10 countries with the lowest"
      } else {
        "\n(The 10 countries with the highest"
      }, y_column, ")"),
      x = x_column,
      y = y_column
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    ylim(0, 1)  
  
  # Save plot if output_file is provided
  if (!is.null(output_file)) {
    ggsave(output_file, plot = p, width = width, height = height)
  }
  
  print(p)
}

# Visualization of the 10 Countries with the Highest and Lowest Clean Fuels Access Rates
plot_bar(latest_data, "Country", "Clean_Fuels_Access_Rate", top_n_countries = 10)
plot_bar(latest_data, "Country", "Clean_Fuels_Access_Rate", top_n_countries = 10, ascending = TRUE)

# Visualization of the 10 Countries with the Highest and Lowest Electricity Access Rates
plot_bar(latest_data, "Country", "Electricity_Access_Rate", top_n_countries = 10)
plot_bar(latest_data, "Country", "Electricity_Access_Rate", top_n_countries = 10, ascending = TRUE)


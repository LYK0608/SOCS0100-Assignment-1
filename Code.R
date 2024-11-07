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

# Import data
energy_access_data <- read.csv("Number of people with and without energy access (OWID based on World Bank, 2021).csv", header = TRUE)

# Data exploration
library(skimr)  # Load the data exploration package
library(dplyr)
library(tidyr)
skim(energy_access_data)  # Get data overview
str(energy_access_data)  # Display data structure
head(energy_access_data)  # Show the first few rows of data
summary(energy_access_data)  # Overview statistics

# Check for missing values
missing_values <- energy_access_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))
print("Number of missing values in each column:")
print(missing_values)
skim(energy_access_data)  # Get data overview again

# Data types and unique value counts
data_types <- energy_access_data %>%
  summarise(across(everything(), ~ class(.)))
unique_counts <- energy_access_data %>%
  summarise(across(everything(), ~ length(unique(.))))
print("Data types for each column:")
print(data_types)
print("Unique value count for each column:")
print(unique_counts)

# List all unique entity names
unique_entities <- unique(energy_access_data$Entity)
print(unique_entities)

# Remove unnecessary regions and international organizations
filter_entities <- function(data, entities_to_remove) {
  filtered_data <- data %>%
    filter(!Entity %in% entities_to_remove)  # Filter out unselected entities
  return(filtered_data)
}

# Define entities to be removed
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

# Filter data
filtered_data <- filter_entities(energy_access_data, entities_to_remove)

# Define a function to calculate statistics for numeric columns
calculate_statistics <- function(data) {
  # Get numeric columns, excluding the first and second columns
  numeric_columns <- data %>% select(-c(1, 2)) %>% select(where(is.numeric))
  # Initialize a list to store statistics
  stats_list <- list()
  # Loop through each numeric column
  for (col in names(numeric_columns)) {
    # Filter out 0 and missing values
    filtered_col <- numeric_columns[[col]] %>% na.omit() %>% .[. != 0]
    # Calculate statistics
    stats <- list(
      mean = mean(filtered_col),
      sd = sd(filtered_col),
      min = min(filtered_col),
      max = max(filtered_col),
      median = median(filtered_col)
    )
    # Add statistics to the list
    stats_list[[col]] <- stats
  }
  return(stats_list)
}
# Call the function to calculate statistics
stats <- calculate_statistics(filtered_data)
# Print statistics
print(stats)

handle_missing_values <- function(data) {
  # Group data by Entity column
  grouped_data <- data %>% group_by(Entity)
  
  # Calculate column means (ignore NA) within each group
  means <- grouped_data %>% 
    summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = 'drop')
  # Fill missing values, 0, and empty values with means
  clean_data <- grouped_data %>%
    mutate(across(where(is.numeric), ~ {
      col_name <- cur_column()
      ifelse(is.na(.) | . == 0, means[[col_name]][match(Entity, means$Entity)], .)
    })) %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.) | trimws(.) == "", "Default Value", .)))  # Fill string columns with "Default Value"
  
  return(clean_data)
}

# Test function
filtered_data_no_na <- handle_missing_values(filtered_data)  # Cleaned data
head(filtered_data_no_na)  # View the first few rows of data

# Secondary cleaning, define a function to filter data
filter_data <- function(data) {
  # Delete rows with missing values
  data <- na.omit(data)
  # Delete rows where all columns have a value of 0
  data <- data[rowSums(data == 0) == 0, ]
  # Check each column, remove entire row if values are less than 100
  data <- data[rowSums(data < 100) == 0, ]
  return(data)
}

filtered_data_no_na <- filter_data(filtered_data_no_na)

# Output cleaned data to a CSV file
output_file <- "cleaned_data.csv"
write.csv(filtered_data_no_na, file = output_file, row.names = FALSE)

# Descriptive statistics
descriptive_stats <- filtered_data_no_na %>%
  summarise(across(where(is.numeric), list(
    mean = ~ mean(., na.rm = TRUE),  # Calculate mean
    median = ~ median(., na.rm = TRUE),  # Calculate median
    sd = ~ sd(., na.rm = TRUE),  # Calculate standard deviation
    min = ~ min(., na.rm = TRUE),  # Calculate minimum
    max = ~ max(., na.rm = TRUE)  # Calculate maximum
  )))
print("Descriptive statistics for numeric columns:")
print(descriptive_stats)

# Select variables and rename columns
selected_data <- filtered_data_no_na %>%
  select(
    Entity, 
    Year, 
    Number.of.people.with.access.to.electricity,
    Number.of.people.without.access.to.electricity,
    number_with_clean_fuels_cooking,
    number_without_clean_fuels_cooking
  ) %>%
  rename(
    Country = Entity,  # Rename to Country
    Year = Year,
    Electricity = Number.of.people.with.access.to.electricity,
    No_Electricity = Number.of.people.without.access.to.electricity,
    Clean_Fuels = number_with_clean_fuels_cooking,
    No_Clean_Fuels = number_without_clean_fuels_cooking
  )

process_data <- function(df) {
  # Select variables
  selected_data <- df %>%
    select(Country, Year, Electricity, No_Electricity, Clean_Fuels, No_Clean_Fuels)
  
  # Create new variable
  new_variable_data <- selected_data %>%
    mutate(Total_Population = Electricity + No_Electricity)
  
  # Reshape data frame
  reshaped_data <- new_variable_data %>%
    pivot_longer(
      cols = c(Electricity, No_Electricity, Clean_Fuels, No_Clean_Fuels, Total_Population),
      names_to = "Variable",
      values_to = "Value"
    )
  
  return(reshaped_data)
}

reshaped_data<- process_data(selected_data)

# View final processed data
print(head(reshaped_data))

# Calculate electricity access rate
selected_data <- selected_data %>%
  mutate(
    Electricity_Access_Rate = if_else((Electricity + No_Electricity) == 0 | Electricity == 0, NA_real_, Electricity / (Electricity + No_Electricity)),
    Clean_Fuels_Access_Rate = if_else((Clean_Fuels + No_Clean_Fuels) == 0 | Clean_Fuels == 0, NA_real_, Clean_Fuels / (Clean_Fuels + No_Clean_Fuels))
  )

# Select the latest year data for each country
latest_data <- selected_data %>%
  group_by(Country) %>%
  filter(Year == max(Year)) %>%
  ungroup()

head(latest_data)  # View the latest year's data

# Output cleaned data to a CSV file
output_file <- "cleaned_data_2.csv"
write.csv(latest_data, file = output_file, row.names = FALSE)

# Plot histogram of electricity access rate
ggplot(latest_data, aes(x = Electricity_Access_Rate)) +
  geom_histogram(binwidth = 0.05, fill = "#76afda", color = "black") +
  labs(
    title = "Distribution of Electricity Access Rate (Latest Year)",
    x = "Electricity Access Rate",
    y = "Frequency"
  )


# Summarize electricity access rate by year
yearly_summary <- selected_data %>%
  group_by(Year) %>%
  summarise(mean_electricity_access_rate = mean(Electricity_Access_Rate, na.rm = TRUE),  # Calculate mean
            mean_clean_fuels_access_rate = mean(Clean_Fuels_Access_Rate, na.rm = TRUE))  # Calculate mean
print(yearly_summary)

# Visualize yearly summary of electricity access rate
plot_summary <- function(data) {
  # Use ggplot2 to plot line chart
  p <- ggplot(data, aes(x = Year)) +
    geom_line(aes(y = mean_electricity_access_rate, color = "Electricity Access Rate"), linetype = "solid", size = 0.75) +
    geom_line(aes(y = mean_clean_fuels_access_rate, color = "Clean Fuels Access Rate"), linetype = "dashed", size = 0.75) +
    labs(title = "Yearly Summary of Electricity Access Rate and Clean Fuels Access Rate",
         x = "Year",
         y = "Access Rate (%)",
         color = "Indicator") +
    theme_minimal() +
    theme(
      legend.position = "bottom",  # Place legend at bottom
      legend.background = element_rect(fill = "transparent")  # Make legend background transparent
    )
  
  # Print chart
  print(p)
  
  # Save chart as a PNG file
  ggsave("yearly_summary_plot.png", plot = p, width = 10, height = 6, units = "in")
}

plot_summary(yearly_summary)

# Define function to plot scatter plot
plot_scatter <- function(data, x_column, y_column, top_n_countries = 10, width = 15, height = 8, output_file = NULL, ascending = FALSE) {
  if (ascending) {
    data <- data %>%
      arrange(!!sym(y_column)) %>% 
      head(top_n_countries)  # Sort in ascending order
  } else {
    data <- data %>%
      arrange(desc(!!sym(y_column))) %>% 
      head(top_n_countries)  # Sort in descending order
  }
  
  p <- ggplot(data, aes(x = !!sym(x_column), y = !!sym(y_column))) +
    geom_point(size = 4, color = "steelblue") +  # Plot scatter plot
    geom_text(aes(label = round(!!sym(y_column), 2)), vjust = -0.3) +  # Add data labels
    labs(
      title = paste(y_column, "by", x_column, if (ascending) {
        "\n(Top 50 countries with lowest access rate)"
      } else {
        "\n(Top 50 countries with highest access rate)"
      }),
      x = x_column,
      y = y_column
    ) +
    theme_minimal() +  # Use minimal theme
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate x-axis labels
    ) +
    ylim(0, 1)  # Set y-axis range
  
  if (!is.null(output_file)) {
    if (file.exists(dirname(output_file))) {
      ggsave(output_file, plot = p, width = width, height = height)  # Save image
      message("Image saved to: ", output_file)
    } else {
      warning("Output directory does not exist: ", dirname(output_file))
    }
  }
  
  print(p)  # Print image
}

# Save plot examples
plot_scatter(latest_data, "Country", "Clean_Fuels_Access_Rate", top_n_countries = 50, output_file = "clean_fuels_top_50.png")
plot_scatter(latest_data, "Country", "Electricity_Access_Rate", top_n_countries = 50, output_file = "electricity_top_50.png")
plot_scatter(latest_data, "Country", "Clean_Fuels_Access_Rate", top_n_countries = 50, ascending = TRUE, output_file = "clean_fuels_bottom_50.png")
plot_scatter(latest_data, "Country", "Electricity_Access_Rate", top_n_countries = 50, ascending = TRUE, output_file = "electricity_bottom_50.png")

# Visualize the top 10 countries with the highest electricity access rate
plot_bar <- function(data, x_column, y_column, top_n_countries = 10, width = 15, height = 8, output_file = NULL, ascending = FALSE) {
  if (ascending) {
    data <- data %>%
      arrange(!!sym(y_column)) %>% 
      head(top_n_countries)  # Sort in ascending order
  } else {
    data <- data %>%
      arrange(desc(!!sym(y_column))) %>% 
      head(top_n_countries)  # Sort in descending order
  }
  data[[x_column]] <- factor(data[[x_column]], levels = data[[x_column]])  # Convert x-axis to factor
  p <- ggplot(data, aes(x = !!sym(x_column), y = !!sym(y_column))) +
    geom_bar(stat = "identity", fill = "steelblue") +  # Plot bar chart
    geom_text(aes(label = round(!!sym(y_column), 2)), vjust = -0.3) +  # Add data labels
    labs(
      title = paste(y_column, "by", x_column, if (ascending) {
        "\n(Top 10 countries with lowest access rate)"
      } else {
        "\n(Top 10 countries with highest access rate)"
      }, y_column, ")"),
      x = x_column,
      y = y_column
    ) +
    theme_minimal() +  # Use minimal theme
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate x-axis labels
    ) +
    ylim(0, 1)  # Set y-axis range
  if (!is.null(output_file)) {
    ggsave(output_file, plot = p, width = width, height = height)  # Save image
  }
  
  print(p)  # Print image
}

# Plot visualizations for the countries with the highest electricity access rate
plot_bar(latest_data, "Country", "Clean_Fuels_Access_Rate", top_n_countries = 10)  # Highest clean fuel access rate countries
plot_bar(latest_data, "Country", "Electricity_Access_Rate", top_n_countries = 10)  # Highest electricity access rate countries
plot_bar(latest_data, "Country", "Clean_Fuels_Access_Rate", top_n_countries = 10, ascending = TRUE)  # Lowest clean fuel access rate countries
plot_bar(latest_data, "Country", "Electricity_Access_Rate", top_n_countries = 10, ascending = TRUE)  # Lowest electricity access rate countries


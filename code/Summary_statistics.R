# Load necessary libraries
library(readxl)
data<- read_excel("VA_data_filtered.xlsx", 
                               sheet = "Data_Indian Ocean")
# Extract numeric columns
numeric_data <- data[sapply(data, is.numeric)]

# Get summary statistics
summary_stats <- as.data.frame(do.call(rbind, lapply(numeric_data, summary)))


# Get summary statistics including standard deviation
summary_stats <- as.data.frame(do.call(rbind, lapply(numeric_data, function(x) {
  c(Mean = mean(x, na.rm = TRUE), 
    SD = sd(x, na.rm = TRUE), 
    Min = min(x, na.rm = TRUE), 
    Max = max(x, na.rm = TRUE), 
    Median = median(x, na.rm = TRUE))
})))

# View or save as a CSV file
write.csv(summary_stats, "summary_statistics.csv", row.names = TRUE)


# Load necessary libraries
library(ggplot2)
library(tidyr)

# Reshape data to long format
numeric_data <- data %>% select(where(is.numeric))
long_data <- pivot_longer(numeric_data, cols = everything(), names_to = "Variable", values_to = "Value")



# Load necessary libraries
library(dplyr)
library(psych)

# Specify the selected variables 
selected_vars <- c("Edible_portion_coeff", "Edible_catch", "Price", "Fishing_vulnerability", "Climate_vulnerability",
                   "Calcium (mg)", "DHA_EPA (g)", "Iron, total (mg)", "Selenium (mcg)", "Vitamin A (mcg)", 
                   "Zinc (mg)", "B12 (mcg)", "Calcium catch (g)", "DHA_EPA catch (g)", "Iron catch (g)", 
                   "Selenium catch (g)", "VitA catch (g)", "Zinc catch (g)", "B12 catch (g)", "Edible Nut supply (grams)")


# Filter for selected variables and calculate summary stats with standard deviation
summary_stats <- data %>%
  select(all_of(selected_vars)) %>%
  summarise_all(list(
    Min = ~ min(., na.rm = TRUE),
    Median = ~ median(., na.rm = TRUE),
    Mean = ~ mean(., na.rm = TRUE),
    Max = ~ max(., na.rm = TRUE),
    SD = ~ sd(., na.rm = TRUE)  # Adding standard deviation
  ))

# Convert to a data frame and reshape to long format
summary_stats_long <- summary_stats %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", "Statistic"), 
               names_sep = "_", 
               values_to = "Value")

# Filter for selected variables and reshape data to long format
long_data <-data %>%
  select(all_of(selected_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")



# Create faceted violin plot for each numeric variable
ggplot(long_data, aes(x = "", y = Value)) +
  geom_violin(fill = "lightblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +  # Separate plots for each variable with independent scales
  theme_minimal() +
  labs(title = "Separate Violin Plots for Each Numeric Variable", y = "Value") +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks.x = element_blank(), # Remove x-axis ticks
        strip.background = element_rect(fill = "lightblue", color = "black")) # Optional: style for facet labels

# Create faceted box plot for selected numeric variables
ggplot(long_data, aes(x = "", y = Value)) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +  # Separate plots for each variable with independent scales
  theme_minimal() +
  labs(title = "Separate Box Plots for Selected Numeric Variables", y = "Value") +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks.x = element_blank(), # Remove x-axis ticks
        strip.background = element_rect(fill = "lightblue", color = "black")) # Optional: style for facet labels

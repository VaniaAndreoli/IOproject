library(readxl)
library(dplyr)

data <- read_excel("R-data-variables.xlsx", 
                   sheet = "Countries-cwmc")



# Create Q-Q plots for each numeric variable
numeric_columns <- data[, sapply(data, is.numeric)]

# Set up plotting layout to view multiple Q-Q plots in one window
par(mfrow = c(3, 3))  # Adjust the layout based on the number of variables

# Loop through each numeric column and create a Q-Q plot
for (col_name in colnames(numeric_columns)) {
  qqnorm(numeric_columns[[col_name]], main = paste("Q-Q Plot of", col_name))
  qqline(numeric_columns[[col_name]], col = "red")  # Adds a reference line
}

# Reset layout
par(mfrow = c(1, 1))

# Perform Shapiro-Wilk test on each numeric column
normality_tests <- sapply(data[, sapply(data, is.numeric)], function(x) shapiro.test(x)$p.value)

# Display the results
normality_tests


 ##### normality_tests results 
 # Calcium (g/t)         DHA_EPA (g/t)            Iron (g/t)        Selenium (g/t) 
# 8.405817e-03          1.104105e-05          1.671841e-03          2.211807e-03 
# Vitamin A (g/t)            Zinc (g/t)             B12 (g/t)                 Price 
# 1.125240e-09          4.850643e-04          6.520185e-01          1.205149e-05 
# Fishing_vulnerability Climate_vulnerability 
 # 1.972920e-03          9.176338e-02 


## Rename col
data <- data %>%
  rename(cal = `Calcium (g/t)`, omega = `DHA_EPA (g/t)`, iron=`Iron (g/t)`, Sel=`Selenium (g/t)`,
         vitA=`Vitamin A (g/t)`,zinc=`Zinc (g/t)`,b12=`B12 (g/t)`,IORDWF=`IOR-DWF`)

data <- data %>%
  mutate(IORDWF = case_when(
    IORDWF == "DWF" ~ "0",
    IORDWF == "IOR" ~ "1",
    TRUE ~ as.character(IORDWF)  # Keep other values unchanged
  ))

is_numeric<- is.numeric(data$IORDWF)
data<-data %>%
mutate(IORDWF = as.numeric(IORDWF))

# Specify the nutrient to compare
nutrient <- "Climate_vulnerability"

# Check if the nutrient column exists in the data
if (nutrient %in% colnames(data) && "IORDWF" %in% colnames(data)) {
  
  # Print the type and number of NAs before conversion
  cat(sprintf("Column: %s, Type: %s, NAs: %d\n", nutrient, class(data[[nutrient]]), sum(is.na(data[[nutrient]]))))
  
  # Check unique values in the nutrient column
  cat("Unique values in nutrient column:", unique(data[[nutrient]]), "\n")
  
  # Convert the nutrient column to numeric 
  data[[nutrient]] <- as.numeric(as.character(data[[nutrient]]))
  
  # Extract data for IOR and DWF groups, omitting NA values
  ior_data <- na.omit(data[data$IORDWF == 1, nutrient, drop = FALSE])
  dwf_data <- na.omit(data[data$IORDWF == 0, nutrient, drop = FALSE])
  
  # Convert to numeric vectors explicitly
  ior_data <- as.numeric(ior_data[[nutrient]])
  dwf_data <- as.numeric(dwf_data[[nutrient]])
  
  # Print types of extracted data
  cat(sprintf("Type of ior_data for %s: %s\n", nutrient, class(ior_data)))
  cat(sprintf("Type of dwf_data for %s: %s\n", nutrient, class(dwf_data)))
  
  # Check if both groups have data to compare
  if (length(ior_data) > 0 && length(dwf_data) > 0) {
    # Apply the Kolmogorov-Smirnov test
    test_result <- ks.test(ior_data, dwf_data)
    
    # Print the results
    cat(sprintf("Nutrient: %s, p-value: %.4f, Statistic: %.4f\n", 
                nutrient, 
                test_result$p.value, 
                test_result$statistic))
  } else {
    cat(sprintf("Not enough data for nutrient: %s (ior_data: %d, dwf_data: %d)\n", 
                nutrient, length(ior_data), length(dwf_data)))
  }
  
} else {
  cat(sprintf("Column %s or IORDWF does not exist in the data frame.\n", nutrient))
}

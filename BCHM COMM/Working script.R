library(tidyverse)
library(readxl)

library(dplyr)

Raw.data <- read_excel("Dataset_R_2025.xlsx")
head(Raw.data)
str(Raw.data)
Clean.data$Dove <- Raw.data %>% drop_na()
head(Clean.data)
summary(Clean.data)

# Convert the 'Dial' column to numeric and handle non-numeric values
Raw.data <- Raw.data %>%
  mutate(Dial = suppressWarnings(as.numeric(Dial)),
         Dove = suppressWarnings(as.numeric(Dove)))

# Clean the data by removing rows with NA values
Clean.data <- Raw.data %>% drop_na(Dove, Dial)
head(Clean.data)
summary(Clean.data)

# Group by 'Chem_name' and summarize the mean of 'Dove'
grouped_data <- Clean.data %>%
  group_by(Chem_name) %>%
  summarize(mean_value = mean(Dove, na.rm = TRUE))

# Display the grouped and summarized data
print(grouped_data)

# Create a scatter plot (using Dove and Dial columns)
ggplot(Clean.data, aes(x = Dial, y = Dove)) +
  geom_point() +
  labs(title = "Scatter Plot of Dove vs Dial", x = "Dial", y = "Dove")

# Save the cleaned data to a new CSV file
write_excel_csv(Clean.data, "Cleaned_Dataset_R_2025.csv")

library(tidyverse)
library(readxl)


# Read the Excel file
Raw.data <- read_excel("Dataset_R_2025.xlsx")
head(Raw.data)
str(Raw.data)

# Convert the 'Dove' column to numeric and handle non-numeric values
Raw.data <- Raw.data %>%
  mutate(Dove = suppressWarnings(as.numeric(Dove)))

# Clean the data by removing rows with NA values in the 'Dove' column
Clean.data <- Raw.data %>% drop_na(Dove)
head(Clean.data)
summary(Clean.data)

# Create a heat map of Dove values for each Chem_name
ggplot(Clean.data, aes(x = "", y = Chem_name, fill = Dove)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Heat Map of Dove Values", x = "", y = "Chemical Name", fill = "Dove Value") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Save the cleaned data to a new CSV file
write_excel_csv(Clean.data, "Cleaned_Dataset_R_2025.csv")

library(tidyverse)
library(readxl)


# Read the Excel file
Raw.data <- read_excel("Dataset_R_2025.xlsx")
head(Raw.data)
str(Raw.data)

# Convert the 'Dove' column to numeric and handle non-numeric values
Raw.data <- Raw.data %>%
  mutate(Dove = suppressWarnings(as.numeric(Dove)))

# Clean the data by removing rows with NA values in the 'Dove' column
Clean.data <- Raw.data %>% drop_na(Dove)
head(Clean.data)
summary(Clean.data)

# Add a numerical index column
Clean.data <- Clean.data %>% mutate(Index = row_number())

# Create a heat map of Dove values with numerical indices
ggplot(Clean.data, aes(x = "", y = Index, fill = Dove)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Heat Map of Dove Values", x = "", y = "Index", fill = "Dove Value") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Save the cleaned data to a new CSV file
write_excel_csv(Clean.data, "Cleaned_Dataset_R_2025.csv")

#Heat map of dove
library(tidyverse)
library(readxl)


# Read the Excel file
Raw.data <- read_excel("Dataset_R_2025.xlsx")
head(Raw.data)
str(Raw.data)

# Convert the 'Dove' column to numeric and handle non-numeric values
Raw.data <- Raw.data %>%
  mutate(Dove = suppressWarnings(as.numeric(Dove)))

# Clean the data by removing rows with NA values in the 'Dove' column
Clean.data <- Raw.data %>% drop_na(Dove)
head(Clean.data)
summary(Clean.data)

# Add a numerical index column
Clean.data <- Clean.data %>% mutate(Index = row_number())

# Create a heat map of Dove values with numerical indices
ggplot(Clean.data, aes(x = "", y = Index, fill = Dove)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Heat Map of Dove Values", x = "", y = "Index", fill = "Dove Value") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Save the cleaned data to a new CSV file
write_excel_csv(Clean.data, "Cleaned_Dataset_R_2025.csv")

library(tidyverse)
library(readxl)


# Read the Excel file
Raw.data <- read_excel("Dataset_R_2025.xlsx")
head(Raw.data)
str(Raw.data)

# Convert soap columns to numeric and handle non-numeric values
Raw.data <- Raw.data %>%
  mutate(SimpleTruth = suppressWarnings(as.numeric(SimpleTruth)),
         Dial = suppressWarnings(as.numeric(Dial)),
         Dove = suppressWarnings(as.numeric(Dove)),
         Native = suppressWarnings(as.numeric(Native)))

# Clean the data by removing rows with NA values in soap columns
Clean.data <- Raw.data %>% drop_na(SimpleTruth, Dial, Dove, Native)
head(Clean.data)
summary(Clean.data)

# Reshape the data for scatter plot
melted_data <- Clean.data %>%
  pivot_longer(cols = c("Dial", "Dove"), names_to = "Soap_Type", values_to = "Value")

# Create a scatter plot with different colors for each soap type
ggplot(melted_data, aes(x = Soap_Type, y = Value, color = Soap_Type)) +
  geom_point() +
  scale_color_manual(values = c("Dial" = "red", "Dove" = "blue")) +
  theme_minimal() +
  labs(title = "Scatter Plot of Soaps", x = "Soap Type", y = "Value", color = "Soap Type")

# Save the cleaned data to a new CSV file
write_excel_csv(Clean.data, "Cleaned_Dataset_R_2025.csv")



# Load required libraries
library(tidyverse)

# Example dataset (replace this with your actual data)
# Assuming columns for soap sample, Preferred (mosquito count), and Non_Preferred (mosquito count)
mosquito_data <- tibble(
  Soap_Sample = c("Dial", "Native", "Dove", "Simple Truth"),
  Preferred = c(50, 60, 40, 70),       # Replace with actual mosquito counts for preferred soap
  Non_Preferred = c(30, 20, 50, 30)    # Replace with actual mosquito counts for non-preferred soap
)

# Calculate Preference Index
mosquito_data <- mosquito_data %>%
  mutate(
    Total = Preferred + Non_Preferred, # Total mosquitoes tested
    PI = (Preferred - Non_Preferred) / Total # Preference Index formula
  )

# Print the dataset with calculated Preference Index
print(mosquito_data)

# Optional: Save the results to a CSV file
write_csv(mosquito_data, "Mosquito_Preference_Index_Soap.csv")

# Visualization (Optional): Plot Preference Index for each soap sample
ggplot(mosquito_data, aes(x = Soap_Sample, y = PI, fill = Soap_Sample)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Mosquito Preference Index by Soap Sample",
    x = "Soap Sample",
    y = "Preference Index"
  ) +
  theme_minimal()



# Load required libraries
library(tidyverse)
library(randomForest)

# Example dataset (replace this with your actual data)
# Features: Volunteer ID, Body Temperature, CO2 Emission, Sweat Composition, etc.
# Target: Mosquito Attraction Level
data <- tibble(
  Volunteer_ID = 1:4,
  Body_Temp = c(36.7, 37.1, 36.5, 36.9), # Body temperature (Celsius)
  CO2_Emission = c(120, 130, 125, 140),  # CO2 emission levels (ppm)
  Sweat_Composition = c(3.4, 3.9, 3.7, 4.1), # Sweat composition (arbitrary units)
  Attraction_Level = c(80, 90, 85, 95)   # Number of mosquitoes attracted
)

# Split data into training and test sets
training_data <- data %>% filter(Volunteer_ID != 5) # All volunteers except the fifth
test_data <- tibble(
  Volunteer_ID = 5,
  Body_Temp = 36.8, # Replace with actual fifth volunteer's data
  CO2_Emission = 135,
  Sweat_Composition = 3.8
)

# Train a Random Forest model
model <- randomForest(
  Attraction_Level ~ Body_Temp + CO2_Emission + Sweat_Composition,
  data = training_data,
  ntree = 100
)

# Make prediction for the fifth volunteer
predicted_attraction <- predict(model, test_data)

# Print the prediction
print(paste("Predicted mosquito attraction level for the fifth volunteer:", predicted_attraction))




# Visualize the dataset and prediction
library(ggplot2)

# Add the prediction to the test data
test_data <- test_data %>%
  mutate(Attraction_Level = predicted_attraction)

# Combine training and test data for visualization
combined_data <- bind_rows(training_data, test_data)

# Plot results
ggplot(combined_data, aes(x = Volunteer_ID, y = Attraction_Level, color = as.factor(Volunteer_ID))) +
  geom_point(size = 4) +
  geom_line(aes(group = 1), linetype = "dashed") +
  labs(
    title = "Mosquito Attraction Levels by Volunteer",
    x = "Volunteer ID",
    y = "Attraction Level",
    color = "Volunteer"
  ) +
  theme_minimal()



# Train a Random Forest model
training_data <- data
model <- randomForest(
  Attraction_Level ~ Body_Temp + CO2_Emission + Sweat_Composition,
  data = training_data,
  ntree = 100
)

# Make prediction for the fifth volunteer
predicted_attraction <- predict(model, test_data)

# Add the prediction to the test data
test_data <- test_data %>%
  mutate(Attraction_Level = predicted_attraction)

# Combine training and test data for visualization
combined_data <- bind_rows(training_data, test_data)

# Visualization: Scatter plot with a line connecting volunteers
ggplot(combined_data, aes(x = Volunteer_ID, y = Attraction_Level)) +
  geom_point(aes(color = as.factor(Volunteer_ID)), size = 4) +
  geom_line(group = 1, linetype = "solid", color = "blue") + # Ensure line aesthetics are consistent
  labs(
    title = "Mosquito Attraction Levels by Volunteer",
    x = "Volunteer ID",
    y = "Attraction Level",
    color = "Volunteer"
  ) +
  theme_minimal()


# Load required libraries
library(tidyverse)

# Example dataset (replace this with your actual data)
# Includes mosquito attraction levels for different soap types
data <- tibble(
  Soap_Type = c("Dial", "Dial", "Native", "Native", "Dove", "Dove", "Simple Truth", "Simple Truth"),
  Attraction_Level = c(80, 85, 60, 65, 70, 75, 90, 95) # Replace with your actual data
)

# Bar Plot: Average mosquito attraction by soap type
bar_plot <- data %>%
  group_by(Soap_Type) %>%
  summarize(Average_Attraction = mean(Attraction_Level)) %>%
  ggplot(aes(x = Soap_Type, y = Average_Attraction, fill = Soap_Type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Mosquito Attraction by Soap Type",
    x = "Soap Type",
    y = "Average Attraction Level"
  ) +
  theme_minimal()

print(bar_plot)


# Load required libraries
library(tidyverse)

# Example dataset (replace this with your actual data)
# Includes mosquito attraction levels for different soap types
data <- tibble(
  Soap_Type = c("Dial", "Dial", "Native", "Native", "Dove", "Dove", "Simple Truth", "Simple Truth"),
  Attraction_Level = c(-0.5, 0.8, -0.3, 0.6, 0.2, -0.7, 0.9, -0.4) # Simulated values between -1 and +1
)

# Bar Plot: Average mosquito attraction by soap type
bar_plot <- data %>%
  group_by(Soap_Type) %>%
  summarize(Average_Attraction = mean(Attraction_Level)) %>%
  ggplot(aes(x = Soap_Type, y = Average_Attraction, fill = Soap_Type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Mosquito Attraction by Soap Type",
    x = "Soap Type",
    y = "Average Attraction Level"
  ) +
  theme_minimal()

# Print the bar plot
print(bar_plot)


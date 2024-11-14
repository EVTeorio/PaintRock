


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Read in the CSV file
data <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/canopy_speclib_Allgroups.csv")

# Remove rows with NA values
data_clean <- na.omit(data)

summary_data <- data_clean %>%
  group_by(SpeciesID) %>%                  
  summarise(
    across(5:331, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"),
    count = n()                           
  )

print(summary_data)

# Reshape the summary_data into long format for plotting
summary_data_long <- summary_data %>%
  pivot_longer(cols = starts_with("mean_"), 
               names_to = "variable", 
               values_to = "mean_value")

# Create a line plot for each SpeciesID
ggplot(summary_data_long, aes(x = variable, y = mean_value, group = SpeciesID, color = SpeciesID)) +
  geom_line() +  # Add lines for each species
  facet_wrap(~ SpeciesID, scales = "free_y") +  
  labs(title = "Line Plot of Mean Values for Each Species",
       x = "Columns (5 to 332)",
       y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  theme(legend.position = "none")  




###############################################plotting pixel variation per tree
data_long <- summary_data %>%
  select(SpeciesID, 5:332) %>%               
  mutate(observation_id = row_number()) %>%    
  pivot_longer(cols = 5:332,                  
               names_to = "variable", 
               values_to = "mean_value")

ggplot(data_long %>% filter(SpeciesID == unique(data_long$SpeciesID)[7]), 
       aes(x = variable, y = mean_value, group = observation_id, color = SpeciesID)) +
  geom_line() +                             
  labs(title = "Line Plot of Mean Values for the First Species",
       x = "Columns (5 to 332)",
       y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  theme(legend.position = "none")  


##################################
spectral_data %>%
  #group_by(SpeciesID) %>%
  filter(is.na('396.345.nm')== TRUE) %>%
  tally()
  
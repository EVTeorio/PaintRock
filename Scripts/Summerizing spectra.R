


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Read in the CSV file
data <- read.csv(
  "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/masked_speclib.csv")
data <- data[,-1]


#######Summerizing TreeID##########
summary_data <- data %>%
  group_by(TreeID) %>%                  
  summarise(
    across(4:330, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"),
    count = n()                           
  )

print(summary_data)

#plotting
summary_data_long <- summary_data %>%
  pivot_longer(cols = starts_with("mean_"), 
               names_to = "variable", 
               values_to = "mean_value")

ggplot(summary_data_long, aes(x = variable, y = mean_value, group = TreeID, color = TreeID)) +
  geom_line() +  # Add lines for each species
  facet_wrap(~ TreeID, scales = "free_y") +  
  labs(title = "Line Plot of Mean Values for Each Individual",
       x = "Bands)",
       y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  theme(legend.position = "none")  

##plotting pixel variation per tree
data_long <- summary_data %>%
  select(TreeID, 2:329) %>%               
  mutate(observation_id = row_number()) %>%    
  pivot_longer(cols = 2:329,                  
               names_to = "variable", 
               values_to = "mean_value")

ggplot(summary_data %>% filter(TreeID == unique(data_long$TreeID)[2]), #Change which TreeID to look at
       aes(x = variable, y = mean_value, group = observation_id, color = TreeID)) +
  geom_line() +                             
  labs(title = "Line Plot of Mean Values for the First Species",
       x = "Columns (2 to 329)",
       y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  theme(legend.position = "none")  



################# Summerizing SpeciesID##########
summary_data <- data_clean %>%
  group_by(SpeciesID) %>%                  
  summarise(
    across(4:330, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"),
    count = n()                           
  )

print(summary_data)

# Reshape the summary_data into long format for plotting
summary_data_long <- summary_data %>%
  pivot_longer(cols = starts_with("mean_"), 
               names_to = "variable", 
               values_to = "mean_value")

# Create a line plot for each TreeID
ggplot(summary_data_long, aes(x = variable, y = mean_value, group = SpeciesID, color = SpeciesID)) +
  geom_line() +  # Add lines for each species
  facet_wrap(~ SpeciesID, scales = "free_y") +  
  labs(title = "Line Plot of Mean Values for Each Species",
       x = "Bands",
       y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  theme(legend.position = "none")  


##################################
spectral_data %>%
  #group_by(SpeciesID) %>%
  filter(is.na('396.345.nm')== TRUE) %>%
  tally()
  
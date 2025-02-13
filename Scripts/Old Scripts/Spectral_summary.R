
library(ggplot2)
library(dplyr)

# Read in the CSV file
data <- read.csv(
  "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/masked_speclib.csv")
data <- data[,-1]

Testdata <- filter(data, Group == 1)


# Summarize values by TreeID and SpeciesID
summary_data <- Testdata %>%
  group_by(TreeID, SpeciesID) %>%
  summarise(
    across(4:329, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"),  
    count = n()  
  )



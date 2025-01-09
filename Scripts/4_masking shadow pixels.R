

# Load necessary libraries
library(dplyr)

# Read in the CSV file
data <- read.csv(
  "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/canopy_speclib.csv")

# Remove rows with NA values
data_clean <- na.omit(data)

# Filter rows 
filtered_data <- data_clean[data_clean$X790.821.nm >= 0.4, ]

write.csv(filtered_data,"C:/Users/PaintRock/Documents/Data processing/Hyperspectral/masked_speclib.csv")

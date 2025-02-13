
library(vegan)
library(dplyr)
library(corrplot)
library(factoextra)
library(ggplot2)
library(proxy)


#read in Vegitation indices calculation
tree_VI <-read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Vegindex_calc.csv")
tree_VI <- tree_VI[,-1]
tree_VI <- tree_VI[,-4]
tree_VI <- tree_VI[,-7]
# Define a list of SpeciesID values to remove
species_to_remove <- c("LITU", "DIVI5","AEFL","FAGR","JUNI","LIST2","QUNI","QUSH")
tree_VI <- tree_VI %>% dplyr::filter(!SpeciesID %in% species_to_remove)

str(tree_VI)


#creating a balanced sample
# Count the number of observations per species
species_counts <- tree_VI %>%
  group_by(SpeciesID) %>%
  summarise(n = n())

# Sample the same number of observations (min_obs) from each species
tree_VI <- tree_VI %>%
  group_by(SpeciesID) %>%
  sample_n(150)

# Data preparation: removing unnecessary columns (Group, SpeciesID, TreeID)
data <- tree_VI %>%
  dplyr::select(-Group, -SpeciesID, -TreeID)  # Remove non-numeric columns


# 1. Correlation analysis between SpeciesID and TreeID
# Calculate the correlation between numerical columns and TreeID, grouped by SpeciesID
cor_data <- tree_VI %>% 
  group_by(SpeciesID) %>%  # Group by SpeciesID
  summarise(across(where(is.numeric), ~cor(.x, TreeID, use = "complete.obs"), .names = "cor_{col}"))


# Display the correlation results
print(cor_data)

# 2. ANOVA with Adonis2
# Select only numeric columns from 'data'
numeric_data <- data[,-1]

# Creating a dissimilarity matrix (e.g., using Bray-Curtis distance)
dist_matrix <- vegdist(numeric_data, method = "euclidean")

hist(dist_matrix)

# Conduct the Adonis2 test to examine the relationship between SpeciesID and vegetation indices
adonis_result <- adonis2(dist_matrix ~ as.factor(SpeciesID), data = tree_VI, permutations = 500)
print(adonis_result)

# 3. PCA (Principal Component Analysis)
# Perform PCA on the numeric vegetation indices
pca_result <- prcomp(numeric_data, scale. = TRUE)

# 4. Visualizing PCA with ggplot2
# Create a data frame with PCA scores and species information
pca_scores <- data.frame(pca_result$x, SpeciesID = tree_VI$SpeciesID)

# PCA plot with species ID as colors
ggplot(pca_scores, aes(x = PC1, y = PC2, color = SpeciesID)) +
  geom_point() +
  theme_minimal() +
  labs(title = "PCA of Vegetation Indices by Species", x = "Principal Component 1", y = "Principal Component 2") +
  theme(legend.position = "right")

# 5. Additional visualizations - Correlation heatmap
# Plot a correlation matrix for the vegetation indices
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.6, addCoef.col = "black")

# 6. Exploring vegetation index distributions by species
# Create boxplots for a selection of vegetation indices by species
# Example: Boxplot for the NDVI index across species
ggplot(tree_VI, aes(x = SpeciesID, y = NDVI)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "NDVI Distribution Across Species", x = "Species ID", y = "NDVI")

# You can repeat the boxplot step for other vegetation indices of interest.
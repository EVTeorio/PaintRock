
# Load necessary libraries
library(tidyverse)
library(caret)
library(corrplot)
library(ggplot2)
library(randomForest)
library(tidyr)
library(vegan)
library(reshape2)

# Load dataset
data <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/masked_speclib.csv")
data <- data[,-1] 
data <- data %>% filter(TreeID != 10358)

colnames(data) <- gsub("^X|\\.nm$", "", colnames(data))  # Remove "X" prefix and ".nm" suffix
colnames(data) <- gsub("\\.", ".", colnames(data))       # Ensure periods remain as decimal separators

# Convert band columns to numeric if needed (for proper sorting)
band_columns <- grep("^[0-9]+\\.[0-9]+$", colnames(data), value = TRUE)
numeric_band_names <- as.numeric(band_columns)  # Convert band column names to numeric
names(numeric_band_names) <- band_columns  # Keep a mapping of original names

# Rename the columns in the dataset to use the numeric values
colnames(data)[match(band_columns, colnames(data))] <- numeric_band_names

# Convert TreeID and SpeciesID into factors (assuming they are categorical)
data$TreeID <- as.factor(data$TreeID)
data$SpeciesID <- as.factor(data$SpeciesID)
data$Group <- as.factor(data$Group)

# Extract the band columns (reflectance data)
band_columns <- grep("^[0-9]+\\.[0-9]+$", colnames(data), value = TRUE)  # Extract numeric band columns

# Split data based on Group column (1 for training, 2 for testing)
train_data <- data %>% filter(Group == 1)
test_data <- data %>% filter(Group == 2)

#Balance Sample 
train_data_bal <- train_data %>% group_by(SpeciesID, TreeID) %>%
  slice_sample(n = 50) #%>% tally()

#Balance Sample 
test_data_bal <- test_data %>% group_by(SpeciesID, TreeID) %>%
  slice_sample(n = 50) #%>% tally

# Extract the reflectance matrix for training and testing
X_train <- as.matrix(train_data_bal[, band_columns])
X_test <- as.matrix(test_data_bal[, band_columns])

# Perform PCA on the training data
pca_model <- prcomp(X_train, scale. = TRUE)

# Calculate explained variance
explained_variance <- pca_model$sdev^2 / sum(pca_model$sdev^2)
cumulative_variance <- cumsum(explained_variance)

# Plot explained variance for each component
explained_variance_df <- data.frame(
  Component = 1:length(explained_variance),
  Variance = explained_variance,
  CumulativeVariance = cumulative_variance
)

ggplot(explained_variance_df, aes(x = Component)) +
  geom_bar(aes(y = Variance), stat = "identity", fill = "skyblue", alpha = 0.8) +
  labs(
    title = "Explained Variance by PCA Components",
    x = "Principal Component",
    y = "Variance Explained"
  ) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Variance"))
PCA_modelscores <- scores(pca_model)
plot(PCA_modelscores[,2],PCA_modelscores[,3],col = train_data_bal$SpeciesID)


# Prepare PCA scores data frame
PCA_modelscores <- data.frame(
  PC1 = pca_model$x[, 1],
  PC2 = pca_model$x[, 2],
  PC3 = pca_model$x[, 3],
  SpeciesID = train_data_bal$SpeciesID
)

# Plot PC1 vs. PC2 without legend
pca_plot_pc1_pc2 <- ggplot(PCA_modelscores, aes(x = PC1, y = PC2, color = SpeciesID)) +
  geom_point(alpha = 0.8, size = 6) +
  labs(
    title = "PCA Plot (PC1 vs PC2)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Extract the legend for PC1 vs. PC2
legend_pc1_pc2 <- ggplot(PCA_modelscores, aes(x = PC1, y = PC2, color = SpeciesID)) +
  geom_point(alpha = 0.8, size = 6) +
  labs(color = "Species ID") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# Plot PC1 vs. PC3 without legend
pca_plot_pc1_pc3 <- ggplot(PCA_modelscores, aes(x = PC1, y = PC3, color = SpeciesID)) +
  geom_point(alpha = 0.8, size = 6) +
  labs(
    title = "PCA Plot (PC1 vs PC3)",
    x = "Principal Component 1",
    y = "Principal Component 3"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Extract the legend for PC1 vs. PC3
legend_pc1_pc3 <- ggplot(PCA_modelscores, aes(x = PC1, y = PC3, color = SpeciesID)) +
  geom_point(alpha = 0.8, size = 6) +
  labs(color = "Species ID") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# Display all plots separately
print(pca_plot_pc1_pc2)  
print(pca_plot_pc1_pc3)  
print(legend_pc1_pc2)    
print(legend_pc1_pc3)    


#biplot maybe

# Reduce dimensionality using the selected principal components
num_components <- 3  # Retain a specified number of components (adjust as needed)
X_train_pca <- pca_model$x[, 1:num_components]  # Training data in PCA space
X_test_pca <- predict(pca_model, newdata = X_test)[, 1:num_components]  # Project test data

# Update train_data and test_data with PCA components
train_data_pca <- cbind(data.frame(TreeID = train_data_bal$TreeID, SpeciesID = train_data_bal$SpeciesID), X_train_pca)
test_data_pca <- cbind(data.frame(TreeID = test_data_bal$TreeID, SpeciesID = test_data_bal$SpeciesID), X_test_pca)
################permanova#################################
##########################################################
# Perform PERMANOVA on PCA-transformed training data
permanova_result_train <- adonis2(
  X_train_pca ~ SpeciesID,
  data = train_data_bal,
  permutations = 100,
  method = "euclidean"
)
print("PERMANOVA results for training data:")
print(permanova_result_train)

# Perform PERMANOVA on PCA-transformed testing data
permanova_result_test <- adonis2(
  X_test_pca ~ SpeciesID,
  data = test_data_bal,
  permutations = 100,
  method = "euclidean"
)
print("PERMANOVA results for testing data:")
print(permanova_result_test)

###################################################
# Get PCA loadings
pca_loadings <- as.data.frame(pca_model$rotation)  # Extract loadings matrix
pca_loadings$Band <- rownames(pca_loadings)       # Add a column for the band names
num_pcs_to_plot <- 3  # Adjust based on the number of PCs of interest
top_loadings <- pca_loadings[, c("Band", paste0("PC", 1:num_pcs_to_plot))]

# Convert to long format for easier plotting
top_loadings_long <- pivot_longer(
  top_loadings,
  cols = starts_with("PC"),
  names_to = "PrincipalComponent",
  values_to = "Loading"
)

# Plot loadings for each principal component
ggplot(top_loadings_long, aes(x = Band, y = Loading, fill = PrincipalComponent)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(
    title = "PCA Loadings for Spectral Bands",
    x = "Spectral Band",
    y = "PCA Loading"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  ) +
  scale_fill_brewer(palette = "Set2")

# Identify top contributing spectral bands numerically
top_contributors <- top_loadings_long %>%
  group_by(PrincipalComponent) %>%
  arrange(desc(abs(Loading))) %>%
  slice_head(n = 10)  # Adjust the number to display the top N bands

print(top_contributors)

#########Top bands for each species##########
# Prepare PCA scores and species labels
PCA_modelscores <- data.frame(
  PC1 = pca_model$x[, 1],
  PC2 = pca_model$x[, 2],
  PC3 = pca_model$x[, 3],
  SpeciesID = train_data_bal$SpeciesID
)

# Get the PCA loadings for each spectral band (from pca_model)
pca_loadings <- as.data.frame(pca_model$rotation)
pca_loadings$Band <- rownames(pca_loadings)  # Add the band names

# Now, we will compute the mean PCA scores for each species
species_pca_means <- PCA_modelscores %>%
  group_by(SpeciesID) %>%
  summarise(across(starts_with("PC"), mean))

# For each species, we want to identify which bands contribute most to the differentiation
# First, we will look at how the loadings vary for each PC

# Select the first few principal components 
num_pcs <- 3
top_loadings_species <- pca_loadings[, c("Band", paste0("PC", 1:num_pcs))]


# Melt the loadings data for easier plotting
top_loadings_melted <- melt(top_loadings_species, id.vars = "Band")

# Plot the loadings for each band and principal component
ggplot(top_loadings_melted, aes(x = Band, y = variable, fill = value)) +
  geom_tile() +
  labs(
    title = "PCA Loadings by Spectral Band and Principal Component",
    x = "Spectral Band",
    y = "Principal Component",
    fill = "Loading"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.position = "top"
  )

# Now, let's identify the bands that contribute most to each species' differentiation
# Find which bands have the highest loadings for each species label in the PCA components
top_contributors_per_species <- lapply(unique(PCA_modelscores$SpeciesID), function(species) {
  species_data <- PCA_modelscores[PCA_modelscores$SpeciesID == species, ]
  species_pca_means <- colMeans(species_data[, c("PC1", "PC2", "PC3")])
  
  # Rank the bands based on their loadings for each principal component
  top_loading_bands <- apply(pca_loadings[, 1:num_pcs], 2, function(x) {
    ranked_bands <- pca_loadings$Band[order(abs(x), decreasing = TRUE)]
    ranked_bands[1:5]  # Get the top 5 contributing bands for this PC
  })
  
  return(data.frame(SpeciesID = species, TopBandsPC1 = top_loading_bands[, 1], TopBandsPC2 = top_loading_bands[, 2], TopBandsPC3 = top_loading_bands[, 3]))
})

# Combine the results into one dataframe
top_contributors_df <- do.call(rbind, top_contributors_per_species)

# Print the top contributors
print(top_contributors_df)


##########################################
# Random Forest for TreeID
tree_model_rf <- randomForest(TreeID ~ ., data = train_data_pca, importance = TRUE, ntree = 100)
summary(tree_model_rf)

# Random Forest for SpeciesID
species_model_rf <- randomForest(SpeciesID ~ ., data = train_data_pca, importance = TRUE, ntree = 100)
summary(species_model_rf)

##########################################
# Predict on test data and evaluate accuracy
tree_predictions <- predict(tree_model_rf, newdata = test_data_pca)
species_predictions <- predict(species_model_rf, newdata = test_data_pca)

tree_accuracy <- mean(tree_predictions == test_data_pca$TreeID)
species_accuracy <- mean(species_predictions == test_data_pca$SpeciesID)

##########################################
# Confusion Matrix for TreeID
tree_confusion_matrix <- table(Predicted = tree_predictions, Actual = test_data_pca$TreeID)
tree_label_accuracy <- diag(tree_confusion_matrix) / rowSums(tree_confusion_matrix)
tree_accuracy_df <- data.frame(Label = names(tree_label_accuracy), Accuracy = tree_label_accuracy)

# Confusion Matrix for SpeciesID
species_confusion_matrix <- table(Predicted = species_predictions, Actual = test_data_pca$SpeciesID)
species_label_accuracy <- diag(species_confusion_matrix) / rowSums(species_confusion_matrix)
species_accuracy_df <- data.frame(Label = names(species_label_accuracy), Accuracy = species_label_accuracy)

##########################################
# Plot Accuracy for TreeID
ggplot(tree_accuracy_df, aes(x = Label, y = Accuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Per-Class Accuracy for TreeID", x = "Label", y = "Accuracy") +
  theme_minimal()

# Plot Accuracy for SpeciesID
ggplot(species_accuracy_df, aes(x = Label, y = Accuracy)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(title = "Per-Class Accuracy for SpeciesID", x = "Label", y = "Accuracy") +
  theme_minimal()
##########################################
# Metrics for TreeID
tree_cm <- confusionMatrix(tree_predictions, test_data_pca$TreeID)

# Overall accuracy
tree_overall_accuracy <- tree_cm$overall["Accuracy"]

# Kappa
tree_kappa <- tree_cm$overall["Kappa"]

# F1 Score (average)
tree_f1_score <- mean(tree_cm$byClass[, "F1"])

cat("TreeID Metrics:\n")
cat("Overall Accuracy:", tree_overall_accuracy, "\n")
cat("Kappa:", tree_kappa, "\n")
cat("F1 Score (average):", tree_f1_score, "\n")

# Metrics for SpeciesID
species_cm <- confusionMatrix(species_predictions, test_data_pca$SpeciesID)

# Overall accuracy
species_overall_accuracy <- species_cm$overall["Accuracy"]

# Kappa
species_kappa <- species_cm$overall["Kappa"]

# F1 Score (average)
species_f1_score <- mean(species_cm$byClass[, "F1"])

cat("SpeciesID Metrics:\n")
cat("Overall Accuracy:", species_overall_accuracy, "\n")
cat("Kappa:", species_kappa, "\n")
cat("F1 Score (average):", species_f1_score, "\n")
##########################################
# Save Confusion Matrices to CSV
write.csv(as.data.frame(tree_confusion_matrix), "C:/Users/PaintRock/Documents/Data Analysis/Hyperspectral/TreeID_confusion_matrix.csv")
write.csv(as.data.frame(species_confusion_matrix), "C:/Users/PaintRock/Documents/Data Analysis/Hyperspectral/SpeciesID_confusion_matrix.csv")

################me doing craziness because i am sleep deprived#####################3

# Extract the first 5 rows of top_contributors_df and get the bands for PC1, PC2, and PC3
top_bands <- top_contributors_df[1:5, ] %>%
  pivot_longer(cols = starts_with("TopBands"), names_to = "PC", values_to = "Band")

# Ensure the Band names in `top_bands` match the format of `avg_reflectance_species_long$Band`
top_bands$Band <- paste0("avg_", top_bands$Band)

# Add vertical lines for these bands in the plot
ggplot(avg_reflectance_species_long, aes(x = Band, y = Reflectance, color = SpeciesID, group = SpeciesID)) +
  geom_smooth(se = FALSE, span = .1) +
  theme_minimal() +
  labs(
    title = "Average Reflectance Across Bands for Each SpeciesID",
    x = "Band",
    y = "Reflectance"
  ) +
  scale_x_discrete(
    breaks = avg_reflectance_species_long$Band[seq(1, length(avg_reflectance_species_long$Band), by = 20)]
  ) +  # Show every 20th label
  geom_vline(
    data = top_bands,
    aes(xintercept = Band, linetype = PC),
    color = "black",
    show.legend = TRUE
  ) +
  geom_text(
    data = top_bands,
    aes(
      x = Band,
      y = 1,
      label = PC
    ),
    angle = 90,
    hjust = -0.2,
    color = "black"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = .9),
    legend.position = "none"
  )+
  coord_cartesian(ylim = c(0, .9))

###########################################3

cumulative_variance <- cumsum(explained_variance[1:3])
print(cumulative_variance)



# Load necessary libraries
library(tidyverse)
library(caret)
library(corrplot)
library(ggplot2)
library(randomForest)
library(tidyr)

# Load dataset
data <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/masked_speclib.csv")
data <- data[,-1]  # Remove the first column
str(data)

colnames(data) <- gsub("^X|\\.nm$", "", colnames(data))  # Remove "X" prefix and ".nm" suffix
colnames(data) <- gsub("\\.", ".", colnames(data))       # Ensure periods remain as decimal separators
str(data)  # Check updated column names

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

# Extract band columns (reflectance data)
band_columns <- grep("^[0-9]+\\.[0-9]+$", colnames(data), value = TRUE)

# Verify band_columns contains expected column names
print(band_columns)

# Convert band columns to numeric for sorting
numeric_band_names <- as.numeric(band_columns)

# Map original band names to numeric values
names(numeric_band_names) <- band_columns

# Rename columns in the dataset to numeric equivalents
colnames(data)[match(band_columns, colnames(data))] <- numeric_band_names

# Verify column names after renaming
print(colnames(data))

# Extract band columns again after renaming
band_columns <- grep("^[0-9]+\\.[0-9]+$", colnames(data), value = TRUE)
band_columns <- as.numeric(band_columns)

# Split data based on Group column (1 for training, 2 for testing)
train_data <- data %>% filter(Group == 1)
test_data <- data %>% filter(Group == 2)

# Extract the reflectance matrix for training and testing
X_train <- train_data[, band_columns]
X_test <- test_data[, band_columns]

# Correlation matrix to check for collinearity among bands
cor_matrix <- cor(X_train)
cor_summary <- summary(cor_matrix)

##################################################
#################################################
# Step 2: Remove the diagonal (set it to NA)
diag(cor_matrix) <- NA

# Step 3: Calculate summary statistics (mean, 50th percentile, 75th percentile) for each band’s correlations
cor_summary <- apply(cor_matrix, 1, function(row) {
  c(mean = mean(row, na.rm = TRUE),
    p50 = quantile(row, 0.50, na.rm = TRUE),
    p75 = quantile(row, 0.75, na.rm = TRUE))
})

# Transpose to convert to a data frame
cor_summary_df <- as.data.frame(t(cor_summary))
cor_summary_df$Variable <- rownames(cor_summary_df)
rownames(cor_summary_df) <- NULL


# Reshape the data for ggplot2
cor_summary_long <- pivot_longer(cor_summary_df, cols = c("mean"),
                                 names_to = "Statistic", values_to = "Value")

# Step 4: Plot the data with shaded percentiles for the mean
ggplot() +
  # Shaded ribbons for percentiles (mean to 50th, 75th percentile)
  geom_ribbon(data = cor_summary_df, aes(x = as.factor(Variable), ymin = `p50.50%`, ymax = `p75.75%`), fill = "lightgrey", alpha = 0.5) +  # 50th-75th percentile shading
  geom_ribbon(data = cor_summary_df, aes(x = as.factor(Variable), ymin = mean, ymax = `p50.50%`), fill = "darkgrey", alpha = 0.5) +  # Mean-50th percentile shading
  # Plotting the mean as a line
  geom_line(data = cor_summary_long, aes(x = as.factor(Variable), y = Value, color = Statistic, group = Statistic), size = 1.2) +
  geom_point(data = cor_summary_long, aes(x = as.factor(Variable), y = Value, color = Statistic), size = 2) +
  # Adjust x-axis to show every 15th label
  scale_x_discrete(breaks = cor_summary_df$Variable[seq(1, nrow(cor_summary_df), by = 15)]) +
  labs(title = "Mean Correlation Values with Percentiles",
       x = "Variables", y = "Correlation Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##########################################
# Random Forest for TreeID (training model)
tree_model_rf <- randomForest(TreeID ~ ., data = train_data[, c("TreeID", band_columns)], importance = TRUE, ntree = 100)
summary(tree_model_rf)

# Random Forest for SpeciesID (training model)
species_model_rf <- randomForest(SpeciesID ~ ., data = train_data_bal[, c("SpeciesID", band_columns)], importance = TRUE, ntree = 100)
summary(species_model_rf)

# Plot important features (bands) for TreeID model
importance_tree <- importance(tree_model_rf)
important_bands_tree <- data.frame(Band = rownames(importance_tree), Importance = importance_tree[, 1])

# Plot important features (bands) for SpeciesID model
importance_species <- importance(species_model_rf)
important_bands_species <- data.frame(Band = rownames(importance_species), Importance = importance_species[, 1])
###################################################
############################################################
# Add a new column to each dataset indicating the group (TreeID or SpeciesID)
important_bands_tree <- important_bands_tree %>% mutate(Group = "TreeID")
important_bands_species <- important_bands_species %>% mutate(Group = "SpeciesID")

# Combine the two datasets
combined_data <- bind_rows(important_bands_tree, important_bands_species)

# Reorder the 'Band' factor alphabetically (or numerically if Band is numeric)
combined_data$Band <- factor(combined_data$Band, levels = sort(unique(combined_data$Band)))

# Plot the combined data with smoothing and every 15th label on the x-axis
ggplot(combined_data, aes(x = Band, y = Importance, color = Group, group = Group)) +
  geom_smooth(method = "loess", se = FALSE) +  # Add a smoothing line
  geom_point() +
  theme_minimal() +
  labs(title = "Important Bands for TreeID and SpeciesID", x = "Band", y = "Importance") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks = combined_data$Band[seq(1, nrow(combined_data), by = 15)])
################################################

# Predict on test data (for both models) and evaluate accuracy
tree_predictions <- predict(tree_model_rf, newdata = test_data)
species_predictions <- predict(species_model_rf, newdata = test_data)

# Evaluate model performance (e.g., accuracy)
tree_accuracy <- mean(tree_predictions == test_data$TreeID)
species_accuracy <- mean(species_predictions == test_data$SpeciesID)

########################################################################################################################
############################################################################################################
# Plot variability in reflectance for TreeID (average reflectance for each TreeID)
avg_reflectance_tree <- train_data %>%
  group_by(TreeID) %>%
  summarise(across(starts_with("X"), mean, .names = "avg_{col}"))

# Plot average reflectance across bands for each TreeID
avg_reflectance_tree_long <- avg_reflectance_tree %>%
  pivot_longer(cols = starts_with("avg_X"), names_to = "Band", values_to = "Reflectance")

# Reorder 'Band' factor based on unique values and ensure it is ordered
avg_reflectance_tree_long$Band <- factor(avg_reflectance_tree_long$Band, levels = unique(avg_reflectance_tree_long$Band))

# Plot average reflectance across bands for each TreeID with every 20th band label displayed
ggplot(avg_reflectance_tree_long, aes(x = Band, y = Reflectance, color = TreeID, group = TreeID)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Average Reflectance Across Bands for Each TreeID", x = "Band", y = "Reflectance") +
  scale_x_discrete(breaks = avg_reflectance_tree_long$Band[seq(1, length(avg_reflectance_tree_long$Band), by = 20)]) +  # Show every 20th label
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Plot variability in reflectance for SpeciesID
avg_reflectance_species <- train_data %>%
  group_by(SpeciesID) %>%
  summarise(across(starts_with(("^[0-9]")), mean, .names = "avg_{col}"))

# Plot variability in reflectance for SpeciesID (average reflectance for each SpeciesID)
avg_reflectance_species_long <- avg_reflectance_species %>%
  pivot_longer(cols = starts_with("avg"), names_to = "Band", values_to = "Reflectance")

# Reorder 'Band' factor based on unique values and ensure it is ordered
avg_reflectance_species_long$Band <- factor(avg_reflectance_species_long$Band, levels = unique(avg_reflectance_species_long$Band))

# Plot average reflectance across bands for each SpeciesID with every 20th band label displayed
ggplot(avg_reflectance_species_long, aes(x = Band, y = Reflectance, color = SpeciesID, group = SpeciesID)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Average Reflectance Across Bands for Each SpeciesID", x = "Band", y = "Reflectance") +
  scale_x_discrete(breaks = avg_reflectance_species_long$Band[seq(1, length(avg_reflectance_species_long$Band), by = 20)]) +  # Show every 20th label
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

##########################################################################################################
##########################################################################################################
# Create confusion matrix for TreeID
tree_confusion_matrix <- table(Predicted = tree_predictions, Actual = test_data$TreeID)

# Convert the confusion matrix to a data frame
tree_confusion_df <- as.data.frame(tree_confusion_matrix)

# Create a table with Predicted values as columns and Actual values as rows
tree_confusion_table <- reshape(tree_confusion_df, 
                                idvar = "Actual", 
                                timevar = "Predicted", 
                                direction = "wide")

# Rename columns for clarity (optional)
colnames(tree_confusion_table) <- gsub("Freq.", "", colnames(tree_confusion_table))

# Calculate accuracy for each actual label
tree_label_accuracy <- diag(tree_confusion_matrix) / rowSums(tree_confusion_matrix)

# Convert to data frame for better visualization
tree_accuracy_df <- data.frame(Label = names(tree_label_accuracy), Accuracy = tree_label_accuracy)

# Plot TreeID Accuracy
ggplot(tree_accuracy_df, aes(x = Label, y = Accuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Per-Class Accuracy for TreeID", x = "Label", y = "Accuracy") +
  theme_minimal()

write.csv(tree_confusion_table,"C:/Users/PaintRock/Documents/Data Analysis/Hyperspectral/TreeID_confusion_matrix.csv")

# Create confusion matrix for SpeciesID
species_confusion_matrix <- table(Predicted = species_predictions, Actual = test_data$SpeciesID)

# Convert the confusion matrix to a data frame
species_confusion_df <- as.data.frame(species_confusion_matrix)

# Create a table with Predicted values as columns and Actual values as rows
species_confusion_table <- reshape(species_confusion_df, 
                                   idvar = "Actual", 
                                   timevar = "Predicted", 
                                   direction = "wide")

# Rename columns for clarity (optional)
colnames(species_confusion_table) <- gsub("Freq.", "", colnames(species_confusion_table))

# Calculate accuracy for each actual label
species_label_accuracy <- diag(species_confusion_matrix) / rowSums(species_confusion_matrix)

# Convert to data frame for better visualization
species_accuracy_df <- data.frame(Label = names(species_label_accuracy), Accuracy = species_label_accuracy)

# Plot SpeciesID Accuracy
ggplot(species_accuracy_df, aes(x = Label, y = Accuracy)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(title = "Per-Class Accuracy for SpeciesID", x = "Label", y = "Accuracy") +
  theme_minimal()

write.csv(species_confusion_table,"C:/Users/PaintRock/Documents/Data Analysis/Hyperspectral/SpeciesID_confusion_matrix.csv")




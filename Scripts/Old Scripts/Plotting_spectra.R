


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)
library(grDevices)

# Check the column names to make sure the correct pattern is used
col<-colnames(paintrock_spectra_df)


# Now adjust the code to pivot the data
paintrock_spectra_df_tall <- paintrock_spectra_df %>%
  # Rename columns to clean up names (remove non-numeric characters, e.g., "mean_X")
  rename_with(~ gsub("[^0-9]", "", .), starts_with("mean_X")) %>%
  # Transform the dataset to long format, ensuring the correct columns are selected
  pivot_longer(cols = starts_with("mean_X"), names_to = "Wavelength", values_to = "Reflectance") %>%
  mutate(Wavelength = as.numeric(Wavelength)) %>% 
  # Filter out rows where Wavelength could not be converted to numeric
  filter(!is.na(Wavelength)) %>%
  # Group by SpeciesID and Wavelength to calculate the median and variation statistics
  group_by(SpeciesID) %>%
  mutate(sample_size = n(),
         SpeciesID_wN = glue("{SpeciesID} (n={sample_size})")) %>%
  ungroup() %>%
  group_by(SpeciesID, SpeciesID_wN, Wavelength) %>%
  summarise(
    Median_Reflectance = median(Reflectance, na.rm = TRUE),
    Max_Reflectance = max(Reflectance, na.rm = TRUE),
    Min_Reflectance = min(Reflectance, na.rm = TRUE),
    Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875, na.rm = TRUE),
    Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125, na.rm = TRUE),
    Upper_Reflectance = quantile(Reflectance, probs = 0.95, na.rm = TRUE),
    Lower_Reflectance = quantile(Reflectance, probs = 0.05, na.rm = TRUE)
  ) %>%
  # Convert Wavelength to numeric and arrange by wavelength
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  arrange(Wavelength) %>%
  as.data.frame()


# Color palette for spectral bands
color <- grDevices::hcl.colors(6, palette = "Spectral", rev = TRUE)

# Generate first plot: Tree species spectral profiles with variation
jpeg("output/Tree_species_spectral_profiles.jpg", height = 9000, width = 6000, res = 350)

ggplot(paintrock_spectra_df_tall, aes(Wavelength, Median_Reflectance)) +
  # Annotate Sentinel-2 bands
  annotate("rect", xmin = 492.4 - (66 / 2), xmax = 492.4 + (66 / 2), ymin = 0, ymax = 100, alpha = .7, color = color[2], fill = color[2]) +  # Band 2
  annotate("rect", xmin = 559.8 - (36 / 2), xmax = 559.8 + (36 / 2), ymin = 0, ymax = 100, alpha = .7, color = color[3], fill = color[3]) +  # Band 3
  annotate("rect", xmin = 664.6 - (31 / 2), xmax = 664.6 + (31 / 2), ymin = 0, ymax = 100, alpha = .7, color = color[4], fill = color[4]) +  # Band 4
  annotate("rect", xmin = 704.1 - (15 / 2), xmax = 704.1 + (15 / 2), ymin = 0, ymax = 100, alpha = .7, color = color[5], fill = color[5]) +  # Band 5
  annotate("rect", xmin = 740.5 - (15 / 2), xmax = 740.5 + (15 / 2), ymin = 0, ymax = 100, alpha = .7, color = color[6], fill = color[6]) +  # Band 6
  annotate("rect", xmin = 782.8 - (20 / 2), xmax = 782.8 + (20 / 2), ymin = 0, ymax = 100, alpha = .2) +  # Band 7
  annotate("rect", xmin = 864 - (21 / 2), xmax = 864 + (21 / 2), ymin = 0, ymax = 100, alpha = .2) +  # Band 8
  annotate("rect", xmin = 945.1 - (20 / 2), xmax = 945.1 + (20 / 2), ymin = 0, ymax = 100, alpha = .2) +  # Band 9
  annotate("rect", xmin = 1373.5 - (31 / 2), xmax = 1373.5 + (31 / 2), ymin = 0, ymax = 100, alpha = .2) +  # Band 10
  annotate("rect", xmin = 1613.7 - (91 / 2), xmax = 1613.7 + (91 / 2), ymin = 0, ymax = 100, alpha = .2) +  # Band 11
  annotate("rect", xmin = 2202.4 - (175 / 2), xmax = 2202.4 + (175), ymin = 0, ymax = 100, alpha = .2) +  # Band 12
  geom_line(aes(color = "black"), size = 1.5) +  # Median reflectance line
  geom_ribbon(aes(ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance), alpha = 0.3) +  # 12.5th to 87.5th percentile range
  geom_ribbon(aes(ymin = Lower_Reflectance, ymax = Upper_Reflectance), alpha = 0.2) +  # 5th to 95th percentile range
  facet_wrap(vars(SpeciesID_wN), scales = "fixed", ncol = 4) +  # Facet by SpeciesID
  labs(x = "Wavelength (nm)", y = "Reflectance") +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    legend.position = "none",
    title = element_text(size = 15),
    strip.text = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 15)
  ) +
  scale_color_grey()

dev.off()

# Generate second plot: Median reflectance for each species with individual colors
tree_list <- data.frame(SpeciesID = unique(paintrock_spectra_df$SpeciesID)) %>%
  mutate(Color = grDevices::hcl.colors(nrow(.), palette = "Spectral", rev = TRUE))

jpeg("output/Tree_species_median_spectral_profiles.jpg", height = 2000, width = 2500, res = 250)

median_df <- paintrock_spectra_df_tall %>%
  inner_join(tree_list, by = "SpeciesID", keep = FALSE)

ggplot(median_df, aes(Wavelength, Median_Reflectance, color = Color)) +
  geom_line(aes(color = SpeciesID), size = 1.5) +
  labs(title = "Median Reflectance by Tree Species", x = "Wavelength (nm)", y = "Reflectance") +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    legend.position = "right",
    title = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45)
  ) +
  scale_color_identity()  # Use the color specified in the 'Color' column

dev.off()



# Load necessary packages
library(data.table)  
library(dplyr)       
library(RStoolbox)
library(vegan)
library(Polychrome)
library(glue)

paintrock_spectra_df<- summary_data

paintrock_spectra_df_median_sp <- paintrock_spectra_df %>%
  rename_with(~ gsub("[^0-9]", "", .), starts_with("mean_X")) %>%
  group_by(SpeciesID) %>%
  pivot_longer(cols = -SpeciesID, names_to = "Wavelength", values_to = "Reflectance") %>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  filter(!is.na(Wavelength)) %>%
  group_by(SpeciesID, Wavelength) %>%
  summarise(Reflectance = median(Reflectance, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = SpeciesID, values_from = Reflectance) %>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  arrange(Wavelength)

writeSLI(paintrock_spectra_df_median_sp,"C:/Users/PaintRock/Documents/Data processing/Hyperspectral/canopy_spectra/speclib.sli", wavl.units = "Nanometers")


























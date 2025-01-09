
# Read in the CSV file
data <- read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/masked_speclib.csv")

#only pixels of the first 5 trees
unique_TreeID <- unique(data$TreeID)[1:5]
subset_data <- data[data$TreeID %in% unique_TreeID, ]

# Write the subsetted data to a new CSV file
write.csv(summary_data, "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/masked_subset.csv", row.names = FALSE)







##########################################################################################

# Load necessary libraries
library(data.table)  # for fread
library(dplyr)

# Function to create the .sli file
create_sli_library <- function(input_csv, output_sli) {
  # Step 1: Read the CSV file using fread for efficiency
  cat("Reading data...\n")
  data <- fread(input_csv)  # fread is faster and more memory efficient than read.csv
  cat("Data read successfully!\n")
  
  # Step 2: Separate attributes and spectral data
  attributes <- data[, 1:4]  
  spectral_data <- data[, 5:ncol(data)]
  
  # Step 3: Extract wavelengths from the column headers (spectral bands)
  wavelengths <- colnames(spectral_data)  # Extract column names (assumed to be wavelengths)
  
  # Step 4: Prepare the .sli file content
  cat("Preparing .sli file content...\n")
  
  # Initialize the file content with header information
  sli_content <- "# ENVI Spectral Library\n"
  sli_content <- paste(sli_content, "# Spectral library: Custom Spectral Data\n", sep="")
  
  # Add number of spectra and bands
  num_spectra <- nrow(spectral_data)
  num_bands <- ncol(spectral_data)
  sli_content <- paste(sli_content, paste("# Number of spectra:", num_spectra), "\n", sep="")
  sli_content <- paste(sli_content, paste("# Number of bands:", num_bands), "\n", sep="")
  
  # Add wavelengths section
  sli_content <- paste(sli_content, "# Wavelengths\n", sep="")
  sli_content <- paste(sli_content, paste(wavelengths, collapse = ", "), "\n", sep="")
  
  # Write metadata section (attributes)
  sli_content <- paste(sli_content, "# Metadata\n", sep="")
  for (i in 1:nrow(attributes)) {
    sli_content <- paste(sli_content, paste(attributes[i, ], collapse = ","), "\n", sep="")
  }
  
  # Write spectral data section
  sli_content <- paste(sli_content, "# Spectral Data\n", sep="")
  
  # Instead of concatenating rows one by one, write data in chunks
  writeLines(sli_content, output_sli)  # Write the header and metadata to the file first
  
  cat("Writing spectral data...\n")
  # Write spectral data to file using write.table (faster than repeated paste)
  write.table(spectral_data, file = output_sli, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
  
  cat("Spectral library saved to", output_sli, "\n")
}

# Example usage
create_sli_library("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/masked_subset.csv",
                   "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/canopy_spectra/speclib.sli")

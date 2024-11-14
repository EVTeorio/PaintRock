
length(spruce_imgs)
length(canopies)

library(terra)
library(raster)
library(sf)

# Define file paths
HSI_img = "E:/Hyperspec Images/raw_32619_rd_rf_or"
spruce_imgs <- c(HSI_img)

canopies_path = "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/Labeling"
canopies_sites <- list.files(canopies_path, pattern = "*.shp$", full.names = TRUE)
canopies <- lapply(canopies_sites, terra::vect)

# Process each hyperspectral image
lapply(1:length(spruce_imgs), function(img_idx) {
  
  # Load the hyperspectral image
  tst_img <- terra::rast(spruce_imgs[img_idx])
  tst_names <- names(tst_img)  # Get the band names
  
  # Get the canopy polygons for the current image
  tst_quads <- canopies[[img_idx]]
  
  # Loop through each polygon and extract spectral data
  lapply(1:length(tst_quads), function(i) {
    
    # Get the name of the polygon (assuming it's stored in a field called "name")
    polygon_name <- tst_quads[i]$name  # Change "name" to the correct field name in your shapefile
    
    # If the polygon name is NULL or missing, use a default name
    if (is.null(polygon_name)) {
      polygon_name <- paste0("polygon_", i)
    }
    
    # Crop the raster to the polygon boundary
    tst_crop <- terra::crop(tst_img, tst_quads[i])
    
    # Mask the cropped raster with the polygon
    tst_mask <- terra::mask(tst_crop, tst_quads[i])
    
    # Set the band names
    names(tst_mask) <- tst_names
    
    # Generate the output filename using the polygon name
    output_filename <- paste0("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/canopy_spectra/", polygon_name, ".ENVI")
    
    # Save the masked raster to a file
    writeRaster(tst_mask, output_filename, overwrite = TRUE)
    
    # Clean up memory after processing each polygon
    rm(tst_crop, tst_mask)
    gc()
  })
  
  # Clean up memory after processing the whole image
  rm(tst_img, tst_quads)
  gc()
})


